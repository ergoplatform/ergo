package org.ergoplatform.wallet.boxes

import org.ergoplatform.contracts.ReemissionContracts
import scorex.util.ModifierId
import org.ergoplatform.{ErgoBoxAssets, ErgoBoxAssetsHolder, ErgoBoxCandidate}
import org.ergoplatform.wallet.Constants.MaxAssetsPerBox
import org.ergoplatform.wallet.{AssetUtils, TokensMap}
import scala.annotation.tailrec
import scala.collection.mutable
import org.ergoplatform.wallet.transactions.TransactionBuilder._
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionError

/**
  * Default implementation of the box selector. It simply picks boxes till sum of their monetary values
  * meets target Ergo balance, then it checks which assets are not fulfilled and adds boxes till target
  * asset values are met.
  *
  * @param reemissionDataOpt - reemission parameters, if wallet is checking re-emission rules
  */
class DefaultBoxSelector(override val reemissionDataOpt: Option[ReemissionData]) extends BoxSelector {

  import DefaultBoxSelector._
  import BoxSelector._
  import scorex.util.idToBytes

  // helper function which returns count of assets in `initialMap` not fully spent in `subtractor`
  private def diffCount(initialMap: mutable.Map[ModifierId, Long], subtractor: TokensMap): Int = {
    initialMap.foldLeft(0){case (cnt, (tokenId, tokenAmt)) =>
      if (tokenAmt - subtractor.getOrElse(tokenId, 0L) > 0) {
        cnt + 1
      } else {
        cnt
      }
    }
  }

  override def select[T <: ErgoBoxAssets](inputBoxes: Iterator[T],
                                          externalFilter: T => Boolean,
                                          targetBalance: Long,
                                          targetAssets: TokensMap): Either[BoxSelectionError, BoxSelectionResult[T]] = {
    //mutable structures to collect results
    val res = mutable.Buffer[T]()
    var currentBalance = 0L
    val currentAssets = mutable.Map[ModifierId, Long]()

    def pickUp(unspentBox: T) = {
      currentBalance = currentBalance + valueOf(unspentBox, reemissionDataOpt)
      AssetUtils.mergeAssetsMut(currentAssets, unspentBox.tokens)
      res += unspentBox
    }

    /**
      * Helper functions which checks whether enough ERGs collected
      */
    def balanceMet: Boolean = {
      val diff = currentBalance - targetBalance

      // We estimate how many ERG needed for assets in change boxes
      val assetsDiff = diffCount(currentAssets, targetAssets)
      val diffThreshold = if (assetsDiff <= 0) {
        0
      } else {
        MinBoxValue * (assetsDiff / MaxAssetsPerBox + 1)
      }

      diff >= diffThreshold
    }

    /**
      * Helper functions which checks whether enough assets collected
      */
    def assetsMet: Boolean = {
      targetAssets.forall {
        case (id, targetAmt) => currentAssets.getOrElse(id, 0L) >= targetAmt
      }
    }

    @tailrec
    def pickBoxes(boxesIterator: Iterator[T], filterFn: T => Boolean, successFn: => Boolean): Boolean =
      if (successFn) {
        true
      } else if (!boxesIterator.hasNext) {
        false
      } else {
        val box = boxesIterator.next()
        if (filterFn(box)) pickUp(box)
        pickBoxes(boxesIterator, filterFn, successFn)
      }

    //first, we pick all the boxes until ergo target balance is met
    if (pickBoxes(inputBoxes, externalFilter, balanceMet)) {
      //then we pick boxes until all the target asset amounts are met (we pick only boxes containing needed assets).
      //If this condition is satisfied on the previous step, we will do one extra call to pickBoxes
      //with no touching the iterator (which is not that much).
      if (pickBoxes(
        inputBoxes,
        bc =>
          externalFilter(bc) && bc.tokens.exists {
            case (id, _) =>
              val targetAmt = targetAssets.getOrElse(id, 0L)
              lazy val currentAmt = currentAssets.getOrElse(id, 0L)
              targetAmt > 0 && targetAmt > currentAmt
          },
        assetsMet
      )) {
        val ra = reemissionAmount(res)
        formChangeBoxes(currentBalance, targetBalance, currentAssets, targetAssets, ra).mapRight { changeBoxes =>
          BoxSelectionResult(res, changeBoxes)
        }
      } else {
        Left(NotEnoughTokensError(
          s"not enough boxes to meet token needs $targetAssets (found only $currentAssets)", currentAssets.toMap)
        )
      }
    } else {
      Left(NotEnoughErgsError(
        s"not enough boxes to meet ERG needs $targetBalance (found only $currentBalance)", currentBalance)
      )
    }
  }

  /**
    * Helper method to construct change outputs
    *
    * @param foundBalance - ERG balance of boxes collected
    *                       (spendable only, so after possibly deducting re-emission tokens)
    * @param targetBalance - ERG amount to be transferred to recipients
    * @param foundBoxAssets - assets balances of boxes
    * @param targetBoxAssets - assets amounts to be transferred to recipients
    * @param reemissionAmt - amount of re-emission tokens in collected boxes
    * @return
    */
  def formChangeBoxes(foundBalance: Long,
                      targetBalance: Long,
                      foundBoxAssets: mutable.Map[ModifierId, Long],
                      targetBoxAssets: TokensMap,
                      reemissionAmt: Long): Either[BoxSelectionError, Seq[ErgoBoxAssets]] = {
    AssetUtils.subtractAssetsMut(foundBoxAssets, targetBoxAssets)
    val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] = foundBoxAssets.grouped(MaxAssetsPerBox).toSeq
    val changeBalance = foundBalance - targetBalance
    //at least a minimum amount of ERG should be assigned per a created box
    if (changeBoxesAssets.size * MinBoxValue > changeBalance) {
      Left(NotEnoughCoinsForChangeBoxesError(
        s"Not enough nanoERGs ($changeBalance nanoERG) to create ${changeBoxesAssets.size} change boxes, \nfor $changeBoxesAssets"
      ))
    } else {
      val changeBoxes = if (changeBoxesAssets.nonEmpty) {
        val baseChangeBalance = changeBalance / changeBoxesAssets.size

        val changeBoxesNoBalanceAdjusted = changeBoxesAssets.map { a =>
          ErgoBoxAssetsHolder(baseChangeBalance, a.toMap)
        }

        val modifiedBoxOpt = changeBoxesNoBalanceAdjusted.headOption.map { firstBox =>
          ErgoBoxAssetsHolder(
            changeBalance - baseChangeBalance * (changeBoxesAssets.size - 1),
            firstBox.tokens
          )
        }

        modifiedBoxOpt.toSeq ++ changeBoxesNoBalanceAdjusted.tail
      } else if (changeBalance > 0) {
        Seq(ErgoBoxAssetsHolder(changeBalance))
      } else {
        Seq.empty
      }

      if (reemissionAmt > 0) {
        reemissionDataOpt match {
          case Some(reemissionData) =>
            // we construct this instance to get pay-to-reemission contract from it
            // we set re-emission contract NFT id, re-emission start height is not used so we set it to 0
            val rc: ReemissionContracts = new ReemissionContracts {
              override val reemissionNftIdBytes: Array[Byte] = idToBytes(reemissionData.reemissionNftId)
              override val reemissionStartHeight: Int = 0
            }
            val p2r = rc.payToReemission
            val payToReemissionBox = new ErgoBoxCandidate(reemissionAmt, p2r, creationHeight = 0)
            Right(payToReemissionBox +: changeBoxes)
          case None =>
            log.error("reemissionData when reemissionAmt > 0, should not happen at all")
            Right(changeBoxes)
        }
      } else {
        Right(changeBoxes)
      }
    }
  }

}

object DefaultBoxSelector {

  final case class NotEnoughErgsError(message: String, balanceFound: Long) extends BoxSelectionError

  final case class NotEnoughTokensError(message: String, tokensFound: Map[ModifierId, Long]) extends BoxSelectionError

  final case class NotEnoughCoinsForChangeBoxesError(message: String) extends BoxSelectionError

}
