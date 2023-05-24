package org.ergoplatform.wallet.boxes

import org.ergoplatform.sdk.wallet.{AssetUtils, TokensMap}
import org.ergoplatform.wallet.Constants.MaxAssetsPerBox
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionError
import org.ergoplatform.wallet.transactions.TransactionBuilder._
import org.ergoplatform.{ErgoBoxAssets, ErgoBoxAssetsHolder}
import scorex.util.ModifierId

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Default implementation of the box selector. It simply picks boxes till sum of their monetary values
  * meets target Ergo balance, then it checks which assets are not fulfilled and adds boxes till target
  * asset values are met.
  *
  * @param reemissionDataOpt - reemission parameters, if wallet is checking re-emission rules
  */
class DefaultBoxSelector(override val reemissionDataOpt: Option[ReemissionData]) extends BoxSelector {

  import BoxSelector._
  import DefaultBoxSelector._

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
        formChangeBoxes(currentBalance, targetBalance, currentAssets, targetAssets).mapRight { changeBoxes =>
          selectionResultWithEip27Output(res.toSeq, changeBoxes)
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
    * @return
    */
  def formChangeBoxes(foundBalance: Long,
                      targetBalance: Long,
                      foundBoxAssets: mutable.Map[ModifierId, Long],
                      targetBoxAssets: TokensMap): Either[BoxSelectionError, Seq[ErgoBoxAssets]] = {
    AssetUtils.subtractAssetsMut(foundBoxAssets, targetBoxAssets)
    val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] = foundBoxAssets.grouped(MaxAssetsPerBox).toIndexedSeq
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
      Right(changeBoxes)
    }
  }

}

object DefaultBoxSelector {

  final case class NotEnoughErgsError(message: String, balanceFound: Long) extends BoxSelectionError

  final case class NotEnoughTokensError(message: String, tokensFound: Map[ModifierId, Long]) extends BoxSelectionError

  final case class NotEnoughCoinsForChangeBoxesError(message: String) extends BoxSelectionError

}
