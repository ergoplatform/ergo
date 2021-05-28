package org.ergoplatform.wallet.boxes

import scorex.util.ModifierId
import org.ergoplatform.ErgoBoxAssets
import org.ergoplatform.ErgoBoxAssetsHolder
import org.ergoplatform.ErgoBox.MaxTokens
import org.ergoplatform.wallet.{AssetUtils, TokensMap}

import scala.annotation.tailrec
import scala.collection.mutable
import org.ergoplatform.wallet.Utils._

/**
  * Default implementation of the box selector. It simply picks boxes till sum of their monetary values
  * meets target Ergo balance, then it checks which assets are not fulfilled and adds boxes till target
  * asset values are met.
  */
object DefaultBoxSelector extends BoxSelector {

  import BoxSelector._

  final case class NotEnoughErgsError(message: String) extends BoxSelectionError

  final case class NotEnoughTokensError(message: String) extends BoxSelectionError

  final case class NotEnoughCoinsForChangeBoxError(message: String) extends BoxSelectionError

  final case class NoSuchTokensError(message: String) extends BoxSelectionError

  override def select[T <: ErgoBoxAssets](inputBoxes: Iterator[T],
                                          externalFilter: T => Boolean,
                                          targetBalance: Long,
                                          targetAssets: TokensMap): Either[BoxSelectionError, BoxSelectionResult[T]] = {
    //mutable structures to collect results
    val res = mutable.Buffer[T]()
    var currentBalance = 0L
    val currentAssets = mutable.Map[ModifierId, Long]()

    def pickUp(unspentBox: T) = {
      currentBalance = currentBalance + unspentBox.value
      AssetUtils.mergeAssetsMut(currentAssets, unspentBox.tokens)
      res += unspentBox
    }

    def balanceMet = currentBalance >= targetBalance

    def assetsMet = targetAssets.forall {
      case (id, targetAmt) => currentAssets.getOrElse(id, 0L) >= targetAmt
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
        formChangeBox(currentBalance, targetBalance, currentAssets, targetAssets).mapRight { changeBox =>
          BoxSelectionResult(res, changeBox)
        }
      } else {
        Left(NotEnoughTokensError(s"not enough boxes to meet token needs $targetAssets (found only $currentAssets)"))
      }
    } else {
      Left(NotEnoughErgsError(s"not enough boxes to meet ERG needs $targetBalance (found only $currentBalance)"))
    }
  }

  def formChangeBox(
                     foundBalance: Long,
                     targetBalance: Long,
                     foundBoxAssets: mutable.Map[ModifierId, Long],
                     targetBoxAssets: TokensMap
                   ): Either[BoxSelectionError, Option[ErgoBoxAssets]] = {
    val changeBalance = foundBalance - targetBalance
    foundBoxAssets.foldLeft[Either[BoxSelectionError, TokensMap]](Right(Map.empty)) { case (acc, (id, amount)) =>
      targetBoxAssets.get(id) match {
        case Some(value) => acc.map(_.updated(id, amount - value))
        case None => Left(NoSuchTokensError(
          s"There are no $id tokens in target"
        ))
      }
    }.flatMap {
      tMap =>
        tMap.filter(tm => tm._2 == 0)
        val uu = foundBoxAssets.map { case (id, amount) => (id, amount - targetBoxAssets(id)) }
        if (changeBalance < 0)
          Left(NotEnoughCoinsForChangeBoxError(s"Not enough ERG $changeBalance to create change box"))
        else if (!uu.forall { case (_, amount) => amount >= 0 })
          Left(NotEnoughTokensError(s"Not enough tokens to create change box"))
        else if (changeBalance == 0 && uu.exists { case (_, am) => am > 0 })
          Left(NotEnoughErgsError("Cannot create change box out of tokens without ERGs"))
        else if (changeBalance == 0 && uu.forall { case (_, amount) => amount == 0 }) Right(None)
        else Right(Some(ErgoBoxAssetsHolder(changeBalance, tMap)))
    }
  }

}
