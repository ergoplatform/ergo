package org.ergoplatform.wallet.boxes

import scorex.util.ModifierId
import org.ergoplatform.ErgoBoxAssets
import org.ergoplatform.ErgoBox.MaxTokens
import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Default implementation of the box selector. It simply picks boxes till sum of their monetary values
  * meets target Ergo balance, then it checks which assets are not fulfilled and adds boxes till target
  * asset values are met.
  */
object DefaultBoxSelector extends BoxSelector {

  import BoxSelector._

  override def select[T <: ErgoBoxAssets](inputBoxes: Iterator[T],
                      externalFilter: T => Boolean,
                      targetBalance: Long,
                      targetAssets: Map[ModifierId, Long]): Option[BoxSelectionResult[T]] = {
    //mutable structures to collect results
    val res            = mutable.Buffer[T]()
    var currentBalance = 0L
    val currentAssets  = mutable.Map[ModifierId, Long]()

    def pickUp(unspentBox: T) = {
      currentBalance = currentBalance + unspentBox.value
      mergeAssetsMut(currentAssets, unspentBox.tokens)
      res += unspentBox
    }

    def balanceMet = currentBalance >= targetBalance
    def assetsMet = targetAssets.forall {
      case (id, targetAmt) => currentAssets.getOrElse(id, 0L) >= targetAmt
    }

    @tailrec
    def pickBoxes(
      boxesIterator: Iterator[T],
      filterFn: T => Boolean,
      successFn: => Boolean
    ): Boolean =
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
                  val targetAmt       = targetAssets.getOrElse(id, 0L)
                  lazy val currentAmt = currentAssets.getOrElse(id, 0L)
                  targetAmt > 0 && targetAmt > currentAmt
              },
            assetsMet
          )) {
        subtractAssetsMut(currentAssets, targetAssets)
        val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] =
          currentAssets.grouped(MaxTokens).toSeq
        val changeBalance = currentBalance - targetBalance
        formChangeBoxes(changeBalance, changeBoxesAssets).map(changeBoxes =>
          BoxSelectionResult(res, changeBoxes)
        )
      } else {
        None
      }
    } else {
      None
    }
  }

}
