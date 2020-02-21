package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBox
import scorex.util.ModifierId

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Default implementation of the box selector. It simply picks boxes till sum of their monetary values
  * meets target Ergo balance, then it checks which assets are not fulfilled and adds boxes till target
  * asset values are met.
  */
object DefaultBoxSelector extends BoxSelector {

  import BoxSelector._

  override def select(inputBoxes: Iterator[TrackedBox],
                      externalFilter: TrackedBox => Boolean,
                      targetBalance: Long,
                      targetAssets: Map[ModifierId, Long]): Option[BoxSelectionResult] = {
    //mutable structures to collect results
    val res = mutable.Buffer[ErgoBox]()
    var currentBalance = 0L
    val currentAssets = mutable.Map[ModifierId, Long]()

    def pickUp(unspentBox: TrackedBox) = {
      currentBalance = currentBalance + unspentBox.value
      mergeAssetsMut(currentAssets, unspentBox.assets)
      res += unspentBox.box
    }

    def balanceMet = currentBalance >= targetBalance
    def assetsMet = targetAssets.forall { case (id, targetAmt) => currentAssets.getOrElse(id, 0L) >= targetAmt }

    @tailrec
    def pickBoxes(boxesIterator: Iterator[TrackedBox],
                  filterFn: TrackedBox => Boolean,
                  successFn: => Boolean): Boolean =
      if (successFn) { true }
      else if (!boxesIterator.hasNext) { false }
      else {
        val box = boxesIterator.next()
        if (filterFn(box)) pickUp(box)
        pickBoxes(boxesIterator, filterFn, successFn)
      }

    //first, we pick all the boxes until ergo target balance is met
    if (pickBoxes(inputBoxes, externalFilter, balanceMet)) {
      //then we pick boxes until all the target asset amounts are met (we pick only boxes containing needed assets).
      //If this condition is satisfied on the previous step, we will do one extra call to pickBoxes
      //with no touching the iterator (which is not that much).
      if (pickBoxes(inputBoxes, bc => externalFilter(bc) && bc.assets.exists { case (id, _) =>
        val targetAmt = targetAssets.getOrElse(id, 0L)
        lazy val currentAmt = currentAssets.getOrElse(id, 0L)
        targetAmt > 0 && targetAmt > currentAmt
      }, assetsMet)) {
        subtractAssetsMut(currentAssets, targetAssets)
        val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] = currentAssets.grouped(ErgoBox.MaxTokens).toSeq
        val changeBalance = currentBalance - targetBalance
        formChangeBoxes(changeBalance, changeBoxesAssets).map(changeBoxes => BoxSelectionResult(res, changeBoxes))
      } else {
        None
      }
    } else {
      None
    }
  }

}
