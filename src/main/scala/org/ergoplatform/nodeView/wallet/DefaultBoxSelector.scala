package org.ergoplatform.nodeView.wallet

import org.ergoplatform.ErgoBox
import org.ergoplatform.utils.AssetUtils.{mergeAssets, subtractAssets}
import scorex.core.ModifierId

import scala.collection.mutable

/**
  * Default implementation of the box selector. It simply picks boxes till sum of their monetary values
  * meets target Ergo balance, then it checks which assets are not fulfilled and adds boxes till target
  * asset values are met.
  */
object DefaultBoxSelector extends BoxSelector {

  private def formChangeBoxes(changeBalance: Long,
                              changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]]): Option[Seq[(Long, Map[ModifierId, Long])]] = {
    //at least 1 ergo token should be assigned per a created box
    if (changeBoxesAssets.size > changeBalance) {
      None
    } else {
      val changeBoxes = if (changeBoxesAssets.nonEmpty) {
        val baseChangeBalance = changeBalance / changeBoxesAssets.size

        val changeBoxesNoBalanceAdjusted = changeBoxesAssets.map { a =>
          baseChangeBalance -> a.toMap
        }

        val modifiedBoxOpt = changeBoxesNoBalanceAdjusted.headOption.map { firstBox =>
          (changeBalance - baseChangeBalance * (changeBoxesAssets.size - 1)) -> firstBox._2
        }

        modifiedBoxOpt.toSeq ++ changeBoxesNoBalanceAdjusted.tail
      } else if (changeBalance > 0) {
        Seq(changeBalance -> Map.empty[ModifierId, Long])
      } else {
        Seq.empty
      }
      Some(changeBoxes)
    }
  }

  override def select(inputBoxes: Iterator[TrackedBox],
                      filterFn: TrackedBox => Boolean,
                      targetBalance: Long,
                      targetAssets: Map[ModifierId, Long]): Option[BoxSelectionResult] = {

    //mutable structures to collect results
    val res = mutable.Buffer[ErgoBox]()
    var currentBalance = 0L
    val currentAssets = mutable.Map[ModifierId, Long]()

    def collect(unspentBox: TrackedBox) = {
      currentBalance = currentBalance + unspentBox.value
      mergeAssets(currentAssets, unspentBox.assets)
      res += unspentBox.box
    }

    def successMet = currentBalance >= targetBalance && targetAssets.forall { case (id, targetAmt) =>
      currentAssets.getOrElse(id, 0L) >= targetAmt
    }

    //first, we pick all the boxes until ergo target balance is met
    inputBoxes.find { bc =>
      if (filterFn(bc)) collect(bc)
      currentBalance >= targetBalance
    }
    if (!successMet) {
      //then we pick boxes until all the target asset amounts are met (we pick only boxes containing needed assets).
      //If this condition is satisfied on the previous step, we will do one extra check (which is not that much).
      inputBoxes.find { bc =>
        if (filterFn(bc) && {
          bc.assets.exists { case (id, _) =>
            val targetAmt = targetAssets.getOrElse(id, 0L)
            lazy val currentAmt = currentAssets.getOrElse(id, 0L)
            targetAmt > 0 && targetAmt > currentAmt
          }
        }) {
          collect(bc)
        }
        successMet
      }
    }

    if (successMet) {
      subtractAssets(currentAssets, targetAssets)
      val changeBoxesAssets: Seq[mutable.Map[ModifierId, Long]] = currentAssets.grouped(ErgoBox.MaxTokens).toSeq
      val changeBalance = currentBalance - targetBalance
      formChangeBoxes(changeBalance, changeBoxesAssets).map(changeBoxes => BoxSelectionResult(res, changeBoxes))
    } else {
      None
    }
  }
}
