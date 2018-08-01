package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.utils.AssetUtils.mergeAssets

import scala.collection.mutable

/**
  * Default implementation of the box selector
  */
class DefaultBoxSelector extends BoxSelector {

  //todo: refactor code below, it is pretty terrible
  override def select(inputBoxes: Iterator[UnspentBox],
                      filterFn: UnspentBox => Boolean,
                      targetBalance: Long,
                      targetAssets: Map[ByteArrayWrapper, Long]): Option[BoxSelectionResult] = {

    val res = mutable.Buffer[ErgoBox]()
    var currentBalance = 0L
    val currentAssets = mutable.Map[ByteArrayWrapper, Long]()

    def successMet = currentBalance >= targetBalance && targetAssets.forall { case (id, targetAmt) =>
      currentAssets.getOrElse(id, 0L) >= targetAmt
    }

    //first, we pick all the boxes until ergo target balance is met
    inputBoxes.find { bc =>
      if (filterFn(bc)) {
        currentBalance = currentBalance + bc.value
        mergeAssets(currentAssets, bc.assets)
        res += bc.box
      }
      currentBalance >= targetBalance
    }

    //then we pick boxes until all the target asset amounts are met (we pick only boxes containing needed assets).
    //If this condition is satisfied on the previous step, we will do one extra check (which is not that much).
    inputBoxes.find { bc =>
      if (filterFn(bc)) {
        if (bc.assets.exists { case (id, _) =>
          val targetAmt = targetAssets.getOrElse(id, 0L)
          lazy val currentAmt = currentAssets.getOrElse(id, 0L)
          targetAmt > 0 && targetAmt > currentAmt
        }) {
          currentBalance = currentBalance + bc.value
          mergeAssets(currentAssets, bc.assets)
          res += bc.box
        }
      }
      successMet
    }

    if (successMet) {
      targetAssets.foreach { case (id, targetAmt) =>
        val currentAmt = currentAssets(id)
        if (currentAmt == targetAmt) {
          currentAssets.remove(id)
        } else {
          currentAssets.put(id, currentAmt - targetAmt)
        }
      }

      val changeBoxesAssets = currentAssets.grouped(ErgoBox.MaxTokens).toSeq
      val changeBalance = currentBalance - targetBalance

      //at least 1 ergo token should be assigned per a created box
      if (changeBoxesAssets.size > changeBalance) {
        None
      } else {
        val baseChangeBalance = changeBalance / changeBoxesAssets.size

        val changeBoxesNoBalanceAdjusted = changeBoxesAssets.map{ a =>
          baseChangeBalance -> a.toMap
        }

        val firstBox = changeBoxesNoBalanceAdjusted.head
        val modifiedBox = (changeBalance - baseChangeBalance * (changeBoxesAssets.size - 1)) -> firstBox._2

        val changeBoxes = modifiedBox +: changeBoxesNoBalanceAdjusted.tail

        Some(BoxSelectionResult(res, changeBoxes))
      }
    } else {
      None
    }
  }
}
