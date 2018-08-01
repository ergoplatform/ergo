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
      if(filterFn(bc)) {
        currentBalance = currentBalance + bc.value
        mergeAssets(currentAssets, bc.assets)
        res += bc.box
      }
      currentBalance >= targetBalance
    }

    inputBoxes.find { bc =>
      if(filterFn(bc)) {
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

    //todo: it could be the case that too many currentAssets to be in 1 Box and we need to add more ergo tokens

    if (successMet) {
      targetAssets.foreach { case (id, targetAmt) =>
        val currentAmt = currentAssets(id)
        if (currentAmt == targetAmt) {
          currentAssets.remove(id)
        } else {
          currentAssets.put(id, currentAmt - targetAmt)
        }
      }
      val change = Seq((currentBalance - targetBalance) ->  currentAssets.toMap)
      Some(BoxSelectionResult(res, change))
    } else {
      None
    }
  }
}
