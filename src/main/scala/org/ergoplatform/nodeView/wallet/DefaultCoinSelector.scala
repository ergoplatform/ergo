package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.utils.AssetUtils.mergeAssets

import scala.collection.mutable

class DefaultCoinSelector extends CoinSelector {

  //todo: refactor code below, it is pretty terrible
  override def select(inputBoxes: Iterator[BoxUnspent],
                      filterFn: BoxUnspent => Boolean,
                      targetBalance: Long,
                      availableBalance: Long,
                      targetAssets: Map[ByteArrayWrapper, Long],
                      availableAssets: Map[ByteArrayWrapper, Long]): Option[CoinSelectionResult] = {

    val res = mutable.Buffer[ErgoBox]()
    var currentBalance = 0L
    val currentAssets = mutable.Map[ByteArrayWrapper, Long]()

    def successMet = currentBalance >= targetBalance && targetAssets.forall { case (id, targetAmt) =>
      currentAssets.getOrElse(id, 0L) >= targetAmt
    }

    inputBoxes.find { bc =>
      if(filterFn(bc)) {
        currentBalance = currentBalance + bc.ergoValue
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
          currentBalance = currentBalance + bc.ergoValue
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
      Some(CoinSelectionResult(res, currentBalance - targetBalance, currentAssets.toMap))
    } else {
      None
    }
  }
}
