package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox

import scala.collection.mutable

case class CoinSelectionResult(boxes: Seq[ErgoBox],
                               changeBalance: Long,
                               changeAssets: Map[ByteArrayWrapper, Long])

trait CoinSelector {
  /**
    *
    * @param inputBoxes sorted chronologically
    * @param targetBalance
    * @param targetAssets
    */
  def select(inputBoxes: Iterator[BoxCertain],
             targetBalance: Long,
             availableBalance: Long,
             targetAssets: Map[ByteArrayWrapper, Long],
             availableAssets: Long): Option[CoinSelectionResult]
}

class DefaultCoinSelector extends CoinSelector {
  //todo: move to utils, reuse in ErgoTransaction / ErgoWalletActor
  @inline
  def mergeAssets(into: mutable.Map[ByteArrayWrapper, Long],
                  from: Map[ByteArrayWrapper, Long]): Unit = {
    from.foreach { case (id, amount) =>
      into.put(id, into.getOrElse(id, 0L) + amount)
    }
  }

  //todo: refactor code below, it is pretty terrible
  override def select(inputBoxes: Iterator[BoxCertain],
                      targetBalance: Long,
                      availableBalance: Long,
                      targetAssets: Map[ByteArrayWrapper, Long],
                      availableAssets: Long): Option[CoinSelectionResult] = {

    val res = mutable.Buffer[ErgoBox]()
    var currentBalance = 0L
    val currentAssets = mutable.Map[ByteArrayWrapper, Long]()

    def successMet = currentBalance >= targetBalance && targetAssets.forall{case (id, targetAmt) =>
      currentAssets.getOrElse(id, 0L) >= targetAmt
    }

    inputBoxes.dropWhile { bc =>
      currentBalance = currentBalance + bc.ergoValue
      mergeAssets(currentAssets, bc.assets)
      res += bc.box
      currentBalance < targetBalance
    }.dropWhile { bc =>
      if(bc.assets.exists{ case(id, _) =>
        val targetAmt = targetAssets.getOrElse(id, 0L)
        lazy val currentAmt = currentAssets.getOrElse(id, 0L)
        targetAmt > 0 && targetAmt > currentAmt
      }){
        currentBalance = currentBalance + bc.ergoValue
        mergeAssets(currentAssets, bc.assets)
        res += bc.box
      }
      !successMet
    }

    //todo: it could be the case that too many currentAssets to be in 1 Box and we need to add more ergo tokens

    if(successMet){
      targetAssets.foreach {case (id, targetAmt) =>
        val currentAmt = currentAssets(id)
        if(currentAmt == targetAmt) {
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