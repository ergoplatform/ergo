package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox
import org.ergoplatform.utils.AssetUtils.mergeAssets
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
  def select(inputBoxes: Iterator[UnspentBox],
             filterFn: UnspentBox => Boolean,
             targetBalance: Long,
             availableBalance: Long,
             targetAssets: Map[ByteArrayWrapper, Long],
             availableAssets: Map[ByteArrayWrapper, Long]): Option[CoinSelectionResult]
}

