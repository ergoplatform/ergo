package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox

case class BoxSelectionResult(boxes: Seq[ErgoBox],
                              changeBalance: Long,
                              changeAssets: Map[ByteArrayWrapper, Long])

/**
  * An interface which is exposing a method to select unspent boxes according to target amounts in Ergo tokens and
  * assets and possible user-defined filter.
  */
trait BoxSelector {
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
             availableAssets: Map[ByteArrayWrapper, Long]): Option[BoxSelectionResult]
}

