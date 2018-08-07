package org.ergoplatform.nodeView.wallet

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoBox

case class BoxSelectionResult(boxes: Seq[ErgoBox],
                              changeBoxes: Seq[(Long, Map[ByteArrayWrapper, Long])])

/**
  * An interface which is exposing a method to select unspent boxes according to target amounts in Ergo tokens and
  * assets and possible user-defined filter. The interface could have many instantiations based
  */
trait BoxSelector {

  /**
    * A method which is selecting boxes to spend in order to collect needed amounts of ergo tokens and assets.
    * @param inputBoxes - unspent boxes to choose from.
    * @param filterFn - user-provided filter function for boxes. From inputBoxes, only ones to be chosen for which
    *                 filterFn(box) returns true
    * @param targetBalance - ergo balance to be met
    * @param targetAssets - assets balances to be met
    * @return None if select() is failing to pick appropriate boxes, othrwise Some(res), where res contains boxes
    *         to spend,
    */
  def select(inputBoxes: Iterator[UnspentBox],
             filterFn: UnspentBox => Boolean,
             targetBalance: Long,
             targetAssets: Map[ByteArrayWrapper, Long]): Option[BoxSelectionResult]
}