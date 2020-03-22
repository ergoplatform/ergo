package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBox
import org.ergoplatform.wallet.boxes.BoxSelector.{BoxSelectionResult}//, subtractAssetsMut}
import scorex.util.ModifierId

import scala.collection.mutable

/**
  * An interface which is exposing a method to select unspent boxes according to target amounts in Ergo tokens and
  * assets and possible user-defined filter. The interface could have many instantiations implementing
  * different strategies.
  */
trait BoxSelector {

  /**
    * A method which is selecting boxes to spend in order to collect needed amounts of ergo tokens and assets.
    *
    * @param inputBoxes    - unspent boxes to choose from.
    * @param filterFn      - user-provided filter function for boxes. From inputBoxes, only ones to be chosen for which
    *                      filterFn(box) returns true
    * @param targetBalance - ergo balance to be met
    * @param targetAssets  - assets balances to be met
    * @return None if select() is failing to pick appropriate boxes, otherwise Some(res), where res contains boxes
    *         to spend as well as monetary values and assets for boxes containing change
    *         (wrapped in a special BoxSelectionResult class).
    */
  def select(inputBoxes: Iterator[TrackedBox],
             filterFn: TrackedBox => Boolean,
             targetBalance: Long,
             targetAssets: Map[ModifierId, Long]): Option[BoxSelectionResult]

  protected def calcChange(boxes: Seq[ErgoBox],
                 targetBalance: Long,
                 targetAssets: Map[ModifierId, Long]): Option[Seq[Box]] = 
    BoxSelectors.calcChange(boxes.map(b => GenericBox(b)), targetBalance, targetAssets)
  
}

object BoxSelector {

  final case class BoxSelectionResult(boxes: Seq[ErgoBox],
                                      changeBoxes: Seq[Box])

}
