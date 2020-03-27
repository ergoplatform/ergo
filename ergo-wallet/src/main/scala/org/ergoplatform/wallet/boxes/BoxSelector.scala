package org.ergoplatform.wallet.boxes

import org.ergoplatform.ErgoBoxAssets
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import scorex.util.ModifierId


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
  
}

object BoxSelector {

  final case class BoxSelectionResult(trackedBoxes: Seq[TrackedBox], changeBoxes: Seq[ErgoBoxAssets])

}
