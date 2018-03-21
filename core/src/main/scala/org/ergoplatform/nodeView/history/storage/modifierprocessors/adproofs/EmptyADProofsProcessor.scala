package org.ergoplatform.nodeView.history.storage.modifierprocessors.adproofs

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.ADProofs
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

/**
  * ADProof processor for regimes, that do not keep ADProofs
  */
trait EmptyADProofsProcessor extends ADProofsProcessor {
  protected val adState: Boolean = false

  override protected def process(m: ADProofs): ProgressInfo[ErgoPersistentModifier] =
    ProgressInfo(None, Seq.empty, None, Seq.empty)

  override protected def validate(m: ADProofs): Try[Unit] = Failure(new Error("Regime that do not process ADProofs"))
}
