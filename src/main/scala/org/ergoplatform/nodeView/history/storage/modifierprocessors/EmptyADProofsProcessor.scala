package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.ADProofs
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

/**
  * ADProof processor for regimes, that do not keep ADProofs
  */
trait EmptyADProofsProcessor extends ADProofsProcessor {
  override def process(m: ADProofs): ProgressInfo[ErgoPersistentModifier] = ProgressInfo(None, Seq(), Seq())

  override def toDrop(modifier: ADProofs): Seq[ByteArrayWrapper] = Seq()

  override def validate(m: ADProofs): Try[Unit] = Failure(new Error("Regime that do not process ADProofs"))
}
