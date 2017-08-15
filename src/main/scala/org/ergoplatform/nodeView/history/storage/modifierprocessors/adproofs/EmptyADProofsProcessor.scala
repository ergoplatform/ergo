package org.ergoplatform.nodeView.history.storage.modifierprocessors.adproofs

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.ADProof
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

/**
  * ADProof processor for regimes, that do not keep ADProofs
  */
trait EmptyADProofsProcessor extends ADProofsProcessor {
  protected val adState: Boolean = false

  override protected def process(m: ADProof): ProgressInfo[ErgoPersistentModifier] = ProgressInfo(None, Seq(), Seq())

  override protected def toDrop(modifier: ADProof): Seq[ByteArrayWrapper] = Seq()

  override protected def validate(m: ADProof): Try[Unit] = Failure(new Error("Regime that do not process ADProofs"))
}
