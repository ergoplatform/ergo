package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.nodeView.history.storage.HistoryStorage

import scala.util.{Failure, Try}

/**
  * ADProof processor for regimes, that do not keep ADProofs
  */
trait EmptyADProofsProcessor extends ADProofsProcessor {
  override def toInsert(m: ADProofs, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = Seq()

  override def toDrop(modifier: ADProofs): Seq[ByteArrayWrapper] = Seq()

  override def validate(m: ADProofs): Try[Unit] = Failure(new Error("Regime that do not process ADProofs"))
}
