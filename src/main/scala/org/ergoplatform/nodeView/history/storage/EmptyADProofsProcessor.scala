package org.ergoplatform.nodeView.history.storage
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.ADProofs

/**
  * ADProof processor for regimes, that do not keep ADProofs
  */
trait EmptyADProofsProcessor extends ADProofsProcessor {
  override def toInsert(m: ADProofs, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = Seq()

  override def toDrop(modifier: ADProofs): Seq[ByteArrayWrapper] = Seq()
}
