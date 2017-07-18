package org.ergoplatform.nodeView.history.storage
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.ADProofs

trait EmptyADProofsProcessor extends ADProofsProcessor {
  override def indexes(m: ADProofs, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = ???

  override def idsToDrop(modifier: ADProofs): Seq[ByteArrayWrapper] = ???
}
