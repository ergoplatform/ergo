package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.ADProofs

trait ADProofsProcessor {
  def toInsert(m: ADProofs, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)]

  def toDrop(modifier: ADProofs): Seq[ByteArrayWrapper]

}

