package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.ADProofs

trait ADProofsProcessor {
  def indexes(m: ADProofs, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)]

  def idsToDrop(modifier: ADProofs): Seq[ByteArrayWrapper]

}

