package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.HistoryModifier

trait ModifiersProcessor {

  def indexes(m: HistoryModifier, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)]

  def idsToDrop(modifierId: Array[Byte]): Seq[ByteArrayWrapper]

}
