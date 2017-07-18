package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.HistoryModifier

trait ModifiersProcessor[M <: HistoryModifier] {

  def indexes(m: M, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)]

  def idsToDrop(modifier: M): Seq[ByteArrayWrapper]

}
