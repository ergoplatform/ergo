package org.ergoplatform.nodeView.history.components

import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import scorex.util.ModifierId

import scala.reflect.ClassTag

trait BasicReaders {
  def bestFullBlockOpt: Option[ErgoFullBlock]

  def headerIdsAtHeight(height: Int): Seq[ModifierId]

  def typedModifierById[T <: ErgoPersistentModifier : ClassTag](id: ModifierId): Option[T]

  def contains(id: ModifierId): Boolean
}
