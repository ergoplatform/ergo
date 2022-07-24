package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.{ErgoFullBlock, BlockSection}
import scorex.util.ModifierId

import scala.reflect.ClassTag

trait BasicReaders {
  def bestFullBlockOpt: Option[ErgoFullBlock]

  def headerIdsAtHeight(height: Int): Seq[ModifierId]

  def typedModifierById[T <: BlockSection : ClassTag](id: ModifierId): Option[T]

  def contains(id: ModifierId): Boolean
}
