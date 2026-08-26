package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.modifiers.{ErgoFullBlock, BlockSection}
import scorex.util.ModifierId

import scala.reflect.ClassTag

/**
  * Interface for most basic and used functions reading objects
  * from history database
  */
trait BasicReaders {
  def bestFullBlockOpt: Option[ErgoFullBlock]

  def headerIdsAtHeight(height: Int): Seq[ModifierId]

  /**
    * @return - best known header known for given height, if available
    */
  def bestHeaderIdAtHeight(height: Int): Option[ModifierId]

  def typedModifierById[T <: BlockSection : ClassTag](id: ModifierId): Option[T]

  def contains(id: ModifierId): Boolean
}
