package org.ergoplatform.nodeView.history.components

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import scorex.util.ModifierId

import scala.reflect.ClassTag

trait BasicReaders {

  def bestHeaderIdOpt: Option[ModifierId]

  def bestFullBlockOpt: Option[ErgoFullBlock]

  def headerIdsAtHeight(height: Int): Seq[ModifierId]

  def typedModifierById[T <: ErgoPersistentModifier : ClassTag](id: ModifierId): Option[T]

  def contains(id: ModifierId): Boolean

  /**
    * Header of best Header chain. Empty if no genesis block is applied yet (from a chain or a PoPoW proof).
    * Transactions and ADProofs for this Header may be missed, to get block from best full chain (in mode that support
    * it) call bestFullBlockOpt.
    */
  def bestHeaderOpt: Option[Header] = bestHeaderIdOpt.flatMap(typedModifierById[Header])

}
