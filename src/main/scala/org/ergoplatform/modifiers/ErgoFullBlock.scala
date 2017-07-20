package org.ergoplatform.modifiers

import io.circe.Json
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

//TODO we need it to be ErgoPersistentModifier just to put it to ProcessInfo
case class ErgoFullBlock(header: Header, blockTransactions: BlockTransactions, aDProofs: ADProofs) extends ErgoPersistentModifier {
  override val modifierTypeId: ModifierTypeId = ErgoFullBlock.modifierTypeId

  override def id: ModifierId = ???

  override def json: Json = ???

  override type M = ErgoFullBlock

  override def serializer: Serializer[ErgoFullBlock] = ???
}

object ErgoFullBlock {
  val modifierTypeId: ModifierTypeId = (-127).toByte
}