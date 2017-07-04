package org.ergoplatform.modifiers.experimental

import io.circe.Json
import org.ergoplatform.settings.Constants
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

case class TransactionIdsForHeader(ids: Seq[ModifierId]) extends NodeViewModifier {
  override val modifierTypeId: ModifierTypeId = TransactionIdsForHeader.ModifierTypeId

  override def id: ModifierId = Constants.hash(scorex.core.utils.concatFixLengthBytes(ids))

  override type M = TransactionIdsForHeader

  override def serializer: Serializer[TransactionIdsForHeader] = ???

  override def json: Json = ???

  lazy val rootHash = BlockTransactions.rootHash(ids)
}

object TransactionIdsForHeader {
  val ModifierTypeId: Byte = 103: Byte

  def validate(header: Header, txIds: TransactionIdsForHeader): Boolean = {
    header.transactionsRoot sameElements txIds.rootHash
  }
}
