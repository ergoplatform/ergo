package org.ergoplatform.modifiers.mempool

import io.circe.Json
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.settings.Algos
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.crypto.hash.Digest32

case class TransactionIdsForHeader(ids: Seq[ModifierId]) extends MempoolModifier {
  override val modifierTypeId: ModifierTypeId = TransactionIdsForHeader.modifierTypeId

  override lazy val id: ModifierId = ModifierId @@ Algos.hash(scorex.core.utils.concatFixLengthBytes(ids))

  override type M = TransactionIdsForHeader

  override lazy val serializer: Serializer[TransactionIdsForHeader] = ???

  override lazy val json: Json = ???

  lazy val rootHash: Digest32 = BlockTransactions.rootHash(ids)
}

object TransactionIdsForHeader {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (103: Byte)

  def validate(txIds: TransactionIdsForHeader, header: Header): Boolean = {
    header.transactionsRoot sameElements txIds.rootHash
  }
}