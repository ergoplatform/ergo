package org.ergoplatform.modifiers.experimental

import io.circe.Json
import org.ergoplatform.modifiers.transaction.AnyoneCanSpendTransaction
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer

case class BlockTransactions(txs: Seq[AnyoneCanSpendTransaction]) extends NodeViewModifier {


  override val modifierTypeId: ModifierTypeId = BlockTransactions.ModifierTypeId

  override def id: ModifierId = BlockTransactions.rootHash(txs.map(_.id))

  override type M = BlockTransactions

  override def serializer: Serializer[BlockTransactions] = ???

  override def json: Json = ???

}

object BlockTransactions {
  val ModifierTypeId: Byte = 102: Byte

  def rootHash(ids: Seq[Array[Byte]]): Array[Byte] = Algos.merkleTreeRoot(ids)
}