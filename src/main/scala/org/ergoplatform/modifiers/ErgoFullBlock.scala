package org.ergoplatform.modifiers

import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId, TransactionsCarryingPersistentNodeViewModifier}

case class ErgoFullBlock(header: Header,
                         blockTransactions: BlockTransactions,
                         adProofs: Option[ADProofs])
  extends ErgoPersistentModifier
    with TransactionsCarryingPersistentNodeViewModifier[ErgoTransaction] {

  lazy val toSeq: Seq[ErgoPersistentModifier] = Seq(header, blockTransactions) ++ adProofs.toSeq

  override val modifierTypeId: ModifierTypeId = ErgoFullBlock.modifierTypeId

  override val parentId: ModifierId = header.parentId

  override lazy val id: ModifierId = header.id

  override type M = ErgoFullBlock

  override lazy val serializer: Serializer[ErgoFullBlock] =
    throw new Error("Should never try to serialize ErgoFullBlock")

  override lazy val transactions: Seq[ErgoTransaction] = blockTransactions.txs

}

object ErgoFullBlock {

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (-127: Byte)

}

class ErgoFullBlockEncoder(implicit headerEncoder: Encoder[Header],
                           transactionEncoder: Encoder[BlockTransactions],
                           proofsEncoder: Encoder[ADProofs]) extends Encoder[ErgoFullBlock] {

  def apply(b: ErgoFullBlock): Json = {
    Json.obj(
      "header" -> b.header.asJson,
      "blockTransactions" -> b.blockTransactions.asJson,
      "adProofs" -> b.adProofs.map(_.asJson).getOrElse(Map.empty[String, String].asJson)
    )
  }

}
