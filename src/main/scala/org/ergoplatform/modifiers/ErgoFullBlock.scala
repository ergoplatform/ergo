package org.ergoplatform.modifiers

import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId, TransactionsCarryingPersistentNodeViewModifier}

case class ErgoFullBlock(header: Header,
                         blockTransactions: BlockTransactions,
                         aDProofs: Option[ADProofs])
  extends ErgoPersistentModifier
    with TransactionsCarryingPersistentNodeViewModifier[ErgoTransaction] {

  lazy val toSeq: Seq[ErgoPersistentModifier] = Seq(header, blockTransactions) ++ aDProofs.toSeq

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

  implicit val jsonEncoder: Encoder[ErgoFullBlock] = (b: ErgoFullBlock) =>
    Map(
      "header" -> b.header.asJson,
      "blockTransactions" -> b.blockTransactions.asJson,
      "adProofs" -> b.aDProofs.map(_.asJson).getOrElse(Map.empty[String, String].asJson),
      "bytesSize" ->
        (b.header.bytes.length + b.blockTransactions.bytes.length + b.aDProofs.map(_.bytes.length).getOrElse(0))
          .toLong
          .asJson
    ).asJson
}
