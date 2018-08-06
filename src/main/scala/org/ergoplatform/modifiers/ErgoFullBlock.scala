package org.ergoplatform.modifiers

import io.circe.{Encoder, Json}
import io.circe.syntax._
import org.ergoplatform.api.ApiCodecs
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

  override def serializedId: Array[Byte] = header.serializedId

  override lazy val id: ModifierId = header.id

  override type M = ErgoFullBlock

  override lazy val serializer: Serializer[ErgoFullBlock] =
    throw new Error("Should never try to serialize ErgoFullBlock")

  override lazy val transactions: Seq[ErgoTransaction] = blockTransactions.txs

  val size = {
    val hSize = header.size.getOrElse(0)
    val btSize = blockTransactions.size.getOrElse(0)
    val adSize = aDProofs.flatMap(_.size).getOrElse(0)
    Some(hSize + btSize + adSize)
  }
}

object ErgoFullBlock extends ApiCodecs {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (-127: Byte)

  implicit val jsonEncoder: Encoder[ErgoFullBlock] = (b: ErgoFullBlock) =>
    Json.obj(
      "header" -> b.header.asJson,
      "blockTransactions" -> b.blockTransactions.asJson,
      "adProofs" -> b.aDProofs.map(_.asJson).getOrElse(Map.empty[String, String].asJson),
      "size" -> b.size.asJson
    )

  val blockSizeEncoder: Encoder[ErgoFullBlock] = (b: ErgoFullBlock) =>
    Json.obj(
      "id" -> b.header.id.asJson,
      "size" -> size(b).toLong.asJson
    )

  private def size(block: ErgoFullBlock): Int = {
    block.header.bytes.length +
      block.blockTransactions.bytes.length +
      block.aDProofs.map(_.bytes.length).getOrElse(0)
  }
}
