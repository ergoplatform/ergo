package org.ergoplatform.modifiers

import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Extension, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId, TransactionsCarryingPersistentNodeViewModifier}

case class ErgoFullBlock(header: Header,
                         blockTransactions: BlockTransactions,
                         extension: Extension,
                         adProofs: Option[ADProofs])
  extends ErgoPersistentModifier
    with TransactionsCarryingPersistentNodeViewModifier[ErgoTransaction] {

  override type M = ErgoFullBlock

  override val modifierTypeId: ModifierTypeId = ErgoFullBlock.modifierTypeId

  override val serializedId: Array[Byte] = header.serializedId

  override lazy val id: ModifierId = header.id

  override val parentId: ModifierId = header.parentId

  lazy val blockSections: Seq[BlockSection] = Seq(adProofs, Some(blockTransactions), Some(extension)).flatten

  lazy val toSeq: Seq[ErgoPersistentModifier] = header +: blockSections

  override lazy val transactions: Seq[ErgoTransaction] = blockTransactions.txs

  override val sizeOpt: Option[Int] = None

  override lazy val size: Int = {
    val hSize = header.size
    val btSize = blockTransactions.size
    val adSize = adProofs.map(_.size).getOrElse(0)
    hSize + btSize + adSize
  }

  override lazy val serializer: Serializer[ErgoFullBlock] = throw new Error("Never try to serialize ErgoFullBlock")

}

object ErgoFullBlock extends ApiCodecs {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (-127: Byte)

  implicit val jsonEncoder: Encoder[ErgoFullBlock] = (b: ErgoFullBlock) =>
    Json.obj(
      "header" -> b.header.asJson,
      "blockTransactions" -> b.blockTransactions.asJson,
      "extension" -> b.extension.asJson,
      "adProofs" -> b.adProofs.asJson,
      "size" -> b.size.asJson
    )

  val blockSizeEncoder: Encoder[ErgoFullBlock] = (b: ErgoFullBlock) =>
    Json.obj(
      "id" -> b.header.id.asJson,
      "size" -> b.size.asJson
    )

  private def size(block: ErgoFullBlock): Int = {
    block.header.bytes.length +
      block.blockTransactions.bytes.length +
      block.adProofs.map(_.bytes.length).getOrElse(0)
  }
}
