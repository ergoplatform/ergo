package org.ergoplatform.modifiers

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.serialization.ErgoSerializer
import scorex.core.TransactionsCarryingPersistentNodeViewModifier
import scorex.util.ModifierId

case class ErgoFullBlock(header: Header,
                         blockTransactions: BlockTransactions,
                         extension: Extension,
                         adProofs: Option[ADProofs])
  extends BlockSection
    with TransactionsCarryingPersistentNodeViewModifier {

  override type M = ErgoFullBlock

  override val modifierTypeId: NetworkObjectTypeId.Value = ErgoFullBlock.modifierTypeId

  override def serializedId: Array[Byte] = header.serializedId

  override lazy val id: ModifierId = header.id

  override def parentId: ModifierId = header.parentId

  lazy val mandatoryBlockSections: Seq[NonHeaderBlockSection] = Seq(blockTransactions, extension)

  lazy val blockSections: Seq[NonHeaderBlockSection] = adProofs.toSeq ++ mandatoryBlockSections

  lazy val toSeq: Seq[BlockSection] = header +: blockSections

  override lazy val transactions: Seq[ErgoTransaction] = blockTransactions.txs

  override val sizeOpt: Option[Int] = None

  override lazy val size: Int = header.size + blockTransactions.size + adProofs.map(_.size).getOrElse(0)

  override def serializer: ErgoSerializer[ErgoFullBlock] =
    throw new Error("Serialization for ErgoFullBlock is not (and will be not) implemented")

  def height: Int = header.height

}

object ErgoFullBlock extends ApiCodecs {

  val modifierTypeId: NetworkObjectTypeId.Value = FullBlockTypeId.value

  implicit val jsonEncoder: Encoder[ErgoFullBlock] = { b: ErgoFullBlock =>
    Json.obj(
      "header" -> b.header.asJson,
      "blockTransactions" -> b.blockTransactions.asJson,
      "extension" -> b.extension.asJson,
      "adProofs" -> b.adProofs.asJson,
      "size" -> b.size.asJson
    )
  }

  implicit val jsonDecoder: Decoder[ErgoFullBlock] = { c: HCursor =>
    for {
      header <- c.downField("header").as[Header]
      transactions <- c.downField("blockTransactions").as[BlockTransactions]
      extension <- c.downField("extension").as[Extension]
      adProofs <- c.downField("adProofs").as[Option[ADProofs]]
    } yield ErgoFullBlock(header, transactions, extension, adProofs)
  }

  val blockSizeEncoder: Encoder[ErgoFullBlock] = { b: ErgoFullBlock =>
    Json.obj(
      "id" -> b.header.id.asJson,
      "size" -> b.size.asJson
    )
  }
}
