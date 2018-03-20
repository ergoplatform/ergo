package org.ergoplatform.modifiers.mempool

import com.google.common.primitives.Bytes
import io.circe._
import io.circe.syntax._
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.settings.Algos
import scorex.core.serialization.Serializer
import scorex.core.{ModifierId, ModifierTypeId, NodeViewModifier}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class TransactionIdsForHeader(ids: Seq[ModifierId]) extends MempoolModifier {
  override val modifierTypeId: ModifierTypeId = TransactionIdsForHeader.modifierTypeId

  override lazy val id: ModifierId = ModifierId @@ Algos.hash(scorex.core.utils.concatFixLengthBytes(ids))

  override type M = TransactionIdsForHeader

  override lazy val serializer: Serializer[TransactionIdsForHeader] = TransactionIdsForHeaderSerializer

  lazy val rootHash: Digest32 = BlockTransactions.rootHash(ids)
}

object TransactionIdsForHeader {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (103: Byte)

  def validate(txIds: TransactionIdsForHeader, header: Header): Boolean = {
    header.transactionsRoot sameElements txIds.rootHash
  }

  implicit val jsonEncoder: Encoder[TransactionIdsForHeader] = (t: TransactionIdsForHeader) =>
    Map(
      "ids" -> t.ids.map(Base58.encode)
    ).asJson

}

object TransactionIdsForHeaderSerializer extends Serializer[TransactionIdsForHeader] {

  val fixedSize = NodeViewModifier.ModifierIdSize

  override def toBytes(obj: TransactionIdsForHeader): Array[Byte] =
    Bytes.concat(obj.ids: _*).ensuring(_.length % fixedSize == 0)

  override def parseBytes(bytes: Array[Byte]): Try[TransactionIdsForHeader] = Try {
    require(bytes.length % fixedSize == 0)
    val ids = Range(0, bytes.length, fixedSize).map { i => bytes.slice(i, i + fixedSize) }
    TransactionIdsForHeader(ModifierId @@ ids)
  }
}
