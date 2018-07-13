package org.ergoplatform.modifiers.mempool

import com.google.common.primitives.Bytes
import io.circe._
import io.circe.syntax._
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.settings.Algos
import scorex.core._
import scorex.core.serialization.Serializer
import scorex.crypto.hash.Digest32

import scala.util.Try

// TODO is not used for now
case class TransactionIdsForHeader(ids: Seq[ModifierId]) extends MempoolModifier {
  override val modifierTypeId: ModifierTypeId = TransactionIdsForHeader.modifierTypeId

  override lazy val id: ModifierId = bytesToId(Algos.hash(scorex.core.utils.concatFixLengthBytes(ids.map(idToBytes))))

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
      "ids" -> t.ids.map(Algos.encode)
    ).asJson

}

object TransactionIdsForHeaderSerializer extends Serializer[TransactionIdsForHeader] {

  val fixedSize = NodeViewModifier.ModifierIdSize

  override def toBytes(obj: TransactionIdsForHeader): Array[Byte] =
    Bytes.concat(obj.ids.map(idToBytes): _*).ensuring(_.length % fixedSize == 0)

  override def parseBytes(bytes: Array[Byte]): Try[TransactionIdsForHeader] = Try {
    require(bytes.length % fixedSize == 0)
    val ids = Range(0, bytes.length, fixedSize).map { i => bytesToId(bytes.slice(i, i + fixedSize)) }
    TransactionIdsForHeader(ids)
  }
}
