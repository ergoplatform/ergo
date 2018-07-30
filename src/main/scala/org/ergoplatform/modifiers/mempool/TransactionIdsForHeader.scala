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
case class TransactionIdsForHeader(serializedIds: Seq[Array[Byte]]) extends MempoolModifier {
  override val modifierTypeId: ModifierTypeId = TransactionIdsForHeader.modifierTypeId

  override lazy val id: ModifierId = bytesToId(Algos.hash(scorex.core.utils.concatFixLengthBytes(serializedIds)))

  override type M = TransactionIdsForHeader

  override lazy val serializer: Serializer[TransactionIdsForHeader] = TransactionIdsForHeaderSerializer

  lazy val rootHash: Digest32 = BlockTransactions.rootHash(serializedIds)
}

object TransactionIdsForHeader {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (103: Byte)

  def validate(txIds: TransactionIdsForHeader, header: Header): Boolean = {
    java.util.Arrays.equals(header.transactionsRoot, txIds.rootHash)
  }

  implicit val jsonEncoder: Encoder[TransactionIdsForHeader] = (t: TransactionIdsForHeader) =>
    Map(
      "ids" -> t.serializedIds.map(Algos.encode)
    ).asJson

}

object TransactionIdsForHeaderSerializer extends Serializer[TransactionIdsForHeader] {

  val fixedSize = NodeViewModifier.ModifierIdSize

  override def toBytes(obj: TransactionIdsForHeader): Array[Byte] =
    Bytes.concat(obj.serializedIds: _*).ensuring(_.length % fixedSize == 0)

  override def parseBytes(bytes: Array[Byte]): Try[TransactionIdsForHeader] = Try {
    require(bytes.length % fixedSize == 0)
    val ids = Range(0, bytes.length, fixedSize).map { i => bytes.slice(i, i + fixedSize) }
    TransactionIdsForHeader(ids)
  }
}
