package org.ergoplatform.modifiers.mempool

import com.google.common.primitives.Bytes
import io.circe._
import io.circe.syntax._
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.nodeView.state.ErgoState.Digest
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.{JsonSerializable, Serializer}
import scorex.crypto.encode.Base58

import scala.util.Try

case class TransactionIdsForHeader(ids: Seq[ModifierId]) extends MempoolModifier with JsonSerializable {
  override val modifierTypeId: ModifierTypeId = TransactionIdsForHeader.ModifierTypeId

  override lazy val id: ModifierId = Algos.hash(scorex.core.utils.concatFixLengthBytes(ids))

  override type M = TransactionIdsForHeader

  override lazy val serializer: Serializer[TransactionIdsForHeader] = TransactionIdsForHeaderSerializer

  override lazy val json: Json = Map(
    "ids" -> ids.map(Base58.encode)
  ).asJson

  lazy val rootHash: Digest = BlockTransactions.rootHash(ids)
}

object TransactionIdsForHeader {
  val ModifierTypeId: Byte = 103: Byte

  def validate(txIds: TransactionIdsForHeader, header: Header): Boolean = {
    header.transactionsRoot sameElements txIds.rootHash
  }
}

object TransactionIdsForHeaderSerializer extends Serializer[TransactionIdsForHeader] {

  val fixedSize = NodeViewModifier.ModifierIdSize

  override def toBytes(obj: TransactionIdsForHeader): Array[Byte] =
    Bytes.concat(obj.ids: _*).ensuring(_.length % fixedSize == 0)

  override def parseBytes(bytes: Array[Byte]): Try[TransactionIdsForHeader] = Try {
    require(bytes.length % fixedSize == 0)
    val ids = Range(0, bytes.length, fixedSize).map { i => bytes.slice(i, i + fixedSize) }
    TransactionIdsForHeader(ids)
  }
}
