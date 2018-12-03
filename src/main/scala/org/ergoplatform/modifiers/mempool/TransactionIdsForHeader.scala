package org.ergoplatform.modifiers.mempool

import io.circe._
import io.circe.syntax._
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.settings.Algos
import scorex.core.serialization.ScorexSerializer
import scorex.core.{ModifierTypeId, NodeViewModifier}
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId}

// TODO is not used for now
case class TransactionIdsForHeader(serializedIds: Seq[Array[Byte]]) extends MempoolModifier {
  override val modifierTypeId: ModifierTypeId = TransactionIdsForHeader.modifierTypeId

  override lazy val id: ModifierId = bytesToId(Algos.hash(scorex.core.utils.concatFixLengthBytes(serializedIds)))

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

object TransactionIdsForHeaderSerializer extends ScorexSerializer[TransactionIdsForHeader] {

  val fixedSize: Int = NodeViewModifier.ModifierIdSize

  override def serialize(obj: TransactionIdsForHeader, w: Writer): Unit = {
    obj.serializedIds.ensuring(_.forall(_.length == fixedSize))
    w.putInt(obj.serializedIds.size)
    obj.serializedIds.foreach{ id =>
      w.putBytes(id)
    }
  }

  override def parse(r: Reader): TransactionIdsForHeader = {
    val length = r.getInt()
    val ids = (1 to length).map { _ =>
      r.getBytes(fixedSize)
    }
    TransactionIdsForHeader(ids)
  }
}
