package org.ergoplatform.modifiers.history

import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core._
import scorex.core.serialization.ScorexSerializer
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.Extensions._

case class BlockTransactions(headerId: ModifierId, txs: Seq[ErgoTransaction], override val sizeOpt: Option[Int] = None)
  extends BlockSection
    with TransactionsCarryingPersistentNodeViewModifier[ErgoTransaction] {

  assert(txs.nonEmpty, "Block should always contain at least 1 coinbase-like transaction")

  override val modifierTypeId: ModifierTypeId = BlockTransactions.TypeId

  override def digest: Digest32 = BlockTransactions.transactionsRoot(txs)

  override type M = BlockTransactions

  override lazy val serializer: ScorexSerializer[BlockTransactions] = BlockTransactionsSerializer

  override def toString: String = {
    val idStr = Algos.encode(id)
    val headerIdStr = Algos.encode(headerId)
    val displayMaxObjects = 5
    // Artificial limit to show only first `displayMaxObjects` txs.
    val txsStr = txs.take(displayMaxObjects).map(_.toString).mkString(",")
    val txsSuffix = if (txs.lengthCompare(displayMaxObjects) > 0) ", ..." else ""

    s"BlockTransactions(id: $idStr, headerId: $headerIdStr, txs: $txsStr$txsSuffix)"
  }

  override lazy val transactions: Seq[ErgoTransaction] = txs
}

object BlockTransactions extends ApiCodecs {

  val TypeId: ModifierTypeId = ModifierTypeId @@ (102: Byte)

  def transactionsRoot(txs: Seq[ErgoTransaction]): Digest32 = rootHash(txs.map(_.serializedId))

  def rootHash(serializedIds: Seq[Array[Byte]]): Digest32 = Algos.merkleTreeRoot(LeafData @@ serializedIds)

  implicit val jsonEncoder: Encoder[BlockTransactions] = { bt: BlockTransactions =>
    Map(
      "headerId" -> Algos.encode(bt.headerId).asJson,
      "transactions" -> bt.txs.map(_.asJson).asJson,
      "size" -> bt.size.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[BlockTransactions] = { c: HCursor =>
    for {
      headerId <- c.downField("headerId").as[ModifierId]
      transactions <- c.downField("transactions").as[List[ErgoTransaction]]
      size <- c.downField("size").as[Int]
    } yield BlockTransactions(headerId, transactions, Some(size))
  }
}

object BlockTransactionsSerializer extends ScorexSerializer[BlockTransactions] {

  override def serialize(obj: BlockTransactions, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.headerId))
    w.putUInt(obj.txs.size)
    obj.txs.foreach { tx =>
      ErgoTransactionSerializer.serialize(tx, w)
    }
  }

  override def parse(r: Reader): BlockTransactions = {
    val startPos = r.position
    val headerId: ModifierId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val size = r.getUInt().toIntExact
    val txs = (1 to size).map { _ =>
      ErgoTransactionSerializer.parse(r)
    }
    BlockTransactions(headerId, txs, Some(r.position - startPos))
  }
}
