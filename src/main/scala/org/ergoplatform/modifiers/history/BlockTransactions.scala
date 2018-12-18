package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Ints}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}
import org.ergoplatform.api.ApiCodecs
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core._
import scorex.core.serialization.Serializer
import scorex.core.utils.concatBytes
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, bytesToId, idToBytes}

import scala.util.{Failure, Success, Try}

case class BlockTransactions(headerId: ModifierId, txs: Seq[ErgoTransaction], override val sizeOpt: Option[Int] = None)
  extends BlockSection
    with TransactionsCarryingPersistentNodeViewModifier[ErgoTransaction] {

  assert(txs.nonEmpty, "Block should always contain at least 1 coinbase-like transaction")

  override val modifierTypeId: ModifierTypeId = BlockTransactions.modifierTypeId

  override def digest: Digest32 = BlockTransactions.transactionsRoot(txs)

  override type M = BlockTransactions

  override lazy val serializer: Serializer[BlockTransactions] = BlockTransactionsSerializer

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

  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (102: Byte)

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

object BlockTransactionsSerializer extends Serializer[BlockTransactions] {
  override def toBytes(obj: BlockTransactions): Array[Byte] = {
    val txsBytes = concatBytes(obj.txs.map { tx =>
      val txBytes = ErgoTransactionSerializer.toBytes(tx)
      Bytes.concat(Ints.toByteArray(txBytes.length), txBytes)
    })
    Bytes.concat(idToBytes(obj.headerId), txsBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[BlockTransactions] = Try {
    val headerId: ModifierId = bytesToId(bytes.slice(0, Constants.ModifierIdSize))

    def parseTransactions(index: Int, acc: Seq[ErgoTransaction]): BlockTransactions = {
      if (index == bytes.length) {
        BlockTransactions(headerId, acc)
      } else {
        val txLength = Ints.fromByteArray(bytes.slice(index, index + 4))
        require(txLength > 0)
        val tx = ErgoTransactionSerializer.parseBytes(bytes.slice(index + 4, index + 4 + txLength)) match {
          case Success(parsedTx) => parsedTx
          case Failure(f) => throw f
        }
        parseTransactions(index + 4 + txLength, acc :+ tx)
      }
    }

    parseTransactions(Constants.ModifierIdSize, Seq.empty).copy(sizeOpt = Some(bytes.length))
  }
}
