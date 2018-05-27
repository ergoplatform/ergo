package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Shorts}
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoPersistentModifier, ModifierWithDigest}
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.serialization.Serializer
import scorex.core.utils.concatBytes
import scorex.core.{ModifierId, ModifierTypeId, TransactionsCarryingPersistentNodeViewModifier}
import scorex.crypto.authds.LeafData
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.{Failure, Success, Try}

case class BlockTransactions(headerId: ModifierId, txs: Seq[ErgoTransaction])
  extends ErgoPersistentModifier
    with TransactionsCarryingPersistentNodeViewModifier[ErgoTransaction]
  with ModifierWithDigest {

  assert(txs.nonEmpty, "Block should always contain at least 1 coinbase-like transaction")

  override val modifierTypeId: ModifierTypeId = BlockTransactions.modifierTypeId

  override def digest: Digest32 = BlockTransactions.rootHash(txs.map(_.id))

  override type M = BlockTransactions

  override lazy val serializer: Serializer[BlockTransactions] = BlockTransactionsSerializer

  override def toString: String = {
    val idStr = Algos.encode(id)
    val headerIdStr = Algos.encode(headerId)
    /**
      * Artificial limit to show only first 10 txs.
      */
    val txsStr = txs.take(10).map(_.toString).mkString(",")
    val txsSuffix = if (txs.length > 10) ", ..." else ""

    s"BlockTransactions(Id:$idStr,HeaderId:$headerIdStr,Txs:$txsStr$txsSuffix)"
  }

  override lazy val transactions: Seq[ErgoTransaction] = txs
}

object BlockTransactions {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (102: Byte)

  def rootHash(ids: Seq[ModifierId]): Digest32 = Algos.merkleTreeRoot(LeafData @@ ids)

  implicit val jsonEncoder: Encoder[BlockTransactions] = (bt: BlockTransactions) =>
    Map(
      "headerId" -> Base58.encode(bt.headerId).asJson,
      "transactions" -> bt.txs.map(_.asJson).asJson
    ).asJson
}

object BlockTransactionsSerializer extends Serializer[BlockTransactions] {
  override def toBytes(obj: BlockTransactions): Array[Byte] = {
    val txsBytes = concatBytes(obj.txs.map{tx =>
      val txBytes = ErgoTransaction.serializer.toBytes(tx)
      Bytes.concat(Shorts.toByteArray(txBytes.length.toShort), txBytes)}) //todo: short is wrong
    Bytes.concat(obj.headerId, txsBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[BlockTransactions] = Try {
    val headerId: ModifierId = ModifierId @@ bytes.slice(0, Constants.ModifierIdSize)

    def parseTransactions(index: Int, acc: Seq[ErgoTransaction]): BlockTransactions = {
      if (index == bytes.length) {
        BlockTransactions(headerId, acc)
      } else {
        val txLength = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val tx = ErgoTransaction.serializer.parseBytes(bytes.slice(index + 2, index + 2 + txLength)) match {
          case Success(parsedTx) => parsedTx
          case Failure(f) => throw f
        }
        parseTransactions(index + 2 + txLength, acc :+ tx)
      }
    }
    parseTransactions(Constants.ModifierIdSize, Seq.empty)
  }
}
