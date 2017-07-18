package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Shorts}
import io.circe.Json
import org.ergoplatform.modifiers.mempool.{AnyoneCanSpendTransaction, AnyoneCanSpendTransactionSerializer}
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.core.utils.concatBytes
import scorex.crypto.encode.Base58

import scala.util.Try

case class BlockTransactions(headerId: ModifierId, txs: Seq[AnyoneCanSpendTransaction]) extends HistoryModifier {

  override val modifierTypeId: ModifierTypeId = BlockTransactions.ModifierTypeId

  override lazy val id: ModifierId = if(txs.nonEmpty) BlockTransactions.rootHash(txs.map(_.id)) else headerId

  override type M = BlockTransactions

  override lazy val serializer: Serializer[BlockTransactions] = BlockTransactionsSerializer

  override lazy val json: Json = ???

  override def toString: String = s"BlockTransactions(${Base58.encode(id)},${Base58.encode(headerId)},$txs)"

}

object BlockTransactions {
  val ModifierTypeId: Byte = 102: Byte

  def rootHash(ids: Seq[Array[Byte]]): Array[Byte] = Algos.merkleTreeRoot(ids)
}

object BlockTransactionsSerializer extends Serializer[BlockTransactions] {
  override def toBytes(obj: BlockTransactions): Array[ModifierTypeId] = {
    val txsBytes = concatBytes(obj.txs.map(tx => Bytes.concat(Shorts.toByteArray(tx.bytes.length.toShort), tx.bytes)))
    Bytes.concat(obj.headerId, txsBytes)
  }

  override def parseBytes(bytes: Array[ModifierTypeId]): Try[BlockTransactions] = Try {
    val headerId = bytes.slice(0, Constants.ModifierIdSize)

    def parseTransactions(index: Int, acc: Seq[AnyoneCanSpendTransaction]): BlockTransactions = {
      if (index == bytes.length) {
        BlockTransactions(headerId, acc)
      } else {
        val txLength = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val tx = AnyoneCanSpendTransactionSerializer.parseBytes(bytes.slice(index + 2, index + 2 + txLength)).get
        parseTransactions(index + 2 + txLength, acc :+ tx)
      }
    }
    parseTransactions(Constants.ModifierIdSize, Seq())
  }
}