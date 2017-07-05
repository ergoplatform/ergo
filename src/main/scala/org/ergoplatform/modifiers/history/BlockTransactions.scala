package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Shorts}
import io.circe.Json
import org.ergoplatform.modifiers.mempool.{AnyoneCanSpendTransaction, AnyoneCanSpendTransactionSerializer}
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.serialization.Serializer
import scorex.core.utils.concatBytes

import scala.util.Try

case class BlockTransactions(txs: Seq[AnyoneCanSpendTransaction]) extends HistoryModifier {

  override val modifierTypeId: ModifierTypeId = BlockTransactions.ModifierTypeId

  override lazy val id: ModifierId = BlockTransactions.rootHash(txs.map(_.id))

  override type M = BlockTransactions

  override lazy val serializer: Serializer[BlockTransactions] = BlockTransactionsSerializer

  override lazy val json: Json = ???

}

object BlockTransactions {
  val ModifierTypeId: Byte = 102: Byte

  def rootHash(ids: Seq[Array[Byte]]): Array[Byte] = Algos.merkleTreeRoot(ids)
}

object BlockTransactionsSerializer extends Serializer[BlockTransactions] {
  override def toBytes(obj: BlockTransactions): Array[ModifierTypeId] = {
    val txsBytes = concatBytes(obj.txs.map(tx => Bytes.concat(Shorts.toByteArray(tx.bytes.length.toShort), tx.bytes)))
    Bytes.concat(txsBytes)
  }

  override def parseBytes(bytes: Array[ModifierTypeId]): Try[BlockTransactions] = Try {
    def parseTransactions(index: Int, acc: Seq[AnyoneCanSpendTransaction]): BlockTransactions = {
      if (index == bytes.length) {
        BlockTransactions(acc)
      } else {
        val txLength = Shorts.fromByteArray(bytes.slice(index, index + 2))
        val tx = AnyoneCanSpendTransactionSerializer.parseBytes(bytes.slice(index + 2, index + 2 + txLength)).get
        parseTransactions(index + 2 + txLength, acc :+ tx)
      }
    }
    parseTransactions(0, Seq())
  }
}