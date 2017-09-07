package org.ergoplatform.modifiers.history

import com.google.common.primitives.{Bytes, Shorts}
import io.circe.Json
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.modifiers.mempool.{AnyoneCanSpendTransaction, AnyoneCanSpendTransactionSerializer}
import org.ergoplatform.modifiers.{ErgoPersistentModifier, ModifierWithDigest}
import org.ergoplatform.settings.{Algos, Constants}
import scorex.core.{ModifierId, ModifierTypeId}
import scorex.core.TransactionsCarryingPersistentNodeViewModifier
import scorex.core.serialization.Serializer
import scorex.core.utils.concatBytes
import scorex.crypto.authds.LeafData
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Digest32

import scala.util.Try

case class BlockTransactions(headerId: ModifierId, txs: Seq[AnyoneCanSpendTransaction])
  extends ErgoPersistentModifier
    with TransactionsCarryingPersistentNodeViewModifier[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction]
  with ModifierWithDigest {

  assert(txs.nonEmpty, "Block should contain at least 1 coinbase-like transaction")

  override val modifierTypeId: ModifierTypeId = BlockTransactions.modifierTypeId

  override def digest: Digest32 = BlockTransactions.rootHash(txs.map(_.id))

  override type M = BlockTransactions

  override lazy val serializer: Serializer[BlockTransactions] = BlockTransactionsSerializer

  override lazy val json: Json = ???

  override def toString: String = s"BlockTransactions(${Base58.encode(id)},${Base58.encode(headerId)},$txs)"

  override lazy val transactions: Seq[AnyoneCanSpendTransaction] = txs
}

object BlockTransactions {
  val modifierTypeId: ModifierTypeId = ModifierTypeId @@ (102: Byte)

  def rootHash(ids: Seq[ModifierId]): Digest32 = Algos.merkleTreeRoot(ids)
}

object BlockTransactionsSerializer extends Serializer[BlockTransactions] {
  override def toBytes(obj: BlockTransactions): Array[Byte] = {
    val txsBytes = concatBytes(obj.txs.map{tx =>
      assert(tx.bytes.length.toShort % 8 == 0)
      Bytes.concat(Shorts.toByteArray(tx.bytes.length.toShort), tx.bytes)})
    Bytes.concat(obj.headerId, txsBytes)
  }

  override def parseBytes(bytes: Array[Byte]): Try[BlockTransactions] = Try {
    val headerId: ModifierId = ModifierId @@ bytes.slice(0, Constants.ModifierIdSize)

    def parseTransactions(index: Int, acc: Seq[AnyoneCanSpendTransaction]): BlockTransactions = {
      if (index == bytes.length) {
        BlockTransactions(headerId, acc)
      } else {
        val txLength = Shorts.fromByteArray(bytes.slice(index, index + 2)).ensuring(_ % 8 == 0)
        val tx = AnyoneCanSpendTransactionSerializer.parseBytes(bytes.slice(index + 2, index + 2 + txLength)).get
        parseTransactions(index + 2 + txLength, acc :+ tx)
      }
    }
    parseTransactions(Constants.ModifierIdSize, Seq())
  }
}