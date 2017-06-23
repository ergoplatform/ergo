package org.ergoplatform.modifiers.block

import com.google.common.primitives.{Bytes, Ints, Shorts}
import io.circe.Json
import org.ergoplatform.modifiers.transaction.{AnyoneCanSpendTransaction, AnyoneCanSpendTransactionSerializer}
import org.ergoplatform.settings.Constants
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.block.Block._
import scorex.core.serialization.Serializer

import scala.util.Try

case class ErgoFullBlock(header: ErgoHeader, txs: Seq[AnyoneCanSpendTransaction]) extends ErgoBlock {


  override lazy val version: Version = header.version

  override lazy val timestamp: Timestamp = header.timestamp

  override lazy val parentId: ModifierId = header.parentId

  override val json: Json = header.json

  override val transactions: Option[Seq[AnyoneCanSpendTransaction]] = Some(txs)

  override val modifierTypeId: ModifierTypeId = ErgoFullBlock.ModifierTypeId

  override val id: ModifierId = header.id

  override type M = ErgoFullBlock

  override def serializer: Serializer[ErgoFullBlock] = ErgoFullBlockSerializer
}

object ErgoFullBlock {
  val ModifierTypeId = 11: Byte

  def apply(version: Version,
            parentId: BlockId,
            interlinks: Seq[Array[Version]],
            stateRoot: Array[Version],
            txs: Seq[AnyoneCanSpendTransaction],
            timestamp: Timestamp,
            nonce: Int): ErgoFullBlock = {
    val header = ErgoHeader(version, parentId, interlinks, stateRoot, calcTransactionsRootHash(txs), timestamp, nonce)
    ErgoFullBlock(header, txs)
  }

  def calcTransactionsRootHash(txs: Seq[AnyoneCanSpendTransaction]): Array[Byte] = {
    //TODO Implement
    Constants.hash(txs.headOption.map(_.bytes).getOrElse("empty".getBytes))
  }
}

object ErgoFullBlockSerializer extends Serializer[ErgoFullBlock] {
  override def toBytes(obj: ErgoFullBlock): Array[Version] = {
    val headerBytes = obj.header.bytes
    val transactionBytes = scorex.core.utils.concatBytes(
      obj.txs.flatMap(tx => Seq(Ints.toByteArray(tx.bytes.length), tx.bytes)))

    Bytes.concat(Shorts.toByteArray(headerBytes.length.toShort), headerBytes, transactionBytes)
  }

  override def parseBytes(bytes: Array[Version]): Try[ErgoFullBlock] = Try {
    def parseTransactions(position: Int, acc: Seq[AnyoneCanSpendTransaction]): Seq[AnyoneCanSpendTransaction] = {
      if (position < bytes.length) {
        val l = Ints.fromByteArray(bytes.slice(position, position + 4))
        val tx = AnyoneCanSpendTransactionSerializer.parseBytes(bytes.slice(position + 4, position + 4 + l)).get
        parseTransactions(position + 4 + l, acc :+ tx)
      } else {
        acc
      }
    }
    val headerLength = Shorts.fromByteArray(bytes.slice(0, 2))
    val header = ErgoHeaderSerializer.parseBytes(bytes.slice(2, 2 + headerLength)).get
    val transactions = parseTransactions(2 + headerLength, Seq())
    ErgoFullBlock(header, transactions)
  }
}