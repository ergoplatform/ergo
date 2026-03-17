package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.history.extension.Extension
import org.ergoplatform.modifiers.history.header.HeaderSerializer
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecInputBlocks
import scorex.util.{bytesToId, idToBytes, ModifierId}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._
import spire.syntax.all.cfor

object OrderingBlockAnnouncementMessageSpec extends MessageSpecInputBlocks[OrderingBlockAnnouncement] {

  private val maxSize = 32000 // todo: check and describe why always ok
  
  /**
    * Current protocol version for OrderingBlockAnnouncement messages
    */
  private val CurrentVersion: Byte = 1.toByte

  /**
    * Code which identifies what message type is contained in the payload
    */
  override val messageCode: MessageCode = 106: Byte

  /**
    * Name of this message type. For debug purposes only.
    */
  override val messageName: String = "OrderingBlockAnnouncement"

  override def serialize(ann: OrderingBlockAnnouncement, w: Writer): Unit = {
    w.put(CurrentVersion)
    HeaderSerializer.serialize(ann.header, w)
    w.putUInt(ann.nonBroadcastedTransactions.length)
    cfor(0)(_ < ann.nonBroadcastedTransactions.length, _ + 1) { i =>
      ErgoTransactionSerializer.serialize(ann.nonBroadcastedTransactions(i), w)
    }
    w.putUInt(ann.broadcastedTransactionIds.length)
    cfor(0)(_ < ann.broadcastedTransactionIds.length, _ + 1) { i =>
      w.putBytes(idToBytes(ann.broadcastedTransactionIds(i)))
    }
    w.putUShort(ann.extensionFields.size)
    cfor(0)(_ < ann.extensionFields.length, _ + 1) { i =>
      val (key, value) = ann.extensionFields(i)
      w.putBytes(key)
      w.putUByte(value.length)
      w.putBytes(value)
    }
  }

  override def parse(r: Reader): OrderingBlockAnnouncement = {

    /**
      * Maximum allowed count for array allocations during message parsing to prevent DoS attacks
      */
    val MaxArraySize: Int = 32768

    val startPosition = r.position
    val version = r.getByte()
    val header = HeaderSerializer.parse(r)
    
    val nbtCount = r.getUInt().toIntExact
    require(nbtCount <= MaxArraySize, s"Non-broadcasted transactions count too large: $nbtCount")
    val txs = new Array[ErgoTransaction](nbtCount)
    cfor(0)(_ < nbtCount, _ + 1) { i =>
      txs(i) = ErgoTransactionSerializer.parse(r)
    }
    require(r.position - startPosition < maxSize)
    
    val txIdsCount = r.getUInt().toIntExact
    require(txIdsCount <= MaxArraySize, s"Transaction IDs count too large: $txIdsCount")
    val txIds = new Array[ModifierId](txIdsCount)
    cfor(0)(_ < txIdsCount, _ + 1) { i =>
      txIds(i) = bytesToId(r.getBytes(32))
    }
    require(r.position - startPosition < maxSize)
    
    val fieldsSize = r.getUShort()
    require(fieldsSize <= MaxArraySize, s"Extension fields count too large: $fieldsSize")
    val fields = new Array[(Array[Byte], Array[Byte])](fieldsSize)
    cfor(0)(_ < fieldsSize, _ + 1) { i =>
      val key = r.getBytes(Extension.FieldKeySize)
      val length = r.getUByte()
      val value = r.getBytes(length)
      fields(i) = (key, value)
    }
    require(r.position - startPosition < maxSize)
    OrderingBlockAnnouncement(header, txs, txIds, fields)
    // todo: consider versioning by skipping unparsed bytes if version > 1
  }

}
