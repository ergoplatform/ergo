package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.history.header.HeaderSerializer
import org.ergoplatform.modifiers.mempool.ErgoTransactionSerializer
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecInputBlocks
import scorex.util.{bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._

object OrderingBlockAnnouncementMessageSpec extends MessageSpecInputBlocks[OrderingBlockAnnouncement] {
  /**
    * Code which identifies what message type is contained in the payload
    */
  override val messageCode: MessageCode = 104: Byte

  /**
    * Name of this message type. For debug purposes only.
    */
  override val messageName: String = "OrderingBlockAnnouncement"

  override def serialize(ann: OrderingBlockAnnouncement, w: Writer): Unit = {
    w.put(ann.version)
    HeaderSerializer.serialize(ann.header, w)
    w.putUInt(ann.nonBroadcastedTransactions.length)
    ann.nonBroadcastedTransactions.foreach{ tx =>  // todo: replace with cfor
      ErgoTransactionSerializer.serialize(tx, w)
    }
    w.putUInt(ann.broadcastedTransactionIds.length)
    ann.broadcastedTransactionIds.foreach { txId => // todo: replace with cfor
      w.putBytes(idToBytes(txId))
    }
  }

  override def parse(r: Reader): OrderingBlockAnnouncement = {
    // todo: check for max message size
    val version = r.getByte()
    val header = HeaderSerializer.parse(r)
    val nbtCount = r.getUInt().toIntExact
    val txs = (1 to nbtCount).map { _ =>
      ErgoTransactionSerializer.parse(r)
    }.toArray // todo: replace with cfor
    val txIdsCount = r.getUInt().toIntExact
    val txIds = (1 to txIdsCount).map { _ => // todo: replace with cfor
      bytesToId(r.getBytes(32))
    }.toArray
    OrderingBlockAnnouncement(version, header, txs, txIds)
    // todo: consider versioning by skipping unparsed bytes if version > 1
  }

}
