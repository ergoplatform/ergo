package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.history.header.HeaderSerializer
import org.ergoplatform.modifiers.mempool.ErgoTransactionSerializer
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecInputBlocks
import scorex.crypto.authds.LeafData
import scorex.crypto.authds.merkle.MerkleProof
import scorex.crypto.hash.Blake2b256
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
    w.put(1.toByte) // todo: named constant
    HeaderSerializer.serialize(ann.header, w)
    w.putUInt(ann.nonBroadcastedTransactions.length)
    ann.nonBroadcastedTransactions.foreach{ tx =>  // todo: replace with cfor
      ErgoTransactionSerializer.serialize(tx, w)
    }
    w.putUInt(ann.broadcastedTransactionIds.length)
    ann.broadcastedTransactionIds.foreach { txId => // todo: replace with cfor
      w.putBytes(idToBytes(txId))
    }
    if(ann.prevInputBlockId.isDefined) {
      w.put(1.toByte)
      w.putBytes(ann.prevInputBlockId.get._1)
      // todo: implement MerkleProof serializer, and put proof bytes here
    } else {
      w.put(0.toByte)
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
    val defined = r.getByte()
    val prevInputOpt = if(defined == 1) {
      val prevInputId = r.getBytes(32)
      // todo: read Merkle proof
      val p = MerkleProof(LeafData @@ Array.emptyByteArray, Seq.empty)(Blake2b256)
      Some(prevInputId -> p)
    } else {
      None
    }
    OrderingBlockAnnouncement(header, txs, txIds, prevInputOpt)
    // todo: consider versioning by skipping unparsed bytes if version > 1
  }

}
