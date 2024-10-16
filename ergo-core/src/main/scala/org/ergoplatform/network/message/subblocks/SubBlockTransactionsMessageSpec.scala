package org.ergoplatform.network.message.subblocks

import org.ergoplatform.modifiers.mempool.ErgoTransactionSerializer
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecSubblocks
import scorex.util.{bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import sigma.util.Extensions.LongOps

object SubBlockTransactionsMessageSpec extends MessageSpecSubblocks[SubBlockTransactionsData]{
  /**
    * Code which identifies what message type is contained in the payload
    */
  override val messageCode: MessageCode = 92: Byte
  /**
    * Name of this message type. For debug purposes only.
    */
  override val messageName: String = "SubBlockTxs"

  override def serialize(obj: SubBlockTransactionsData, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.subblockID))
    w.putUInt(obj.transactions.size)
    obj.transactions.foreach { tx =>
      ErgoTransactionSerializer.serialize(tx, w)
    }
  }

  override def parse(r: Reader): SubBlockTransactionsData = {
    val subBlockId = bytesToId(r.getBytes(32))
    val txsCount = r.getUInt().toIntExact
    val transactions = (1 to txsCount).map{_ =>
      ErgoTransactionSerializer.parse(r)
    }
    SubBlockTransactionsData(subBlockId, transactions)
  }

}
