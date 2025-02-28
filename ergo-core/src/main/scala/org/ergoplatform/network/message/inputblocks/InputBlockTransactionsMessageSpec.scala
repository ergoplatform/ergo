package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.mempool.ErgoTransactionSerializer
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecInputBlocks
import scorex.util.{bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import sigma.util.Extensions.LongOps

object InputBlockTransactionsMessageSpec extends MessageSpecInputBlocks[InputBlockTransactionsData]{
  /**
    * Code which identifies what message type is contained in the payload
    */
  override val messageCode: MessageCode = 92: Byte
  /**
    * Name of this message type. For debug purposes only.
    */
  override val messageName: String = "InputBlockTxs"

  override def serialize(obj: InputBlockTransactionsData, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.inputBlockId))
    w.putUInt(obj.transactions.size)
    obj.transactions.foreach { tx =>
      ErgoTransactionSerializer.serialize(tx, w)
    }
  }

  override def parse(r: Reader): InputBlockTransactionsData = {
    val subBlockId = bytesToId(r.getBytes(32))
    val txsCount = r.getUInt().toIntExact
    val transactions = (1 to txsCount).map{_ =>
      ErgoTransactionSerializer.parse(r)
    }
    InputBlockTransactionsData(subBlockId, transactions)
  }

}
