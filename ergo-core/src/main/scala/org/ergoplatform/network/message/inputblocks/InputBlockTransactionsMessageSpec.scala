package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecInputBlocks
import org.ergoplatform.settings.Constants
import scorex.util.{bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import sigma.util.Extensions.LongOps
import spire.syntax.all.cfor

object InputBlockTransactionsMessageSpec extends MessageSpecInputBlocks[InputBlockTransactionsData] {
  /**
    * Code which identifies what message type is contained in the payload
    */
  override val messageCode: MessageCode = 104: Byte
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
    val subBlockId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val txsCount = r.getUInt().toIntExact

    val txs = new Array[ErgoTransaction](txsCount)
    cfor(0)(_ < txsCount, _ + 1) { i =>
      txs(i) = ErgoTransactionSerializer.parse(r)
    }
    InputBlockTransactionsData(subBlockId, txs)
  }

}
