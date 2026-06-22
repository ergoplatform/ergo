package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecInputBlocks
import org.ergoplatform.settings.Constants
import scorex.util.{bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import sigma.util.Extensions.LongOps

object InputBlockTransactionIdsMessageSpec extends MessageSpecInputBlocks[InputBlockTransactionIdsData] {
  /**
    * Code which identifies what message type is contained in the payload
    */
  override val messageCode: MessageCode = 102: Byte
  /**
    * Name of this message type. For debug purposes only.
    */
  override val messageName: String = "InputBlockTxs"

  override def serialize(obj: InputBlockTransactionIdsData, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.inputBlockId))
    w.putUInt(obj.transactionIds.size)
    obj.transactionIds.foreach { id =>
      w.putBytes(id)
    }
  }

  override def parse(r: Reader): InputBlockTransactionIdsData = {
    val subBlockId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val txsCount = r.getUInt().toIntExact
    val transactionIds = (1 to txsCount).map { _ =>
      r.getBytes(ErgoTransaction.WeakIdLength)
    }
    InputBlockTransactionIdsData(subBlockId, transactionIds)
  }

}
