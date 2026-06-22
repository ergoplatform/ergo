package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecInputBlocks
import org.ergoplatform.settings.Constants
import scorex.util.{bytesToId, idToBytes, ModifierId}
import scorex.util.serialization.{Reader, Writer}
import sigma.util.Extensions.LongOps

object InputBlockTransactionsRequestMessageSpec
  extends MessageSpecInputBlocks[InputBlockTransactionsRequest] {

  /**
    * Code which identifies what message type is contained in the payload
    */
  override val messageCode: MessageCode = 105: Byte

  /**
    * Name of this message type. For debug purposes only.
    */
  override val messageName: String = "SubBlockTxsReq"

  override def serialize(req: InputBlockTransactionsRequest, w: Writer): Unit = {
    w.putBytes(idToBytes(req.inputBlockId))
    w.putUInt(req.txIds.length)
    req.txIds.foreach { txId =>
      w.putBytes(txId)
    }
  }

  override def parse(r: Reader): InputBlockTransactionsRequest = {
    val inputBlockId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val cnt          = r.getUInt().toIntExact
    val txIds        = (1 to cnt).map(_ => r.getBytes(ErgoTransaction.WeakIdLength))
    InputBlockTransactionsRequest(inputBlockId, txIds)
  }

}
