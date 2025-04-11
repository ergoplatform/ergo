package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecInputBlocks
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}

object InputBlockTransactionsRequestMessageSpec extends MessageSpecInputBlocks[ModifierId] {
  /**
    * Code which identifies what message type is contained in the payload
    */
  override val messageCode: MessageCode = 103: Byte

  /**
    * Name of this message type. For debug purposes only.
    */
  override val messageName: String = "SubBlockTxsReq"

  override def serialize(subBlockId: ModifierId, w: Writer): Unit = {
    w.putBytes(idToBytes(subBlockId))
  }

  override def parse(r: Reader): ModifierId = {
    bytesToId(r.getBytes(32))
  }

}
