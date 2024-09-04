package org.ergoplatform.network.message.subblocks

import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecSubblocks
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}

object SubBlockTransactionsRequestSpec extends MessageSpecSubblocks[ModifierId] {
  /**
    * Code which identifies what message type is contained in the payload
    */
  override val messageCode: MessageCode = 91: Byte

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
