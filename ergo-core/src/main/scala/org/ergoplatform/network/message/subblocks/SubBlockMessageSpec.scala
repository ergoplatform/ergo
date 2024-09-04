package org.ergoplatform.network.message.subblocks

import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.{InvData, MessageSpecInitial, MessageSpecSubblocks}
import org.ergoplatform.subblocks.SubBlockInfo
import scorex.util.serialization.{Reader, Writer}

/**
  * Message that is informing about sub block produced.
  * Contains header and link to previous sub block ().
  */
object SubBlockMessageSpec extends MessageSpecSubblocks[SubBlockInfo] {

  val MaxMessageSize = 10000

  override val messageCode: MessageCode = 90: Byte
  override val messageName: String = "SubBlock"

  override def serialize(data: SubBlockInfo, w: Writer): Unit = {
    SubBlockInfo.serializer.serialize(data, w)
  }

  override def parse(r: Reader): SubBlockInfo = {
    SubBlockInfo.serializer.parse(r)
  }
}
