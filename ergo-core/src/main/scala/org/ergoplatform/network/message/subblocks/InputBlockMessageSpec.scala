package org.ergoplatform.network.message.subblocks

import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecInputBlocks
import org.ergoplatform.subblocks.InputBlockInfo
import scorex.util.serialization.{Reader, Writer}

/**
  * Message that is informing about sub block produced.
  * Contains header and link to previous sub block ().
  */
object InputBlockMessageSpec extends MessageSpecInputBlocks[InputBlockInfo] {

  val MaxMessageSize = 10000

  override val messageCode: MessageCode = 90: Byte
  override val messageName: String = "SubBlock"

  override def serialize(data: InputBlockInfo, w: Writer): Unit = {
    InputBlockInfo.serializer.serialize(data, w)
  }

  override def parse(r: Reader): InputBlockInfo = {
    InputBlockInfo.serializer.parse(r)
  }
}
