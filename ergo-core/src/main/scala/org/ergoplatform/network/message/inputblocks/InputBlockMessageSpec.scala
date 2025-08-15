package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.message.MessageSpecInputBlocks
import org.ergoplatform.subblocks.InputBlockInfo
import scorex.util.serialization.{Reader, Writer}

/**
  * Message that is informing about sub block produced.
  * Contains header and extension section fields related to sub-blocks (such as link to previous sub block),
  * along with Merkle proof for them.
  */
object InputBlockMessageSpec extends MessageSpecInputBlocks[InputBlockInfo] {

  val MaxMessageSize = 16384

  override val messageCode: MessageCode = 100: Byte
  override val messageName: String = "SubBlock"

  override def serialize(data: InputBlockInfo, w: Writer): Unit = {
    InputBlockInfo.serializer.serialize(data, w)
  }

  override def parse(r: Reader): InputBlockInfo = {
    require(r.remaining < MaxMessageSize, "Too big input block info message")
    InputBlockInfo.serializer.parse(r)
  }

}
