package org.ergoplatform.network

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.network.message.{InvData, InvSpec, Message, MessageSerializer}
import scorex.util.encode.Base16

class InvSpecification extends ErgoPropertyTest with DecodingUtils {

  property("inv reference parser") {
    val magic = Array(1: Byte, 0: Byte, 2: Byte, 4: Byte) // mainnet magic
    val invSpec = new InvSpec(maxInvObjects = 100)

    val headerId = Array.fill(16)(1: Byte) ++ Array.fill(16)(2: Byte)

    val invData = InvData(Header.modifierTypeId, Seq(headerId))

    val invMessage = Message(invSpec, Right(invData), None)

    val ms = new MessageSerializer(Seq(invSpec), magic)

    val bs = ms.serialize(invMessage).toArray
    val bsString = Base16.encode(bs)

    bsString shouldBe "0100020441000000214f904163010101010101010101010101010101010102020202020202020202020202020202"

  }

}
