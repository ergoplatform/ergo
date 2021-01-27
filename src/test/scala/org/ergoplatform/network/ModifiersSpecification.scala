package org.ergoplatform.network

import java.nio.ByteBuffer

import com.google.common.primitives.Ints
import org.ergoplatform.modifiers.history.{Header, HeaderSerializer}
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.network.message.{Message, MessageSerializer, ModifiersData, ModifiersSpec}
import scorex.util.encode.Base16

class ModifiersSpecification extends ErgoPropertyTest with DecodingUtils {

  property("modifiers message reference parser") {
    val hBytes = Base16.decode("4201ad7fffba807080ce7f808001b4771880bdffbd7fff857f8db17fdc89b0015bff0189486d80934a84015e03c064c6d100c17fae55bf01f380007fbb00ff01d9000000013f0680d10015ff11c4b7ffff01ffb2d27f0001050064010080ffb93e7fff4cc15a2bffdd770480007f8080ff8026802f7f51007f004b01ff0080002380a581e8c0ca81dfca62007fa7ff01860019f37f3c4491067cd1001ee07e570fe8689b80ff11017aff7408703927a2bbc4fa0200000000032bdb4c610ddf0078b4f31f5a36bff7b003704dfc93b201fedd8fe331d9ff1102008d7fff010d80ff").get
    val header = HeaderSerializer.parseBytes(hBytes)

    val magic = Array(1: Byte, 0: Byte, 2: Byte, 4: Byte) // mainnet magic
    val mSpec = new ModifiersSpec(maxMessageSize = 10000)

    val mData = ModifiersData(Header.modifierTypeId, Map(header.id -> header.bytes))

    val mMessage = Message(mSpec, Right(mData), None)

    val ms = new MessageSerializer(Seq(mSpec), magic)

    val bs = ms.serialize(mMessage).toArray
    val bsString = Base16.encode(bs)

    //bsString shouldBe ("01000204210000010" + header.id + Base16.encode(header.bytes)) // prefix + header id + header bytes

    val bb = ByteBuffer.wrap(bs)

    val magicRead = getBytes(bb, 4)
    magicRead.toIndexedSeq shouldBe magic.toIndexedSeq

    val messageCode = getByte(bb)
    messageCode shouldBe mSpec.messageCode  // 33 (in dec)

    val messageLength = Ints.fromByteArray(getBytes(bb,4))

    messageLength shouldBe 261

    val checkSum = getBytes(bb, 4)

    val modifierTypeId = getByte(bb) // should read one byte only

    modifierTypeId shouldBe 101.toByte // type id corresponding to block header

    val headersCount = getULong(bb).toInt // could read up to 4 bytes max

    headersCount shouldBe 1

    val headerIdParsed = getBytes(bb, 32)

    Base16.encode(headerIdParsed) shouldBe header.id

    val headerLength = getULong(bb).toInt // could read up to 4 bytes max

    val headerBytes = getBytes(bb, headerLength)

    headerBytes.toIndexedSeq shouldBe header.bytes.toIndexedSeq
  }

}
