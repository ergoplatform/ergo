package org.ergoplatform.network

import java.nio.ByteBuffer

import com.google.common.primitives.Ints
import org.ergoplatform.nodeView.history.{ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.network.message.{Message, MessageSerializer}
import scorex.crypto.hash
import scorex.util.ModifierId
import scorex.util.encode.Base16

class ErgoSyncInfoSpecification extends ErgoPropertyTest with DecodingUtils {

  property("sync info reference parser") {
    val magic = Array(1: Byte, 0: Byte, 2: Byte, 4: Byte)
    val syncSpec = ErgoSyncInfoMessageSpec

    val lastHeaderId = Array.fill(16)(1: Byte) ++ Array.fill(16)(2: Byte)

    val syncInfo = ErgoSyncInfo(Seq(ModifierId @@ Base16.encode(lastHeaderId)))

    val syncMessage = Message(syncSpec, Right(syncInfo), None)

    val ms = new MessageSerializer(Seq(syncSpec), magic)

    val bs = ms.serialize(syncMessage).toArray
    val bsString = Base16.encode(bs)

    bsString shouldBe "0100020441000000214f904163010101010101010101010101010101010102020202020202020202020202020202"

    val bb = ByteBuffer.wrap(bs)

    val magicRead = getBytes(bb, 4)
    magicRead.toIndexedSeq shouldBe magic.toIndexedSeq

    val messageCode = getByte(bb)
    messageCode shouldBe syncSpec.messageCode  // 65 (in dec)

    val messageLength = Ints.fromByteArray(getBytes(bb,4))

    messageLength shouldBe 33

    val checkSum = getBytes(bb, 4)


    val headersCount = getULong(bb).toByte // should read one byte only

    headersCount shouldBe 1.toByte

    val headerId = getBytes(bb, 32)

    headerId.toIndexedSeq shouldBe lastHeaderId.toIndexedSeq

    checkSum shouldBe hash.Blake2b256(headersCount +: headerId).take(4)
  }

}
