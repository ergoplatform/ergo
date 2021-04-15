package org.ergoplatform.network

import java.nio.ByteBuffer

import com.google.common.primitives.Ints
import org.ergoplatform.nodeView.history.{ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.core.network.message.{Message, MessageSerializer}
import scorex.crypto.hash
import scorex.util.ModifierId
import scorex.util.encode.Base16


/**
  * syncInfo is a message sent from one peer to another in order to get (and agree) on synchronization status
  *
  * syncInfo v1 is about a sequence of header ids just
  */
class ErgoSyncInfoSpecification extends ErgoPropertyTest with DecodingUtils {

  property("sync info reference parser") {
    val magic = Array(1: Byte, 0: Byte, 2: Byte, 4: Byte) // mainnet magic
    val syncSpec = ErgoSyncInfoMessageSpec

    val lastHeaderId = Array.fill(16)(1: Byte) ++ Array.fill(16)(2: Byte)

    val syncInfo = ErgoSyncInfo(Seq(ModifierId @@ Base16.encode(lastHeaderId)))

    val syncMessage = Message(syncSpec, Right(syncInfo), None)

    val ms = new MessageSerializer(Seq(syncSpec), magic)

    val bs = ms.serialize(syncMessage).toArray
    val bsString = Base16.encode(bs)

    // test vector got via high-level API
    bsString shouldBe "0100020441000000214f904163010101010101010101010101010101010102020202020202020202020202020202"

    val bb = ByteBuffer.wrap(bs)

    // simple reference parser below

    // read network magic (network id) bytes (4 bytes)
    val magicRead = getBytes(bb, 4)
    magicRead.toIndexedSeq shouldBe magic.toIndexedSeq

    // read message type id
    val messageCode = getByte(bb)
    messageCode shouldBe syncSpec.messageCode  // 65 (in dec)

    // read message length (up to 4 bytes)
    val messageLength = Ints.fromByteArray(getBytes(bb,4))

    messageLength shouldBe 33

    // read message checksum (4 bytes)
    val checkSum = getBytes(bb, 4)

    // read number of headers, up to 8 bytes, but there are app-level limits (400 in reference client)
    val headersCount = getULong(bb).toByte // should read one byte only

    headersCount shouldBe 1.toByte

    // read header ids, one-by-one
    val headerId = getBytes(bb, 32)

    headerId.toIndexedSeq shouldBe lastHeaderId.toIndexedSeq

    // checksum must be equal to first 4 bytes of message payload
    checkSum shouldBe hash.Blake2b256(headersCount +: headerId).take(4)
  }

}
