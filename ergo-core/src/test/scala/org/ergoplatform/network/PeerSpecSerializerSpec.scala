package org.ergoplatform.network

import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.ByteArrayBuilder
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}

import java.nio.ByteBuffer

class PeerSpecSerializerSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.CoreObjectGenerators._

  property("All variants of peer spec should be serialized and deserialized successfully") {
    forAll(peerSpecGen) { peerSpec =>
      val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
      PeerSpecSerializer.serialize(peerSpec, writer)
      val reader = new VLQByteBufferReader(ByteBuffer.wrap(writer.result().toBytes))
      val actualPeerSpec = PeerSpecSerializer.parse(reader)
      peerSpec shouldBe actualPeerSpec
    }
  }
}
