package scorex.core.network

import org.ergoplatform.network.PeerSpecSerializer
import org.ergoplatform.testkit.generators.ObjectGenerators
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.util.ByteArrayBuilder
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}

import java.nio.ByteBuffer

class PeerSpecSerializerSpec extends ErgoPropertyTest with ObjectGenerators {

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
