package scorex.core.network

import org.ergoplatform.utils.ErgoPropertyTest
import scorex.testkit.generators.ObjectGenerators
import scorex.util.ByteArrayBuilder
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}

import java.nio.ByteBuffer

class PeerSpecSerializerSpec extends ErgoPropertyTest with ObjectGenerators {

  private val peerSpecSerializer =
    new PeerSpecSerializer(Map(FullNodePeerFeature.featureId -> FullNodePeerFeature.serializer))

  property("All variants of peer spec should be serialized and deserialized successfully") {
    forAll(peerSpecGen) { peerSpec =>
      val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
      peerSpecSerializer.serialize(peerSpec, writer)
      val reader = new VLQByteBufferReader(ByteBuffer.wrap(writer.result().toBytes))
      val actualPeerSpec = peerSpecSerializer.parse(reader)
      peerSpec shouldBe actualPeerSpec
    }
  }
}
