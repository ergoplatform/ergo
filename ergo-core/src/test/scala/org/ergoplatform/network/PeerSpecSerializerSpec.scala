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

  property("SyncV3PeerFeature roundtrips in Handshake and is safely ignored by legacy feature maps") {
    forAll(modePeerFeatureGen, appVersionGen) { (modeFeature, version) =>
      val specWithV3 = PeerSpec(
        "ergoref",
        version,
        "ergo-node",
        None,
        Seq(modeFeature, SyncV3PeerFeature())
      )
      val hs = Handshake(specWithV3, 1700000000000L)
      val hsBytes = HandshakeSerializer.toBytes(hs)
      val parsedHs = HandshakeSerializer.parseBytes(hsBytes)
      parsedHs.peerSpec.features should contain(SyncV3PeerFeature())
      parsedHs.peerSpec.features should contain(modeFeature)

      // Simulate a legacy node (e.g. v6.0.11 / v6.1.7 / weak-blocks) that does not know SyncV3FeatureId:
      // unknown feature IDs in PeerSpec must be skipped without throwing or corrupting remaining features.
      val writer = new VLQByteBufferWriter(new ByteArrayBuilder())
      writer.putShortString(specWithV3.agentName)
      ApplicationVersionSerializer.serialize(specWithV3.protocolVersion, writer)
      writer.putShortString(specWithV3.nodeName)
      writer.putOption(specWithV3.declaredAddress)((_, _) => ())
      writer.put(3: Byte)
      // 1) SyncV3PeerFeature (0-byte payload)
      writer.put(SyncV3PeerFeature().featureId)
      writer.putUShort(0)
      // 2) Unknown future feature ID with non-empty payload
      writer.put(99: Byte)
      writer.putUShort(4)
      writer.putBytes(Array[Byte](1, 2, 3, 4))
      // 3) Known ModePeerFeature after unknown features
      writer.put(modeFeature.featureId)
      val mfBytes = modeFeature.bytes
      writer.putUShort(mfBytes.length)
      writer.putBytes(mfBytes)

      val reader = new VLQByteBufferReader(ByteBuffer.wrap(writer.result().toBytes))
      val parsedWithUnknown = PeerSpecSerializer.parse(reader)
      parsedWithUnknown.features should contain(SyncV3PeerFeature())
      parsedWithUnknown.features should contain(modeFeature)
      parsedWithUnknown.features.size shouldBe 2
    }
  }
}
