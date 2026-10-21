package org.ergoplatform.network

import akka.util.ByteString
import org.ergoplatform.modifiers.{BlockTransactionsTypeId, ProofsTypeId}
import org.ergoplatform.network.peer.LargeAdProofsPeerFeatureSerializer
import org.ergoplatform.network.message.{Message, MessageConstants, MessageSerializer, ModifiersData, ModifiersSpec}
import org.ergoplatform.network.peer.LargeAdProofsPeerFeature
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.serialization.{VLQByteBufferReader, VLQByteBufferWriter}
import scorex.util.{ByteArrayBuilder, ModifierId, bytesToId, idToBytes}

/**
  * Modifiers messages with a single AD proofs section bigger than `ModifiersSpec.maxMsgSizeWithReserve`
  * (mainnet blocks 1,211,757 and 1,211,759 have AD proofs of ~8.8 MB).
  */
class LargeAdProofsMessageSpecification extends ErgoCorePropertyTest {

  private val Overhead = 5 + 32 + 4 // as estimated by ModifiersSpec: header + id + length field

  private def id(b: Byte): ModifierId = bytesToId(Array.fill(32)(b))

  private def roundTrip(data: ModifiersData) = ModifiersSpec.parseBytesTry(ModifiersSpec.toBytes(data))

  private def sameBytes(a: Array[Byte], b: Array[Byte]): Boolean = java.util.Arrays.equals(a, b)

  property("single AD proofs section above the reserve limit round-trips (mainnet-size proof)") {
    val bytes = Array.fill(8766629)(7: Byte)
    val parsed = roundTrip(ModifiersData(ProofsTypeId.value, Map(id(1) -> bytes))).get
    parsed.typeId shouldBe ProofsTypeId.value
    parsed.modifiers.size shouldBe 1
    sameBytes(parsed.modifiers(id(1)), bytes) shouldBe true
  }

  property("largest single AD proofs section fits the outer frame and passes MessageSerializer") {
    val bytes = Array.fill(ModifiersSpec.maxSingleAdProofsMsgSize - Overhead)(3: Byte)
    val payload = ModifiersSpec.toBytes(ModifiersData(ProofsTypeId.value, Map(id(2) -> bytes)))
    payload.length should be <= MessageConstants.MaxMessageSize

    val magic = Array(1: Byte, 0: Byte, 2: Byte, 4: Byte)
    val ms = new MessageSerializer(Seq(ModifiersSpec), magic)
    val wire = ms.serialize(Message(ModifiersSpec, Right(ModifiersData(ProofsTypeId.value, Map(id(2) -> bytes))), None))
    val msg = ms.deserialize(ByteString(wire.toArray), None).get.get
    val data = msg.input match {
      case Left(raw) => ModifiersSpec.parseBytes(raw)
      case Right(_) => fail("expected raw message bytes")
    }
    sameBytes(data.modifiers(id(2)), bytes) shouldBe true
  }

  property("AD proofs section above the single limit is refused by the serializer (written with count 0)") {
    val bytes = Array.fill(ModifiersSpec.maxSingleAdProofsMsgSize - Overhead + 1)(3: Byte)
    roundTrip(ModifiersData(ProofsTypeId.value, Map(id(3) -> bytes))).isFailure shouldBe true // count = 0
  }

  property("other block sections keep the reserve limit (oversized one written with count 0)") {
    val bytes = Array.fill(ModifiersSpec.maxMsgSizeWithReserve)(5: Byte)
    roundTrip(ModifiersData(BlockTransactionsTypeId.value, Map(id(4) -> bytes))).isFailure shouldBe true // count = 0
  }

  property("serializer and parser agree at the reserve boundary") {
    val fits = Array.fill(ModifiersSpec.maxMsgSizeWithReserve - Overhead)(9: Byte)
    roundTrip(ModifiersData(BlockTransactionsTypeId.value, Map(id(5) -> fits))).get.modifiers.size shouldBe 1

    val tooBig = Array.fill(ModifiersSpec.maxMsgSizeWithReserve - Overhead + 1)(9: Byte)
    roundTrip(ModifiersData(BlockTransactionsTypeId.value, Map(id(5) -> tooBig))).isFailure shouldBe true
  }

  property("two AD proofs sections keep the reserve limit") {
    val half = Array.fill(5 * 1024 * 1024)(1: Byte)
    val parsed = roundTrip(ModifiersData(ProofsTypeId.value, Map(id(6) -> half, id(7) -> half))).get
    parsed.modifiers.size shouldBe 1 // the second one does not fit and is left out
  }

  private def crafted(typeId: Byte, entries: Seq[(ModifierId, Int, Int)]): Array[Byte] = {
    // entries: (id, declared length, actual bytes written)
    val w = new VLQByteBufferWriter(new ByteArrayBuilder())
    w.put(typeId)
    w.putUInt(entries.size)
    entries.foreach { case (mid, declared, actual) =>
      w.putBytes(idToBytes(mid))
      w.putUInt(declared)
      w.putBytes(Array.fill(actual)(0: Byte))
    }
    w.result().toBytes
  }

  property("parser rejects an oversized single AD proofs section before reading its bytes") {
    val msg = crafted(ProofsTypeId.value, Seq((id(8), ModifiersSpec.maxSingleAdProofsMsgSize, 0)))
    val res = ModifiersSpec.parseBytesTry(msg)
    res.isFailure shouldBe true
    res.failed.get.getMessage should include("Too big message")
  }

  property("parser applies the reserve limit to multi-modifier AD proofs messages") {
    val first = 5 * 1024 * 1024
    val msg = crafted(ProofsTypeId.value, Seq((id(9), first, first), (id(10), first, 0)))
    val res = ModifiersSpec.parseBytesTry(msg)
    res.isFailure shouldBe true
    res.failed.get.getMessage should include("Too big message")
  }

  // --- sending plan (what modifiersReq actually sends) ---

  private def mods(sizes: Int*): Seq[(ModifierId, Array[Byte])] =
    sizes.zipWithIndex.map { case (s, i) => id((20 + i).toByte) -> Array.fill(s)(1: Byte) }

  private val Large = 8766629 // AD proofs of mainnet block 1,211,757

  property("small modifiers are batched below maxMessageSize, in order") {
    val (batches, skipped) = ModifiersSpec.planBatches(BlockTransactionsTypeId.value, mods(600000, 600000, 600000, 600000), peerAcceptsLargeAdProofs = false)
    skipped shouldBe empty
    batches.map(_.size) shouldBe Seq(3, 1)
    batches.flatten.map(_._1) shouldBe mods(600000, 600000, 600000, 600000).map(_._1)
  }

  property("a modifier between maxMessageSize and the reserve limit is still sent alone, to any peer") {
    val (batches, skipped) = ModifiersSpec.planBatches(BlockTransactionsTypeId.value, mods(5000000), peerAcceptsLargeAdProofs = false)
    skipped shouldBe empty
    batches.map(_.size) shouldBe Seq(1)
  }

  property("a large AD proofs section is sent only to a peer accepting it, otherwise skipped") {
    val (sent, notSkipped) = ModifiersSpec.planBatches(ProofsTypeId.value, mods(Large), peerAcceptsLargeAdProofs = true)
    sent.map(_.size) shouldBe Seq(1)
    notSkipped shouldBe empty

    val (none, skipped) = ModifiersSpec.planBatches(ProofsTypeId.value, mods(Large), peerAcceptsLargeAdProofs = false)
    none shouldBe empty
    skipped.map(_._2) shouldBe Seq(Large)
  }

  property("a large section of another type is skipped even for a peer accepting large AD proofs") {
    val (batches, skipped) = ModifiersSpec.planBatches(BlockTransactionsTypeId.value, mods(Large), peerAcceptsLargeAdProofs = true)
    batches shouldBe empty
    skipped should have size 1
  }

  property("AD proofs sending plan at the single-message limit") {
    val max = ModifiersSpec.maxSingleAdProofsMsgSize - Overhead
    ModifiersSpec.planBatches(ProofsTypeId.value, mods(max), peerAcceptsLargeAdProofs = true)._1 should have size 1
    ModifiersSpec.planBatches(ProofsTypeId.value, mods(max + 1), peerAcceptsLargeAdProofs = true)._2 should have size 1
  }

  property("a skipped modifier doesn't block the ones after it") {
    val (batches, skipped) = ModifiersSpec.planBatches(ProofsTypeId.value, mods(Large, 1000, Large), peerAcceptsLargeAdProofs = false)
    skipped.map(_._2) shouldBe Seq(Large, Large)
    batches.flatten.map(_._2.length) shouldBe Seq(1000)
  }

  // --- handshake capability ---

  property("PeerSpec parsing skips unknown features (what an older node does with LargeAdProofsPeerFeature)") {
    // PeerSpec: agentName, version (3 bytes), nodeName, no declared address, features
    val w = new VLQByteBufferWriter(new ByteArrayBuilder())
    w.putShortString("ergoref")
    w.put(6: Byte); w.put(0: Byte); w.put(8: Byte)
    w.putShortString("n")
    w.putOption[Int](None)((_, _) => ())
    w.put(2: Byte) // two features: an unknown one (id 99, 3-byte payload) and LargeAdProofsPeerFeature (id 5, empty)
    w.put(99: Byte); w.putUShort(3); w.putBytes(Array[Byte](1, 2, 3))
    w.put(5: Byte); w.putUShort(0)
    val parsed = PeerSpecSerializer.parseBytes(w.result().toBytes)
    parsed.features should have size 1
    LargeAdProofsFilter.condition(parsed) shouldBe true
  }

  property("LargeAdProofsPeerFeature with a non-empty payload is not accepted") {
    val r = new VLQByteBufferReader(java.nio.ByteBuffer.wrap(Array[Byte](0)))
    LargeAdProofsPeerFeatureSerializer.parseTry(r).isFailure shouldBe true
    LargeAdProofsPeerFeatureSerializer.parseBytesTry(Array.emptyByteArray).isSuccess shouldBe true
  }

  property("large AD proofs are only sent to peers announcing LargeAdProofsPeerFeature") {
    def spec(features: Seq[PeerFeature]) = PeerSpec("ergoref", Version(6, 1, 6), "n", None, features)
    LargeAdProofsFilter.condition(spec(Seq.empty)) shouldBe false
    LargeAdProofsFilter.condition(spec(Seq(LargeAdProofsPeerFeature()))) shouldBe true
  }

  property("LargeAdProofsPeerFeature survives handshake serialization") {
    val ps = PeerSpec("ergoref", Version(6, 0, 8), "n", None, Seq(LargeAdProofsPeerFeature()))
    val parsed = PeerSpecSerializer.parseBytes(PeerSpecSerializer.toBytes(ps))
    LargeAdProofsFilter.condition(parsed) shouldBe true
    parsed.features.collect { case f: LargeAdProofsPeerFeature => f } should have size 1
  }

}
