package org.ergoplatform.serialization

import org.ergoplatform.modifiers.history.popow.{NipopowProof, NipopowProofSerializer, PoPowHeaderSerializer, PoPowParams}
import org.ergoplatform.network.ErgoNodeViewSynchronizer
import org.ergoplatform.nodeView.wallet.persistence.WalletDigestSerializer
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoCoreTestConstants.nipopowAlgos
import org.ergoplatform.utils.generators.ErgoNodeGenerators.{poPowProofGen, validNiPoPowProofGen}
import scorex.util.serialization.VLQByteStringWriter

class SerializationTests extends ErgoCorePropertyTest with org.ergoplatform.utils.SerializationTests {
  import org.ergoplatform.utils.generators.ErgoNodeWalletGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  private val nipopowSerializer = new NipopowProofSerializer(nipopowAlgos)

  private sealed trait FrameSite
  private case object PrefixFrame extends FrameSite
  private case object SuffixHeadFrame extends FrameSite
  private case object SuffixTailFrame extends FrameSite

  private val MaxProofElements = PoPowParams.MaxProofElements
  private val MaxHeaderFrameBytes = PoPowHeaderSerializer.MaxHeaderFrameBytes
  private val MaxPoPowHeaderFrameBytes = PoPowHeaderSerializer.MaxSerializedBytes

  private def smallValidProof(): NipopowProof = validNiPoPowProofGen(1, 1).sample.get

  private lazy val framedProof: NipopowProof = {
    val proof = validNiPoPowProofGen(1, 2).sample.get
    require(proof.prefix.nonEmpty, "framing fixture needs a prefix element")
    require(proof.suffixTail.nonEmpty, "framing fixture needs a suffix-tail element")
    proof
  }

  private def uintBytes(value: Long): Array[Byte] =
    (new VLQByteStringWriter).putUInt(value).toBytes

  private def replaceUInt(bytes: Array[Byte], offset: Int, original: Int, replacement: Long): Array[Byte] = {
    val originalLength = uintBytes(original.toLong).length
    bytes.take(offset) ++ uintBytes(replacement) ++ bytes.drop(offset + originalLength)
  }

  private def writerBytes(write: VLQByteStringWriter => Unit): Array[Byte] = {
    val writer = new VLQByteStringWriter
    write(writer)
    writer.toBytes
  }

  private def putFrame(writer: VLQByteStringWriter,
                       bytes: Array[Byte],
                       mutate: Boolean,
                       declaredDelta: Int,
                       fillerLength: Int): Unit = {
    val declaredSize = bytes.length + (if (mutate) declaredDelta else 0)
    require(declaredSize >= 0)
    writer.putUInt(declaredSize.toLong)
    writer.putBytes(bytes)
    if (mutate && fillerLength > 0) {
      writer.putBytes(Array.fill(fillerLength)(0x7f.toByte))
    }
  }

  private def serializeWithFrameMutation(proof: NipopowProof,
                                         site: FrameSite,
                                         declaredDelta: Int,
                                         fillerLength: Int): Array[Byte] = writerBytes { writer =>
    writer.putUInt(proof.m.toLong)
    writer.putUInt(proof.k.toLong)
    writer.putUInt(proof.prefix.length.toLong)
    proof.prefix.zipWithIndex.foreach { case (header, index) =>
      putFrame(writer, header.bytes, site == PrefixFrame && index == 0, declaredDelta, fillerLength)
    }
    putFrame(writer, proof.suffixHead.bytes, site == SuffixHeadFrame, declaredDelta, fillerLength)
    writer.putUInt(proof.suffixTail.length.toLong)
    proof.suffixTail.zipWithIndex.foreach { case (header, index) =>
      putFrame(writer, header.bytes, site == SuffixTailFrame && index == 0, declaredDelta, fillerLength)
    }
    writer.put(if (proof.continuous) 1 else 0)
  }

  private def assertProofParseFailureContains(bytes: Array[Byte], expected: String): Unit = {
    val failure = nipopowSerializer.parseBytesTry(bytes).failed.get
    failure.toString should include(expected)
  }

  property("Serializers should be defined for all block sections") {
    val block = invalidErgoFullBlockGen.sample.get
    block.toSeq.foreach { s =>
      ErgoNodeViewSynchronizer.modifierSerializers.get(s.modifierTypeId) should not be None
    }
  }

  property("WalletDigest serialization") {
    forAll(registrySummaryGen) { index =>
      WalletDigestSerializer.parseBytes(WalletDigestSerializer.toBytes(index)) shouldEqual index
    }
  }

  property("PoPowProof serialization") {
    checkSerializationRoundtrip(poPowProofGen, nipopowSerializer)
  }

  property("PoPowProof parser rejects invalid m") {
    val proof = smallValidProof()
    val bytes = nipopowSerializer.toBytes(proof)
    Seq(0L, 20001L).foreach { invalidM =>
      withClue(s"m=$invalidM") {
        val mutated = replaceUInt(bytes, offset = 0, proof.m, invalidM)
        nipopowSerializer.parseBytesTry(mutated) shouldBe 'failure
      }
    }
  }

  property("PoPowProof parser rejects invalid k") {
    val proof = smallValidProof()
    val bytes = nipopowSerializer.toBytes(proof)
    val kOffset = uintBytes(proof.m.toLong).length
    Seq(0L, 20001L).foreach { invalidK =>
      withClue(s"k=$invalidK") {
        val mutated = replaceUInt(bytes, kOffset, proof.k, invalidK)
        nipopowSerializer.parseBytesTry(mutated) shouldBe 'failure
      }
    }
  }

  property("PoPowProof parser rejects suffix length different from k") {
    val proof = smallValidProof()
    val bytes = nipopowSerializer.toBytes(proof)
    val kOffset = uintBytes(proof.m.toLong).length
    val mutated = replaceUInt(bytes, kOffset, proof.k, proof.k + 1L)

    nipopowSerializer.parseBytesTry(mutated) shouldBe 'failure
  }

  property("PoPowProof parser rejects an invalid continuous mode byte") {
    val bytes = nipopowSerializer.toBytes(smallValidProof())
    bytes(bytes.length - 1) = 2

    assertProofParseFailureContains(bytes, "continuous mode")
  }

  property("PoPowProof outer element frames preserve authoritative slicing") {
    Seq(PrefixFrame, SuffixHeadFrame, SuffixTailFrame).foreach { site =>
      withClue(s"site=$site canonical") {
        nipopowSerializer.parseBytes(serializeWithFrameMutation(framedProof, site, 0, 0)) shouldBe framedProof
      }
      withClue(s"site=$site under-declared") {
        nipopowSerializer.parseBytesTry(serializeWithFrameMutation(framedProof, site, -1, 0)) shouldBe 'failure
      }
      withClue(s"site=$site over-declared without filler") {
        nipopowSerializer.parseBytesTry(serializeWithFrameMutation(framedProof, site, 1, 0)) shouldBe 'failure
      }
      withClue(s"site=$site over-declared with matching filler") {
        nipopowSerializer.parseBytes(serializeWithFrameMutation(framedProof, site, 1, 1)) shouldBe framedProof
      }
    }
  }

  property("PoPowProof rejects an oversized prefix count before iterating") {
    val bytes = writerBytes { writer =>
      writer.putUInt(1)
      writer.putUInt(1)
      writer.putUInt(MaxProofElements + 1L)
    }

    assertProofParseFailureContains(bytes, "prefix count")
  }

  property("PoPowProof rejects an oversized suffix count before iterating") {
    val proof = smallValidProof()
    val bytes = writerBytes { writer =>
      writer.putUInt(proof.m.toLong)
      writer.putUInt(proof.k.toLong)
      writer.putUInt(proof.prefix.length.toLong)
      proof.prefix.foreach(header => putFrame(writer, header.bytes, false, 0, 0))
      putFrame(writer, proof.suffixHead.bytes, false, 0, 0)
      writer.putUInt(MaxProofElements + 1L)
    }

    assertProofParseFailureContains(bytes, "suffix count")
  }

  property("PoPowProof rejects oversized outer frames before reading them") {
    val prefixBytes = writerBytes { writer =>
      writer.putUInt(1)
      writer.putUInt(1)
      writer.putUInt(1)
      writer.putUInt(MaxPoPowHeaderFrameBytes + 1L)
    }
    val suffixHeadBytes = writerBytes { writer =>
      writer.putUInt(1)
      writer.putUInt(1)
      writer.putUInt(0)
      writer.putUInt(MaxPoPowHeaderFrameBytes + 1L)
    }
    val suffixTailBytes = writerBytes { writer =>
      writer.putUInt(1)
      writer.putUInt(2)
      writer.putUInt(0)
      putFrame(writer, framedProof.suffixHead.bytes, false, 0, 0)
      writer.putUInt(1)
      writer.putUInt(MaxHeaderFrameBytes + 1L)
    }

    assertProofParseFailureContains(prefixBytes, "prefix element frame size")
    assertProofParseFailureContains(suffixHeadBytes, "suffix-head frame size")
    assertProofParseFailureContains(suffixTailBytes, "suffix-tail frame size")
  }

}
