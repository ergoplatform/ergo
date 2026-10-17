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

  private sealed trait LengthPrefixedSite
  private case object PrefixElement extends LengthPrefixedSite
  private case object SuffixHeadElement extends LengthPrefixedSite
  private case object SuffixTailElement extends LengthPrefixedSite

  private val MaxProofElements = PoPowParams.MaxProofElements
  private val MaxHeaderBytes = PoPowHeaderSerializer.MaxHeaderBytes
  private val MaxPoPowHeaderBytes = PoPowHeaderSerializer.MaxSerializedBytes

  private def smallValidProof(): NipopowProof = validNiPoPowProofGen(1, 1).sample.get

  private lazy val sampleProof: NipopowProof = {
    val proof = validNiPoPowProofGen(1, 2).sample.get
    require(proof.prefix.nonEmpty, "length-prefix fixture needs a prefix element")
    require(proof.suffixTail.nonEmpty, "length-prefix fixture needs a suffix-tail element")
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

  private def putLengthPrefixed(writer: VLQByteStringWriter,
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

  private def serializeWithDeclaredLengthMutation(proof: NipopowProof,
                                                   site: LengthPrefixedSite,
                                                   declaredDelta: Int,
                                                   fillerLength: Int): Array[Byte] = writerBytes { writer =>
    writer.putUInt(proof.m.toLong)
    writer.putUInt(proof.k.toLong)
    writer.putUInt(proof.prefix.length.toLong)
    proof.prefix.zipWithIndex.foreach { case (header, index) =>
      putLengthPrefixed(
        writer, header.bytes, site == PrefixElement && index == 0, declaredDelta, fillerLength)
    }
    putLengthPrefixed(
      writer, proof.suffixHead.bytes, site == SuffixHeadElement, declaredDelta, fillerLength)
    writer.putUInt(proof.suffixTail.length.toLong)
    proof.suffixTail.zipWithIndex.foreach { case (header, index) =>
      putLengthPrefixed(
        writer, header.bytes, site == SuffixTailElement && index == 0, declaredDelta, fillerLength)
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

  property("PoPowProof declared element lengths define outer slicing") {
    Seq(PrefixElement, SuffixHeadElement, SuffixTailElement).foreach { site =>
      withClue(s"site=$site canonical") {
        nipopowSerializer.parseBytes(
          serializeWithDeclaredLengthMutation(sampleProof, site, 0, 0)) shouldBe sampleProof
      }
      withClue(s"site=$site under-declared") {
        nipopowSerializer.parseBytesTry(
          serializeWithDeclaredLengthMutation(sampleProof, site, -1, 0)) shouldBe 'failure
      }
      withClue(s"site=$site over-declared without filler") {
        nipopowSerializer.parseBytesTry(
          serializeWithDeclaredLengthMutation(sampleProof, site, 1, 0)) shouldBe 'failure
      }
      withClue(s"site=$site over-declared with matching filler") {
        nipopowSerializer.parseBytes(
          serializeWithDeclaredLengthMutation(sampleProof, site, 1, 1)) shouldBe sampleProof
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
      proof.prefix.foreach(header => putLengthPrefixed(writer, header.bytes, false, 0, 0))
      putLengthPrefixed(writer, proof.suffixHead.bytes, false, 0, 0)
      writer.putUInt(MaxProofElements + 1L)
    }

    assertProofParseFailureContains(bytes, "suffix count")
  }

  property("PoPowProof rejects oversized length-prefixed elements before reading payloads") {
    val prefixBytes = writerBytes { writer =>
      writer.putUInt(1)
      writer.putUInt(1)
      writer.putUInt(1)
      writer.putUInt(MaxPoPowHeaderBytes + 1L)
    }
    val suffixHeadBytes = writerBytes { writer =>
      writer.putUInt(1)
      writer.putUInt(1)
      writer.putUInt(0)
      writer.putUInt(MaxPoPowHeaderBytes + 1L)
    }
    val suffixTailBytes = writerBytes { writer =>
      writer.putUInt(1)
      writer.putUInt(2)
      writer.putUInt(0)
      putLengthPrefixed(writer, sampleProof.suffixHead.bytes, false, 0, 0)
      writer.putUInt(1)
      writer.putUInt(MaxHeaderBytes + 1L)
    }

    assertProofParseFailureContains(prefixBytes, "prefix element length")
    assertProofParseFailureContains(suffixHeadBytes, "suffix head length")
    assertProofParseFailureContains(suffixTailBytes, "suffix tail length")
  }

}
