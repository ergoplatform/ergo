package org.ergoplatform.serialization

import org.ergoplatform.modifiers.history.popow.{NipopowProof, NipopowProofSerializer}
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

  private def smallValidProof(): NipopowProof = validNiPoPowProofGen(1, 1).sample.get

  private def uintBytes(value: Long): Array[Byte] =
    (new VLQByteStringWriter).putUInt(value).toBytes

  private def replaceUInt(bytes: Array[Byte], offset: Int, original: Int, replacement: Long): Array[Byte] = {
    val originalLength = uintBytes(original.toLong).length
    bytes.take(offset) ++ uintBytes(replacement) ++ bytes.drop(offset + originalLength)
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

}
