package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen

class ExtensionCandidateTest extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.generators.CoreObjectGenerators.modifierIdGen

  type KV = (Array[Byte], Array[Byte])

  property("proofFor should return a valid proof for an existing value") {
    forAll { explodedFields: (Seq[KV], KV, Seq[KV]) =>
      val (left, middle, right) = explodedFields
      val fields = left ++ (middle +: right)

      val ext = ExtensionCandidate(fields)
      val proof = ext.proofFor(middle._1.clone)
      proof shouldBe defined
      val nakedLeaf = proof.get.leafData
      val numBytesKey = nakedLeaf.head
      val key = nakedLeaf.tail.take(numBytesKey)
      key shouldBe middle._1
      proof.get.valid(ext.digest) shouldBe true
    }
  }

  property("batchProofFor should return a valid proof for a set of existing values") {
    val modifierIds = Gen.listOf(modifierIdGen)
    forAll(modifierIds) { modifiers =>
      whenever(modifiers.nonEmpty) {

        val fields = NipopowAlgos.packInterlinks(modifiers)
        val ext = ExtensionCandidate(fields)
        val proofOpt = ext.batchProofForInterlinks(fields.map(_._1.clone).toArray: _*)
        proofOpt shouldBe defined
        proofOpt.get.valid(ext.interlinksDigest) shouldBe true
      }
    }
  }

  property("batchProofFor should return None for a empty fields") {
    val fields: Seq[KV] = Seq.empty
    val ext = ExtensionCandidate(fields)
    val proof = ext.batchProofForInterlinks(fields.map(_._1.clone).toArray: _*)
    proof shouldBe None
  }
}
