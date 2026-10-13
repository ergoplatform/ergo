package org.ergoplatform.modifiers.history

import java.nio.charset.StandardCharsets

import org.ergoplatform.modifiers.history.extension.{Extension, ExtensionCandidate}
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
        val proof = ext.batchProofFor(fields.map(_._1.clone).toArray: _*)
        proof shouldBe defined
        proof.get.valid(ext.interlinksDigest) shouldBe true
      }
    }
  }

  property("batchProofFor should return None for a empty fields") {
    val fields: Seq[KV] = Seq.empty
    val ext = ExtensionCandidate(fields)
    val proof = ext.batchProofFor(fields.map(_._1.clone).toArray: _*)
    proof shouldBe None
  }

  property("nodeVersionField should encode the node version under key 0x03/0x00") {
    forAll { version: String =>
      val (key, value) = Extension.nodeVersionField(version)
      key shouldBe Array(0x03.toByte, 0x00.toByte)
      key.lengthCompare(Extension.FieldKeySize) shouldBe 0
      value.lengthCompare(Extension.FieldValueMaxSize) should be <= 0
      value shouldBe version.getBytes(StandardCharsets.UTF_8).take(Extension.FieldValueMaxSize)
    }
  }

  property("nodeVersionField should not collide with reserved key spaces") {
    val (key, _) = Extension.nodeVersionField("6.0.6")
    key.head should not be Extension.SystemParametersPrefix
    key.head should not be Extension.InterlinksVectorPrefix
    key.head should not be Extension.ValidationRulesPrefix
  }
}
