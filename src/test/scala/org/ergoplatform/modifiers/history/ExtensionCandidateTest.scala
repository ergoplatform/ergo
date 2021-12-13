package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.utils.ErgoPropertyTest

class ExtensionCandidateTest extends ErgoPropertyTest {
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
    forAll { fields: Seq[KV] =>

      val ext = ExtensionCandidate(fields)
      val proof = ext.batchProofFor(fields.flatMap(_._1.clone).toArray)
      proof shouldBe defined
      proof.get.valid(ext.digest) shouldBe true
    }
  }
}
