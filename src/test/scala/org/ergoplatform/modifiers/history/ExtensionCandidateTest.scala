package org.ergoplatform.modifiers.history

import org.ergoplatform.utils.ErgoPropertyTest

class ExtensionCandidateTest extends ErgoPropertyTest {
  type KV = (Array[Byte], Array[Byte])

  property("proofFor should return a proof for an existing value") {
    forAll { explodedFields: (Seq[KV], KV, Seq[KV]) =>
      val (left, middle, right) = explodedFields
      val fields = left ++ (middle +: right)

      val ext = ExtensionCandidate(fields)
      val pf = ext.proofFor(middle._1.clone)
      pf shouldBe defined
      pf.get.valid(ext.digest) shouldBe true
    }
  }
}
