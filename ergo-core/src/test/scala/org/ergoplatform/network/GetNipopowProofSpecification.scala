package org.ergoplatform.network

import org.ergoplatform.network.message.{GetNipopowProofSpec, NipopowProofData}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class GetNipopowProofSpecification extends AnyPropSpec with Matchers {

  property("a request without a pad is the four bytes deployed nodes write") {
    // m = 6 and k = 10 as signed VLQ, no header id, a pad of length 0
    GetNipopowProofSpec.toBytes(NipopowProofData(6, 10, None)).toSeq shouldBe Seq[Byte](12, 20, 0, 0)
  }

  property("the parameters of a request are read the same with a pad after them") {
    val parsed = GetNipopowProofSpec.parseBytes(Array[Byte](12, 20, 0, 2, 1, 7))
    (parsed.m, parsed.k, parsed.headerId) shouldBe ((6, 10, None))
  }

  property("the pad of a request is kept, and its first byte can ask for the current tip") {
    val pad = Array[Byte](NipopowProofData.CurrentTip, 7)
    val bytes = GetNipopowProofSpec.toBytes(NipopowProofData(6, 10, None, pad))
    bytes.toSeq shouldBe Seq[Byte](12, 20, 0, 2, NipopowProofData.CurrentTip, 7)
    val parsed = GetNipopowProofSpec.parseBytes(bytes)
    parsed.pad.toSeq shouldBe pad.toSeq
    parsed.wantsCurrentTip shouldBe true
    // a request without a pad, and one whose pad begins with another byte, do not ask for it
    GetNipopowProofSpec.parseBytes(Array[Byte](12, 20, 0, 0)).wantsCurrentTip shouldBe false
    GetNipopowProofSpec.parseBytes(Array[Byte](12, 20, 0, 1, 9)).wantsCurrentTip shouldBe false
  }
}
