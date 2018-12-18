package org.ergoplatform.modifiers.history

import org.ergoplatform.settings.NiPoPowSettings
import org.ergoplatform.utils.{ErgoPropertyTest, RealModifiers}

class NiPoPowProofSpec extends ErgoPropertyTest with RealModifiers {

  val chainLength = 6000
  val algos = new NiPoPowAlgos(NiPoPowSettings(enabled = true, 3, 6, 6, 0.45d))
  lazy val headersChain: Seq[Header] = takeHeaders(chainLength)

  property("headers of level") {
    forAll(validNiPoPowProofGen) { proof =>
      proof.prefix.chainOfLevel(0) shouldEqual proof.prefix.chain
    }
  }

  property("prove chain") {
    val proof = algos.prove(headersChain)
    proof.prefix.chain.size shouldEqual proof.prefix.chain.toSet.size
    proof.validate shouldBe 'success
  }

  property("equal proofs comparison") {
    val prefix0 = algos.prove(headersChain).prefix
    val prefix1 = prefix0.copy()
    prefix0.isBetterThan(prefix1) shouldBe false
  }

  property("proofs comparison") {
    val prefix0 = algos.prove(headersChain).prefix
    val smallerChain = takeHeaders(chainLength / 2)
    val prefix1 = algos.prove(smallerChain).prefix
    prefix0.isBetterThan(prefix1) shouldBe true
  }

}
