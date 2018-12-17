package org.ergoplatform.modifiers.history

import org.ergoplatform.settings.NiPoPowSettings
import org.ergoplatform.utils.{ErgoPropertyTest, RealModifiers}

class NiPoPowProofSpec extends ErgoPropertyTest with RealModifiers {

  val algos = new NiPoPowAlgos(NiPoPowSettings(enabled = true, 3, 6, 6, 0.65d))

  property("headers of level") {
    forAll(validNiPoPowProofGen) { proof =>
      proof.prefix.chainOfLevel(0) shouldEqual proof.prefix.chain
    }
  }

  property("prove chain") {
    val headers = takeHeaders(6000)
    val proof = algos.prove(headers)
    proof.prefix.chain.size shouldEqual proof.prefix.chain.toSet.size
    proof.validate shouldBe 'success
  }

}
