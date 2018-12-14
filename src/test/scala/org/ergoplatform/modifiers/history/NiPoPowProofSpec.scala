package org.ergoplatform.modifiers.history

import org.ergoplatform.settings.NiPoPowSettings
import org.ergoplatform.utils.ErgoPropertyTest

class NiPoPowProofSpec extends ErgoPropertyTest {

  val algos = new NiPoPowAlgos(NiPoPowSettings(enabled = true, 3, 6, 6, 1.45d))

  property("headers of level") {
    forAll(validNiPoPowProofGen) { proof =>
      proof.prefix.chainOfLevel(0) shouldEqual proof.prefix.chain
    }
  }

  ignore("prove chain") {
    val chain = genHeaderChain(10000)
    val proof = algos.prove(chain.headers)

    proof.validate shouldBe 'success
  }

}
