package org.ergoplatform.modifiers.history

import org.ergoplatform.utils.ErgoPropertyTest

class NiPoPowProofSpec extends ErgoPropertyTest {

  property("headers of level") {
    forAll(validNiPoPowProofGen) { proof =>
      proof.prefix.chainOfLevel(0) shouldEqual proof.prefix.chain
    }
  }

}
