package org.ergoplatform.modifiers.history

import org.ergoplatform.utils.ErgoPropertyTest

class NiPoPowProofSpec extends ErgoPropertyTest {

  property("headers of level") {
    forAll(validNiPoPowProofGen) { proof =>
      proof.headersOfLevel(0) shouldEqual proof.prefix
    }
  }

}
