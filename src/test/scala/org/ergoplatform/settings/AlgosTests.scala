package org.ergoplatform.settings

import org.ergoplatform.utils.ErgoPropertyTest

class AlgosTests extends ErgoPropertyTest {


  property("blockIdDifficulty should be > 0") {
    forAll(genBytes(32)) { id: Array[Byte] =>
      Algos.blockIdDifficulty(Algos.hash(id)) should be > BigInt(0)
    }
  }
}
