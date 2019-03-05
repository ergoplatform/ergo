package org.ergoplatform.mining

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.testkit.utils.NoShrink

class AutolykosPowSchemeSpec extends ErgoPropertyTest with NoShrink {

  property("generated solution should be valid") {
    val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
    forAll(invalidHeaderGen, Gen.choose(1, 20)) { (inHeader, difficulty) =>
      val nBits = RequiredDifficulty.encodeCompactBits(difficulty)
      val h = inHeader.copy(nBits = nBits)
      pow.validate(h) shouldBe 'failure
      val sk = randomSecret()
      val x = randomSecret()
      val msg = pow.msgByHeader(h)
      val b = pow.getB(h.nBits)
      val newHeader = pow.checkNonces(msg, sk, x, b, 0, 1000).map(s => h.copy(powSolution = s)).get
      pow.validate(newHeader) shouldBe 'success
    }
  }

}
