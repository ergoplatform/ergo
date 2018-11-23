package org.ergoplatform.mining

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history.HeaderSerializer
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.testkit.utils.NoShrink

class AutolykosPowSchemeSpec extends ErgoPropertyTest with NoShrink {

  val k = 21
  val N = 134217728

  property("generate valid solution for small difficulty") {
    val pow = new AutolykosPowScheme(k, N)
    forAll(invalidHeaderGen, Gen.choose(1, 20)) { (inHeader, difficulty) =>
      val nBits = RequiredDifficulty.encodeCompactBits(difficulty)
      val h = inHeader.copy(nBits = nBits)
      pow.validate(h) shouldBe 'failure
      val sk = randomSecret()
      val msg = pow.msgByHeader(h)
      val b = pow.getB(h.nBits)
      pow.initializeIfNeeded(msg, sk, b)
      val newHeader = pow.checkNonces(msg, sk, b, 0, 1000).map(s => h.copy(powSolution = s)).get
      pow.validate(newHeader) shouldBe 'success
    }
  }

  property("generate valid solution for big difficulty") {
    val pow = new AutolykosPowScheme(k, k + 1)
    forAll(invalidHeaderGen, Gen.choose(k + 1, 1000)) { (inHeader, difficulty) =>
      val nBits = RequiredDifficulty.encodeCompactBits(difficulty)
      val h = inHeader.copy(nBits = nBits)
      pow.validate(h) shouldBe 'failure
      val sk = randomSecret()
      val msg = pow.msgByHeader(h)
      val b = pow.getB(h.nBits)
      pow.initializeIfNeeded(msg, sk, b)
      val newHeader = pow.checkNonces(msg, sk, b, 0, Int.MaxValue).map(s => h.copy(powSolution = s)).get
      pow.validate(newHeader) shouldBe 'success
    }
  }

}
