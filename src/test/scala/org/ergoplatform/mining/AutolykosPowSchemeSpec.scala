package org.ergoplatform.mining

import com.google.common.primitives.Ints
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.testkit.utils.NoShrink

class AutolykosPowSchemeSpec extends ErgoPropertyTest with NoShrink {

  property("generated solution should be valid") {
    val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
    forAll(invalidHeaderGen, Gen.choose(1, 1), Gen.choose[Byte](1, 2)) { (inHeader, difficulty, ver) =>
      val nBits = RequiredDifficulty.encodeCompactBits(difficulty)
      val h = inHeader.copy(nBits = nBits, version = ver)
      val sk = randomSecret()
      val x = randomSecret()
      val msg = pow.msgByHeader(h)
      val b = pow.getB(h.nBits)
      val hbs = Ints.toByteArray(h.height)
      val newHeader = pow.checkNonces(ver, hbs, msg, sk, x, b, 0, 1000)
        .map(s => h.copy(powSolution = s)).get
      pow.validate(newHeader) shouldBe 'success
    }
  }

}
