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
      val N = pow.calcN(h)
      val newHeader = pow.checkNonces(ver, hbs, msg, sk, x, b, N, 0, 1000)
        .map(s => h.copy(powSolution = s)).get
      pow.validate(newHeader) shouldBe 'success
    }
  }

  property("calcN test vectors") {
    // mainnet parameters
    val k = 32
    val n = 26

    val pow = new AutolykosPowScheme(k, n)

    // N is always the same in Autolykos v1
    pow.calcN(1, 700000) shouldBe pow.NBase
    pow.calcN(1, 100000) shouldBe pow.NBase
    pow.calcN(1, 70000000) shouldBe pow.NBase

    pow.calcN(2, 500000) shouldBe pow.NBase
    pow.calcN(2, 600000) shouldBe pow.NBase
    pow.calcN(2, 600 * 1024) shouldBe 70464240
    pow.calcN(2, 650 * 1024) shouldBe 73987410
    pow.calcN(2, 700000) shouldBe 73987410
    pow.calcN(2, 788400) shouldBe 81571035 // 3 years
    pow.calcN(2, 1051200) shouldBe 104107290 // 4 years
    pow.calcN(2, 9216000) shouldBe 2147387550 // max height
    pow.calcN(2, 29216000) shouldBe 2147387550
    pow.calcN(2, 292160000) shouldBe 2147387550
  }

}
