package org.ergoplatform.mining

import com.google.common.primitives.Ints
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.history.{Header, HeaderSerializer}
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256
import scorex.testkit.utils.NoShrink

class AutolykosPowSchemeSpec extends ErgoPropertyTest with NoShrink {

  property("generated solution should be valid") {
    val pow = new AutolykosPowScheme(powScheme.k, powScheme.n)
    forAll(invalidHeaderGen,
            Gen.choose(30, 50),
            Gen.choose[Byte](1, 2)) { (inHeader, difficulty, ver) =>
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

      if(ver > Header.InitialVersion) {
        // We remove last byte of "msg", perform PoW and check that it fails validation
        require(HeaderSerializer.bytesWithoutPow(h).last == 0)
        val msg2 = Blake2b256(HeaderSerializer.bytesWithoutPow(h).dropRight(1))

        val newHeader2 = pow.checkNonces(ver, hbs, msg2, sk, x, b, N, 0, 1000)
          .map(s => h.copy(powSolution = s)).get
        pow.validate(newHeader2) shouldBe 'failure
      }
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
    pow.calcN(2, 4198400) shouldBe 2143944600 // max height
    pow.calcN(2, 41984000) shouldBe 2143944600
  }

}
