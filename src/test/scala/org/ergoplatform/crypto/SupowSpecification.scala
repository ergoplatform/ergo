package org.ergoplatform.crypto

import org.ergoplatform.crypto.pow.{DefaultSuHash, Supow, WagnerAlg}
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen
import scorex.crypto.hash.Blake2b256

import scala.util.Random

class SupowSpecification extends ErgoPropertyTest {

  property("Sum to 0") {
    val p = 9973
    val k = 2
    val alg = new WagnerAlg(k, p)
    val finalH = 0
    forAll(Gen.choose(0, 50)) { size: Int =>
      val rnd = new Random(size)
      val initialMap = (0 until size).map(i => BigInt(rnd.nextLong()).mod(p))
      val sols = alg.filterAndProve(initialMap, finalH)
      sols.foreach(sol => alg.validate(sol, finalH) shouldBe 'success)
      sols.foreach(sol => sol.numbers.map(_._2).sum.mod(p) shouldBe finalH)
      sols.foreach(sol => !sols.exists(_.numbers sameElements sol.numbers))
    }
  }

  property("Sum to interval") {
    val p = 9973
    val k = 2
    val alg = new WagnerAlg(k, p)
    val finalH = 30

    forAll(Gen.choose(0, 40)) { size: Int =>
      val rnd = new Random(size)
      val initialMap = (0 until size).map(i => BigInt(rnd.nextLong()).mod(p))
      val sols = alg.filterAndProve(initialMap, finalH)
      sols.foreach(sol => alg.validate(sol, finalH) shouldBe 'success)
    }
  }

  property("Supow should generate solution from random 2^k secrets if difficulty = 0") {
    implicit val hash = new DefaultSuHash(pow.group.p)

    val finalH = pow.group.p
    val k = 2

    forAll { seed: String =>
      val taskSeed = Blake2b256(seed)
      val supow = Supow(seed, BigInt(2).pow(k).toInt, k)
      val solutions = supow.prove(finalH, taskSeed)
      //      todo any combination of initial secrets is valid.
      //      solutions.size shouldBe 1
      supow.verify(solutions.head) shouldBe 'success
    }
  }


}
