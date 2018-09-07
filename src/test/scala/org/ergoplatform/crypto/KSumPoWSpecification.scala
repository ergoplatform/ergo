package org.ergoplatform.crypto

import org.ergoplatform.crypto.pow.WagnerAlg
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.Gen

import scala.util.Random

class KSumPoWSpecification extends ErgoPropertyTest {

  val p = 9973
  val k = 2
  val alg = new WagnerAlg(k, p)

  property("Sum to 0") {
    val finalH = 0
    forAll(Gen.choose(0, 2000)) { size: Int =>
      val rnd = new Random(size)
      val initialMap = (0 until size).map(i =>  BigInt(rnd.nextLong()).mod(p))
      val sols = alg.filterAndProve(initialMap, finalH)
      sols.foreach(sol => alg.validate(sol, finalH) shouldBe 'success)
      sols.foreach(sol => sol.numbers.sum.mod(p) shouldBe finalH)
      sols.foreach(sol => !sols.exists(_.numbers == sol.numbers))
    }
  }

  property("Sum to interval") {
    val finalH = 30

    forAll(Gen.choose(0, 2000)) { size: Int =>
      val rnd = new Random(size)
      val initialMap = (0 until size).map(i =>  BigInt(rnd.nextLong()).mod(p))
      val sols = alg.filterAndProve(initialMap, finalH)
      sols.foreach(sol => alg.validate(sol, finalH) shouldBe 'success)
      sols.foreach(sol => require(sol.numbers.sum.mod(p) <= finalH, s"${sol.numbers.sum.mod(p)} < $finalH"))
    }

    val list = (0 until 200).map(i => (i, BigInt(Random.nextLong()).mod(p))).toMap
    val sols = alg.prove(list, finalH)
    sols.exists(sol => sol.numbers.sum.mod(p) > 0) shouldBe true
  }

}
