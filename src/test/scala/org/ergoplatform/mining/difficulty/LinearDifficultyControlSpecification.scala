package org.ergoplatform.mining.difficulty

import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoGenerators
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

import scala.concurrent.duration._
import scala.util.Try

class LinearDifficultyControlSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators {

  val precision = 0.0001
  val minDiff = (BigDecimal(1) / precision).toBigInt()
  val Epoch = 123
  val control = new LinearDifficultyControl(1.minute, Epoch)
  val UseLastEpochs = control.UseLastEpochs

  property("previousHeadersRequiredForRecalculation() should return correct heights required for recalculation") {
    control.previousHeadersRequiredForRecalculation(Epoch * (UseLastEpochs + 1)) shouldEqual
      Seq(Epoch * (UseLastEpochs + 1) - Epoch / 2, Epoch * (UseLastEpochs + 1) - 3 * Epoch / 2,
        Epoch * (UseLastEpochs + 1) - 5 * Epoch / 2, Epoch * (UseLastEpochs + 1) - 7 * Epoch / 2)
  }

  property("previousHeadersRequiredForRecalculation() should return previous block if there should not be difficulty recalculation") {
    control.previousHeadersRequiredForRecalculation(Epoch / 2) shouldBe Seq(Epoch / 2 - 1)
    control.previousHeadersRequiredForRecalculation(Epoch) shouldBe Seq(Epoch - 1)
    control.previousHeadersRequiredForRecalculation(Epoch * UseLastEpochs) shouldBe Seq(Epoch * UseLastEpochs - 1)
    control.previousHeadersRequiredForRecalculation(Epoch * UseLastEpochs * 2 + 1) shouldBe
      Seq(Epoch * UseLastEpochs * 2)
  }

  property("calculate() should require correct heights") {
    val diff = Constants.InitialDifficulty
    val previousDifficulties = (0 until UseLastEpochs * Epoch by Epoch).map(i => (i, diff))
    Try(control.calculate(previousDifficulties)) shouldBe 'success
    Try(control.calculate(previousDifficulties.map(i => (i._1 * 2, i._2)))) shouldBe 'failure
  }

  property("previousHeadersRequiredForRecalculation() should generate valid heights for calculate()") {
    forAll { h: Int =>
      whenever(h > 0) {
        val previousDifficulties = control.previousHeadersRequiredForRecalculation(h).map(i => (i, BigInt(i)))
        Try(control.calculate(previousDifficulties)) shouldBe 'success
      }
    }
  }

  property("calculate() vectors") {
    val diff = BigInt("675204474840679645414180963439886534428")
    control.calculate(Seq((799167010, diff), (799167133, diff), (799167256, diff), (799167379, diff))) shouldBe diff

    control.calculate(Seq((123, diff), (246, diff), (369, diff), (492, diff))) shouldBe diff

    control.calculate(Vector((123, diff), (246, diff * 2), (369, diff * 2), (492, diff))) shouldBe (diff * 3 / 2)

    control.calculate(Vector((123, diff), (246, diff * 2), (369, diff * 3), (492, diff * 4))) shouldBe BigInt("3376022374203398227070904817199432672139")

  }

  property("calculate() for constant hashrate") {
    forAll(epochGen, diffGen) { (startEpoch: Int, diff: BigInt) =>
      exitOnError {
        val previousDifficulties = (startEpoch * Epoch until (UseLastEpochs + startEpoch) * Epoch by Epoch).map(i => (i, diff))
        val newDiff = control.calculate(previousDifficulties)
        (BigDecimal(newDiff - diff) / BigDecimal(diff)).toDouble should be < precision
      }
    }
  }


  property("calculate() for linear hashrate growth") {
    forAll(epochGen, diffGen) { (startEpoch: Int, diff: BigInt) =>
      exitOnError {
        val previousDifficulties = (startEpoch * Epoch until (UseLastEpochs + startEpoch) * Epoch by Epoch).map(i => (i, diff * i))
        val newDiff = control.calculate(previousDifficulties)
        val expected = previousDifficulties.map(_._2).max + diff
        equalsWithPrecision(expected, newDiff)
      }
    }
  }

  def exitOnError(r: => Unit): Unit = Try {
    r
  }.recoverWith { case e =>
    e.printStackTrace()
    System.exit(1)
    throw e
  }

  def equalsWithPrecision(i: BigInt, j: BigInt): Unit = {
    require((BigDecimal(i - j) / BigDecimal(j)).toDouble < precision, s"$i and $j are too different")
  }

  def diffGen: Gen[BigInt] = for {
    rnd <- Arbitrary.arbitrary[BigInt]
    diff = if (rnd < 0) -rnd + minDiff else rnd + minDiff
    _ = assert(diff > 0, s"$diff,$minDiff,$rnd")
  } yield diff


  def epochGen: Gen[Int] = Gen.choose(1, Int.MaxValue - UseLastEpochs)

}
