package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.utils.{ErgoGenerators, NoShrink}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

import scala.concurrent.duration._
import scala.util.Try

class LinearDifficultyControlSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators
  with NoShrink {

  val precision = 0.0001
  val minDiff: BigInt = (BigDecimal(1) / precision).toBigInt()
  val Epoch = 123

  val UseLastEpochs = 4
  val control = new LinearDifficultyControl(1.minute, UseLastEpochs, Epoch)

  property("previousHeadersRequiredForRecalculation() should return correct heights required for recalculation") {
    val height = Epoch * (UseLastEpochs + 1) + 1
    control.previousHeadersRequiredForRecalculation(height) shouldEqual
      Seq(height - 4 * Epoch - 1, height - 3 * Epoch - 1, height - 2 * Epoch - 1, height - Epoch - 1, height - 1)
  }

  property("previousHeadersRequiredForRecalculation() with Epoch = 1") {
    forAll(Gen.choose(2, 1000)) { useLastEpochs1 =>
      val useLastEpochs = 3
      val control = new LinearDifficultyControl(1.minute, useLastEpochs, 1)
      val height = useLastEpochs + 1
      control.previousHeadersRequiredForRecalculation(height) shouldEqual (0 until height)
    }
  }


  property("previousHeadersRequiredForRecalculation() should return previous block if there should not be difficulty recalculation") {
    control.previousHeadersRequiredForRecalculation(Epoch / 2 + 1) shouldBe Seq(Epoch / 2)
    control.previousHeadersRequiredForRecalculation(Epoch + 1) shouldBe Seq(Epoch)
    control.previousHeadersRequiredForRecalculation(Epoch * UseLastEpochs) shouldBe Seq(Epoch * UseLastEpochs - 1)
    control.previousHeadersRequiredForRecalculation(Epoch * UseLastEpochs + 2) shouldBe Seq(Epoch * UseLastEpochs + 1)
  }

  property("previousHeadersRequiredForRecalculation() should generate valid heights for calculate()") {
    forAll(Gen.choose(1, Int.MaxValue), invalidHeaderGen) { (height: Int, header: Header) =>
      val previousHeaders = control.previousHeadersRequiredForRecalculation(height)
        .map(i => (i, header.copy(timestamp = header.timestamp + i)))

      Try(control.calculate(previousHeaders)) shouldBe 'success
    }
  }


  property("calculate() should require correct heights") {
    forAll(Gen.choose(UseLastEpochs, 10 * UseLastEpochs), invalidHeaderGen) { (i: Int, header: Header) =>
      val previousHeaders = control.previousHeadersRequiredForRecalculation(i * Epoch + 1)
        .map(i => (i, header.copy(timestamp = header.timestamp + i)))
      previousHeaders.length shouldBe UseLastEpochs + 1

      Try(control.calculate(previousHeaders)) shouldBe 'success
      Try(control.calculate(previousHeaders.map(i => (i._1 * 2, i._2)))) shouldBe 'failure
    }
  }

  property("calculate() vectors") {
    val diff = BigInt("675204474840679645414180963439886534428")
    control.interpolate(Seq((799167010, diff), (799167133, diff), (799167256, diff), (799167379, diff))) shouldBe diff

    control.interpolate(Seq((123, diff), (246, diff), (369, diff), (492, diff))) shouldBe diff

    control.interpolate(Vector((123, diff), (246, diff * 2), (369, diff * 2), (492, diff))) shouldBe (diff * 3 / 2)

    control.interpolate(Vector((123, diff), (246, diff * 2), (369, diff * 3), (492, diff * 4))) shouldBe BigInt("3376022374203398227070904817199432672139")

  }

  property("calculate() for constant hashrate") {
    forAll(epochGen, diffGen) { (startEpoch: Int, diff: BigInt) =>
      val previousDifficulties = (startEpoch * Epoch until (UseLastEpochs + startEpoch) * Epoch by Epoch).map(i => (i, diff))
      val newDiff = control.interpolate(previousDifficulties)
      (BigDecimal(newDiff - diff) / BigDecimal(diff)).toDouble should be < precision
    }
  }


  property("calculate() for linear hashrate growth") {
    forAll(epochGen, diffGen) { (startEpoch: Int, diff: BigInt) =>
      val previousDifficulties = (startEpoch * Epoch until (UseLastEpochs + startEpoch) * Epoch by Epoch).map(i => (i, diff * i))
      val newDiff = control.interpolate(previousDifficulties)
      val expected = previousDifficulties.map(_._2).max + diff
      equalsWithPrecision(expected, newDiff)
    }
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
