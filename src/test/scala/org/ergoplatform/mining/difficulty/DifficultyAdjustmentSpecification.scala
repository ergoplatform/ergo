package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.ChainSettings
import org.ergoplatform.utils.ErgoPropertyTest
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.duration._
import scala.util.Try

class DifficultyAdjustmentSpecification extends ErgoPropertyTest {

  val precision = 0.0001
  val minDiff: BigInt = (BigDecimal(1) / precision).toBigInt()
  val Epoch = 123

  val UseLastEpochs = 4
  val DesiredInterval: FiniteDuration = 1.minute

  def chainSettings(blockInterval: FiniteDuration = DesiredInterval,
                    useLastEpochs: Int = UseLastEpochs,
                    epochLength: Int = Epoch): ChainSettings =
    settings.chainSettings.copy(blockInterval = blockInterval, useLastEpochs = useLastEpochs, epochLength = epochLength)

  val control = new DifficultyAdjustment(chainSettings())

  property("nextRecalculationHeight vectors") {
    val epochLength = 128
    control.nextRecalculationHeight(926976, epochLength) shouldBe 926977
    control.nextRecalculationHeight(926977, epochLength) shouldBe 927105
    control.nextRecalculationHeight(926975, epochLength) shouldBe 926977
    control.nextRecalculationHeight(926950, epochLength) shouldBe 926977
    control.nextRecalculationHeight(1, epochLength) shouldBe 129
    control.nextRecalculationHeight(129, epochLength) shouldBe 257
    control.nextRecalculationHeight(256, epochLength) shouldBe 257
  }

  property("heightsForNextRecalculation vectors") {
    val epochLength = 128
    control.heightsForNextRecalculation(926976, epochLength) shouldBe Seq(926464, 926592, 926720, 926848, 926976)
    control.heightsForNextRecalculation(926977, epochLength) shouldBe Seq(926592, 926720, 926848, 926976, 927104)
    control.heightsForNextRecalculation(926975, epochLength) shouldBe Seq(926464, 926592, 926720, 926848, 926976)
    control.heightsForNextRecalculation(926950, epochLength) shouldBe Seq(926464, 926592, 926720, 926848, 926976)
    control.heightsForNextRecalculation(1, epochLength) shouldBe Seq(0, 128)
    control.heightsForNextRecalculation(129, epochLength) shouldBe Seq(0, 128, 256)
    control.heightsForNextRecalculation(256, epochLength) shouldBe Seq(0, 128, 256)
  }

  property("previousHeadersRequiredForRecalculation() should return correct heights required for recalculation") {
    val height = Epoch * (UseLastEpochs + 1) + 1
    control.previousHeightsRequiredForRecalculation(height, Epoch) shouldEqual
      Seq(height - 4 * Epoch - 1, height - 3 * Epoch - 1, height - 2 * Epoch - 1, height - Epoch - 1, height - 1)
  }

  property("previousHeadersRequiredForRecalculation() with Epoch = 1") {
    forAll(Gen.choose(2, 1000)) { _ =>
      val useLastEpochs = 3
      val epochLength = 1
      val control = new DifficultyAdjustment(chainSettings(1.minute, useLastEpochs, epochLength))
      val height = useLastEpochs + 1
      control.previousHeightsRequiredForRecalculation(height, epochLength) shouldEqual (0 until height)
    }
  }

  property("previousHeadersRequiredForRecalculation() should return previous block if there should not be difficulty recalculation") {
    control.previousHeightsRequiredForRecalculation(Epoch / 2 + 1, Epoch) shouldBe Seq(Epoch / 2)
    control.previousHeightsRequiredForRecalculation(Epoch * UseLastEpochs, Epoch) shouldBe Seq(Epoch * UseLastEpochs - 1)
    control.previousHeightsRequiredForRecalculation(Epoch * UseLastEpochs + 2, Epoch) shouldBe Seq(Epoch * UseLastEpochs + 1)
  }

  property("previousHeadersRequiredForRecalculation() should return as much block heights as possible") {
    control.previousHeightsRequiredForRecalculation(Epoch + 1, Epoch) shouldBe Seq(0, Epoch)
    control.previousHeightsRequiredForRecalculation(2 * Epoch + 1, Epoch) shouldBe Seq(0, Epoch, 2 * Epoch)
    control.previousHeightsRequiredForRecalculation(3 * Epoch + 1, Epoch) shouldBe Seq(0, Epoch, 2 * Epoch, 3 * Epoch)
    control.previousHeightsRequiredForRecalculation(4 * Epoch + 1, Epoch) shouldBe Seq(0, Epoch, 2 * Epoch, 3 * Epoch, 4 * Epoch)
    control.previousHeightsRequiredForRecalculation(5 * Epoch + 1, Epoch) shouldBe Seq(Epoch, 2 * Epoch, 3 * Epoch, 4 * Epoch, 5 * Epoch)
  }

  property("previousHeadersRequiredForRecalculation() should generate valid heights for calculate()") {
    forAll(Gen.choose(1, Int.MaxValue), defaultHeaderGen) { (height: Int, header: Header) =>
      val previousHeaders = control.previousHeightsRequiredForRecalculation(height, Epoch)
        .map(i => header.copy(timestamp = header.timestamp + i, height = i))

      Try(control.calculate(previousHeaders, Epoch)) shouldBe 'success
    }
  }


  property("calculate() should require correct heights") {
    forAll(Gen.choose(UseLastEpochs, 10 * UseLastEpochs), defaultHeaderGen) { (i: Int, header: Header) =>
      val previousHeaders = control.previousHeightsRequiredForRecalculation(i * Epoch + 1, Epoch)
        .map(i => header.copy(timestamp = header.timestamp + i, height = i))
      previousHeaders.length shouldBe UseLastEpochs + 1

      Try(control.calculate(previousHeaders, Epoch)) shouldBe 'success
      Try(control.calculate(previousHeaders.map(h => h.copy(height = h.height * 2)), Epoch)) shouldBe 'failure
    }
  }

  property("calculate() should decrease difficulty if block time interval is higher than expected") {
    forAll(Gen.choose(UseLastEpochs, 10 * UseLastEpochs), defaultHeaderGen) { (startEpoch: Int, header: Header) =>
      whenever(header.requiredDifficulty > 10) {
        val previousHeaders = control.previousHeightsRequiredForRecalculation(startEpoch * Epoch + 1, Epoch)
          .map(i => header.copy(timestamp = header.timestamp + DesiredInterval.toMillis * 2 * i, height = i))
        previousHeaders.length shouldBe UseLastEpochs + 1

        control.calculate(previousHeaders, Epoch) < header.requiredDifficulty shouldBe true
      }
    }
  }


  property("interpolate() vectors") {
    val diff = BigInt("675204474840679645414180963439886534428")
    control.interpolate(Seq((799167010, diff), (799167133, diff), (799167256, diff), (799167379, diff)), Epoch) shouldBe diff

    control.interpolate(Seq((123, diff), (246, diff), (369, diff), (492, diff)), Epoch) shouldBe diff

    control.interpolate(Vector((123, diff), (246, diff * 2), (369, diff * 2), (492, diff)), Epoch) shouldBe (diff * 3 / 2)

    control.interpolate(Vector((123, diff), (246, diff * 2), (369, diff * 3), (492, diff * 4)), Epoch) shouldBe BigInt("3376022374203398227070904817199432672139")

  }

  property("interpolate() for constant hashrate") {
    forAll(epochGen, diffGen) { (startEpoch: Int, diff: BigInt) =>
      val previousDifficulties = (startEpoch * Epoch until (UseLastEpochs + startEpoch) * Epoch by Epoch).map(i => (i, diff))
      val newDiff = control.interpolate(previousDifficulties, Epoch)
      (BigDecimal(newDiff - diff) / BigDecimal(diff)).toDouble should be < precision
    }
  }


  property("interpolate() for linear hashrate growth") {
    forAll(epochGen, diffGen, smallPositiveInt, smallPositiveInt) { (startEpoch, diff, epoch, useLastEpochs) =>
      whenever(useLastEpochs > 1) {
        val control = new DifficultyAdjustment(chainSettings(1.minute, useLastEpochs, epoch))
        val previousDifficulties = (startEpoch * epoch until (useLastEpochs + startEpoch) * epoch by epoch).map(i => (i, diff * i))
        val newDiff = control.interpolate(previousDifficulties, epoch)
        val expected = previousDifficulties.map(_._2).max + diff
        equalsWithPrecision(expected, newDiff)
      }
    }
  }

  property("calculate() for different epoch lengths and constant hashrate") {
    forAll(defaultHeaderGen, smallPositiveInt, smallPositiveInt, Gen.choose(1, 60 * 60 * 1000)) { (header: Header, epoch, useLastEpochs, interval) =>
      whenever(useLastEpochs > 1 && header.requiredDifficulty >= 1) {
        val control = new DifficultyAdjustment(chainSettings(interval.millis, useLastEpochs, epoch))
        val previousHeaders = control.previousHeightsRequiredForRecalculation(epoch * useLastEpochs + 1, epoch)
          .map(i => header.copy(timestamp = header.timestamp + i * interval, height = i))
        previousHeaders.length shouldBe useLastEpochs + 1
        control.calculate(previousHeaders, epoch) shouldBe header.requiredDifficulty
      }
    }
  }

  property("calculate() for different epoch lengths and linear hashrate") {
    val step = 1000
    forAll(defaultHeaderGen, smallPositiveInt, smallPositiveInt, Gen.choose(1, 60 * 60 * 1000)) { (header: Header, epoch, useLastEpochs, interval) =>
      whenever(useLastEpochs > 1) {
        val control = new DifficultyAdjustment(chainSettings(interval.millis, useLastEpochs, epoch))
        val previousHeaders = control.previousHeightsRequiredForRecalculation(epoch * useLastEpochs + 1, epoch).map { i =>
          header.copy(timestamp = header.timestamp + i * interval,
            height = i,
            nBits = DifficultySerializer.encodeCompactBits(DifficultySerializer.decodeCompactBits(header.nBits) + step))
        }

        previousHeaders.length shouldBe useLastEpochs + 1
        val expectedDifficulty = previousHeaders.last.requiredDifficulty + step
        val error = BigDecimal(control.calculate(previousHeaders, epoch) - expectedDifficulty) / BigDecimal(expectedDifficulty)
        error should be < BigDecimal(1) / DifficultyAdjustment.PrecisionConstant
      }
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
