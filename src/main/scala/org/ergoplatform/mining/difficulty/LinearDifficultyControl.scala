package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, Height}
import org.ergoplatform.settings.{Args, ChainSettings, ErgoSettings, NetworkType}
import scorex.core.utils.NetworkTimeProvider
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.util.ScorexLogging

import scala.concurrent.duration.FiniteDuration

class LinearDifficultyControl(val chainSettings: ChainSettings) extends ScorexLogging {

  import LinearDifficultyControl._

  val desiredInterval: FiniteDuration = chainSettings.blockInterval
  val useLastEpochs: Int = chainSettings.useLastEpochs
  val epochLength: Int = chainSettings.epochLength
  val initialDifficulty: BigInt = chainSettings.initialDifficulty

  require(useLastEpochs > 1, "useLastEpochs should always be > 1")
  require(epochLength > 0, "epochLength should always be > 0")
  require(epochLength < Int.MaxValue / useLastEpochs, s"epochLength $epochLength is too high for $useLastEpochs epochs")

  /**
    * @return heights of previous headers required for block recalculation
    */
  def previousHeadersRequiredForRecalculation(height: Height): Seq[Int] = {
    if ((height - 1) % epochLength == 0 && epochLength > 1) {
      (0 to useLastEpochs).map(i => (height - 1) - i * epochLength).filter(_ >= 0).reverse
    } else if ((height - 1) % epochLength == 0 && height > epochLength * useLastEpochs) {
      (0 to useLastEpochs).map(i => (height - 1) - i * epochLength).reverse
    } else {
      Seq(height - 1)
    }
  }

  def newCalculate(previousHeaders: Seq[Header]): Difficulty = {
    def bitcoinCalculate(start: Header, end: Header) = {
      end.requiredDifficulty * desiredInterval.toMillis * epochLength / (end.timestamp - start.timestamp)
    }
    val predictiveDiff = calculate(previousHeaders)
    val classicDiff = if(previousHeaders.size>=3){
      val hs = previousHeaders.takeRight(2)
      bitcoinCalculate(hs(0), hs(1))
    } else {
      predictiveDiff
    }
    val avg = (classicDiff + predictiveDiff) / 2
    //val lastDiff = previousHeaders.last.requiredDifficulty

    val uncompressedDiff = avg /*if(avg > lastDiff){
      avg.min(lastDiff * 3 / 2)
    } else {
      avg.max(lastDiff * 2 / 3)
    }*/
    // perform serialization cycle in order to normalize resulted difficulty
    RequiredDifficulty.decodeCompactBits(
      RequiredDifficulty.encodeCompactBits(uncompressedDiff)
    )
  }

  @SuppressWarnings(Array("TraversableHead"))
  def calculate(previousHeaders: Seq[Header]): Difficulty = {
    require(previousHeaders.nonEmpty, "PreviousHeaders should always contain at least 1 element")

    val uncompressedDiff = {
      if (previousHeaders.lengthCompare(1) == 0 || previousHeaders.head.timestamp >= previousHeaders.last.timestamp) {
        previousHeaders.head.requiredDifficulty
      } else {
        val data: Seq[(Int, Difficulty)] = previousHeaders.sliding(2).toList.map { d =>
          val start = d.head
          val end = d.last
          require(end.height - start.height == epochLength, s"Incorrect heights interval for $d")
          val diff = end.requiredDifficulty * desiredInterval.toMillis * epochLength / (end.timestamp - start.timestamp)
          (end.height, diff)
        }
        val diff = interpolate(data)
        if (diff >= 1) diff else initialDifficulty
      }
    }
    // perform serialization cycle in order to normalize resulted difficulty
    RequiredDifficulty.decodeCompactBits(
      RequiredDifficulty.encodeCompactBits(uncompressedDiff)
    )
  }

  //y = a + bx
  private[difficulty] def interpolate(data: Seq[(Int, Difficulty)]): Difficulty = {
    val size = data.size
    if (size == 1) {
      data.head._2
    } else {
      val xy: Iterable[BigInt] = data.map(d => d._1 * d._2)
      val x: Iterable[BigInt] = data.map(d => BigInt(d._1))
      val x2: Iterable[BigInt] = data.map(d => BigInt(d._1) * d._1)
      val y: Iterable[BigInt] = data.map(d => d._2)
      val xySum = xy.sum
      val x2Sum = x2.sum
      val ySum = y.sum
      val xSum = x.sum

      val b: BigInt = (xySum * size - xSum * ySum) * PrecisionConstant / (x2Sum * size - xSum * xSum)
      val a: BigInt = (ySum * PrecisionConstant - b * xSum) / size / PrecisionConstant

      val point = data.map(_._1).max + epochLength
      a + b * point / PrecisionConstant
    }
  }

}

object LinearDifficultyControl {
  val PrecisionConstant: Int = 1000000000
}




object v2testing extends App {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private val ergoSettings: ErgoSettings = ErgoSettings.read(Args(Some("/home/kushti/ergo/mainnet/mainnet.conf"), Some(NetworkType.MainNet)))

  val ntp = new NetworkTimeProvider(ergoSettings.scorexSettings.ntp)

  val ldc = new LinearDifficultyControl(ergoSettings.chainSettings)

  val eh = ErgoHistory.readOrGenerate(ergoSettings, ntp)


  println("best: " + eh.bestHeaderOpt.map(_.height))

  val heights = ldc.previousHeadersRequiredForRecalculation(842752 + 1 + 1024)

  println("hs: " + heights)

  val headerOpts = heights.map(eh.bestHeaderIdAtHeight).map(idOpt => idOpt.flatMap(id => eh.typedModifierById[Header](id)))

  println(headerOpts.map(_.map(_.id)))

  println("dir: " + ergoSettings.directory)

  val h1 = headerOpts.head.get.copy(height = 842752 + 1024, nBits = 122447235L, timestamp = System.currentTimeMillis() + 1000*60*1440*3)

  val headers = headerOpts.flatten.toSeq ++ Seq(h1)

  val diff1 = ldc.calculate(headers)
  println("diff1: " + diff1)
  val nbits1 = RequiredDifficulty.encodeCompactBits(diff1)

  val h2 = headerOpts.head.get.copy(height = 842752 + 2048, nBits = nbits1, timestamp = System.currentTimeMillis() + 1000*60*1440*6)

  val headers2 = headerOpts.flatten.toSeq.tail ++ Seq(h1, h2)
  val diff2 = ldc.calculate(headers2)
  println("diff2: " + diff2)
  val nbits2 = RequiredDifficulty.encodeCompactBits(diff2)

  val h3 = headerOpts.head.get.copy(height = 842752 + 3072, nBits = nbits2, timestamp = System.currentTimeMillis() + 1000*60*1440*8)
  val headers3 = headers2.tail ++ Seq(h3)
  val diff3 = ldc.calculate(headers3)
  println("diff3: " + diff3)
  val nbits3 = RequiredDifficulty.encodeCompactBits(diff3)

  val h4 = headerOpts.head.get.copy(height = 842752 + 4096, nBits = nbits3, timestamp = System.currentTimeMillis() + (1000*60*1440*9.5).toInt)
  val headers4 = headers3.tail ++ Seq(h4)
  val diff4 = ldc.calculate(headers4)
  println("diff4: " + diff4)
  val nbits4 = RequiredDifficulty.encodeCompactBits(diff4)


  val h5 = headerOpts.head.get.copy(height = 842752 + 4096 + 1024, nBits = nbits3, timestamp = System.currentTimeMillis() + (1000*60*1440*10.5).toInt)
  val headers5 = headers4.tail ++ Seq(h5)
  val diff5 = ldc.calculate(headers5)
  println("diff5: " + diff5)
  val nbits5 = RequiredDifficulty.encodeCompactBits(diff5)


  val h6 = headerOpts.head.get.copy(height = 842752 + 4096 + 2048, nBits = nbits3, timestamp = System.currentTimeMillis() + (1000*60*1440*11.5).toInt)
  val headers6 = headers5.tail ++ Seq(h6)
  val diff6 = ldc.calculate(headers6)
  println("diff6: " + diff6)
  val nbits6 = RequiredDifficulty.encodeCompactBits(diff6)

}



object AltDiff extends App {

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private val currentSettings: ErgoSettings =
    ErgoSettings.read(Args(Some("/home/kushti/ergo/mainnet/mainnet.conf"), Some(NetworkType.MainNet)))

  private val altSettings: ErgoSettings =
    ErgoSettings.read(Args(Some("/home/kushti/ergo/mainnet/alt.conf"), Some(NetworkType.MainNet)))

  val ntp = new NetworkTimeProvider(altSettings.scorexSettings.ntp)


  println(currentSettings.chainSettings.epochLength)
  println(altSettings.chainSettings.epochLength)

  val eh = ErgoHistory.readOrGenerate(altSettings, ntp)

  println("best: " + eh.bestHeaderOpt.map(_.height))

  val ldc = new LinearDifficultyControl(altSettings.chainSettings)
/*
  val heights = ldc.previousHeadersRequiredForRecalculation(843265)
  val headerOpts = heights.map(eh.bestHeaderIdAtHeight).map(idOpt => idOpt.flatMap(id => eh.typedModifierById[Header](id)))
  val h1 = headerOpts.head.get.copy(height = 843264, nBits = 122447235L, timestamp = System.currentTimeMillis() + 1000*60*120)

  val headers = headerOpts.flatten.toSeq ++ Seq(h1)

  val diff1 = ldc.calculate(headers)
  println("diff1: " + diff1)
  val nbits1 = RequiredDifficulty.encodeCompactBits(diff1)
*/
  (1 to 843207).foreach{h =>
    if(h % 1024 == 1 && h > 1) {
      val heights = ldc.previousHeadersRequiredForRecalculation(h)
      val headers = heights.map(eh.bestHeaderIdAtHeight).map(idOpt => idOpt.flatMap(id => eh.typedModifierById[Header](id))).flatten
      val calcDiff = ldc.newCalculate(headers)
      val chainDiff = eh.bestHeaderAtHeight(h).get.requiredDifficulty

      println(s"diff for $h: $calcDiff, chain diff: ${chainDiff*100/calcDiff-100}%")
    }
  }

}
