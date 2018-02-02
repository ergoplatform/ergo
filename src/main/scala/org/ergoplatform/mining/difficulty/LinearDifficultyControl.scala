package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, Height}
import org.ergoplatform.settings.Constants
import scorex.core.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

class LinearDifficultyControl(val desiredInterval: FiniteDuration,
                              val useLastEpochs: Int,
                              epochLength: Int) extends ScorexLogging {

  import LinearDifficultyControl._

  assert(useLastEpochs > 1, "useLastEpochs should always be > 1")
  assert(epochLength > 0, "epochLength should always be > 0")
  assert(epochLength < Int.MaxValue / useLastEpochs, s"epochLength $epochLength is too high for $useLastEpochs epochs")

  /**
    * @return heights of previous headers required for block recalculation
    */
  def previousHeadersRequiredForRecalculation(height: Height): Seq[Int] = {
    if ((height - 1) % epochLength == 0 && height > epochLength * useLastEpochs) {
      (0 to useLastEpochs).map(i => (height - 1) - i * epochLength).reverse
    } else {
      Seq(height - 1)
    }
  }

  @SuppressWarnings(Array("TraversableHead"))
  def calculate(previousHeaders: Seq[(Int, Header)]): Difficulty = {
    if (previousHeaders.lengthCompare(useLastEpochs + 1) == 0) {
      val data: Seq[(Int, Difficulty)] = previousHeaders.sliding(2).toList.map { d =>
        val start = d.head
        val end = d.last
        require(end._1 - start._1 == epochLength, s"Incorrect heights interval for $d")
        val diff = end._2.requiredDifficulty * desiredInterval.toMillis * epochLength / (end._2.timestamp - start._2.timestamp)
        (end._1, diff)
      }
      val diff = interpolate(data)
      if (diff >= 1) diff else Constants.InitialDifficulty
    } else previousHeaders.maxBy(_._1)._2.requiredDifficulty
  }

  //y = a + bx
  private[difficulty] def interpolate(data: Seq[(Int, Difficulty)]): Difficulty = {
    val size = data.size
    val xy: Iterable[BigInt] = data.map(d => d._1 * d._2)
    val x: Iterable[BigInt] = data.map(d => BigInt(d._1))
    val x2: Iterable[BigInt] = data.map(d => BigInt(d._1) * d._1)
    val y: Iterable[BigInt] = data.map(d => d._2)
    val xySum = xy.sum
    val x2Sum = x2.sum
    val ySum = y.sum
    val xSum = x.sum

    val k: BigInt = (xySum * size - xSum * ySum) * PrecisionConstant / (x2Sum * size - xSum * xSum)
    val b: BigInt = (ySum * PrecisionConstant - k * xSum) / size / PrecisionConstant

    val point = data.map(_._1).max + epochLength
    b + k * point / PrecisionConstant
  }

}

object LinearDifficultyControl {
  val PrecisionConstant: Int = 1000000000

}
