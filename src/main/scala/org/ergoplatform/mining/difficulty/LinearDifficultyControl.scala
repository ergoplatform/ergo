package org.ergoplatform.mining.difficulty

import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, Height}
import scorex.core.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

class LinearDifficultyControl(val desiredInterval: FiniteDuration,
                              epochLength: Int) extends ScorexLogging {

  import LinearDifficultyControl._

  /**
    * @return heights of previous headers required for block recalculation
    */
  def previousHeadersRequiredForRecalculation(height: Height): Seq[Int] = {
    if ((height - 1) % epochLength == 0 && height > epochLength * UseLastEpochs) {
      (0 to UseLastEpochs).map(i => (height - 1) - i * epochLength).reverse
    } else {
      Seq(height - 1)
    }
  }

  def calculate(previousHeaders: Seq[(Int, Header)]): Difficulty = {
    if (previousHeaders.size == UseLastEpochs + 1) {
      val data: Seq[(Int, Difficulty)] = previousHeaders.sliding(2).toList.map { d =>
        val start = d.head
        val end = d.last
        require(end._1 - start._1 == epochLength, s"Incorrect heights interval for $d")
        val diff = end._2.requiredDifficulty * desiredInterval.toMillis / (end._2.timestamp - start._2.timestamp)
        (end._1 + start._1 / 2, diff)
      }
      val newDiff = interpolate(data)
      log.info(s"New difficulty $newDiff calculated from data $data")
      newDiff
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
  val UseLastEpochs: Int = 4
  val PrecisionConstant: Int = 1000000000

}
