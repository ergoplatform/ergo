package org.ergoplatform.mining.difficulty

import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, Height}

import scala.concurrent.duration.FiniteDuration

class LinearDifficultyControl(val desiredInterval: FiniteDuration,
                              epochLength: Int) extends DifficultyCalculator {


  private[difficulty] val UseLastEpochs: Int = 4
  private[difficulty] val PrecisionConstant: Int = 1000000000

  /**
    * @return heights of previous headers required for block recalculation
    */
  override def previousHeadersRequiredForRecalculation(height: Height): Seq[Int] = {
    if (height % epochLength == 0 && height > epochLength * UseLastEpochs) {
      (0 until UseLastEpochs).map(i => height - i * epochLength - epochLength / 2)
    } else {
      Seq(height - 1)
    }
  }


  /**
    *
    * @param previousDifficulties - difficulties at chosen heights
    * @return difficulty for the next epoch
    */
  override def calculate(previousDifficulties: Seq[(Int, Difficulty)]): Difficulty = {
    if (previousDifficulties.size == UseLastEpochs) {
      require((1 until UseLastEpochs)
        .forall(i => previousDifficulties(i)._1 - previousDifficulties(i - 1)._1 == epochLength),
        s"Heights step in previousDifficulties=$previousDifficulties should equal epochLength=$epochLength")
      interpolate(previousDifficulties)(previousDifficulties.map(_._1).max + epochLength)
    } else previousDifficulties.maxBy(_._1)._2
  }

  //y = a + bx
  def interpolate(data: Seq[(Int, Difficulty)]): (Int) => Difficulty = {
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

    (point: Int) => {
      b + k * point / PrecisionConstant
    }
  }

}
