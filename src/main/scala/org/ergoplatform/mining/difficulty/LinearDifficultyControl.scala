package org.ergoplatform.mining.difficulty

import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, Height}

import scala.concurrent.duration.FiniteDuration

class LinearDifficultyControl(val desiredInterval: FiniteDuration,
                              epochLength: Int) extends DifficultyCalculator {

  private[difficulty] val UseLastEpochs: Int = 4

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
    * @param previousDifficulties - headers at chosen heights
    * @return
    */
  override def calculate(previousDifficulties: Seq[(Int, Difficulty)]): Difficulty = {
    if (previousDifficulties.size >= UseLastEpochs) {
      interpolate(previousDifficulties)(previousDifficulties.map(_._1).max + epochLength)
    } else previousDifficulties.maxBy(_._1)._2
  }

  //y = a + bx
  def interpolate(data: Seq[(Int, Difficulty)]): (Int) => Difficulty = {
    val size = data.size
    val xy: Iterable[BigInt] = data.map(d => d._1 * d._2)
    val x: Iterable[Int] = data.map(d => d._1)
    val x2: Iterable[Int] = data.map(d => d._1 * d._1)
    val y: Iterable[BigInt] = data.map(d => d._2)
    //TODO remove BigDecimal to achieve cross platform compatibility
    val xyMean = BigDecimal(xy.sum) / size
    val x2Mean = BigDecimal(x2.sum) / size
    val yMean = BigDecimal(y.sum) / y.size
    val xMean = BigDecimal(x.sum) / x.size

    val k: BigDecimal = (xyMean - xMean * yMean) / (x2Mean - xMean * xMean)
    val b: BigDecimal = yMean - k * xMean
    (point: Int) => {
      (b + k * point).toBigInt()
    }
  }

}
