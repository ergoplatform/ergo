package org.ergoplatform.mining.difficulty

import org.ergoplatform.nodeView.history.ErgoHistory.{Difficulty, Height}

import scala.concurrent.duration.FiniteDuration

trait DifficultyCalculator {


  val desiredInterval: FiniteDuration

  /**
    * @return heights of previous headers required for difficulty recalculation
    * @return
    */
  def previousHeadersRequiredForRecalculation(height: Height): Seq[Int]

  def calculate(previousDifficulties: Seq[(Int, Difficulty)]): Difficulty

}
