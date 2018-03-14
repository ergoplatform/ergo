package org.ergoplatform.mining.emission

/**
  * Ergo coin emission curve.
  * Properties:
  * block every 2 minutes
  * fixed rate during first 2 years
  * 19710000 coins after the first year
  * 97739925 coins total
  * no slow start period
  * reward reduction every 3 month
  *
  * @param coinsInOneErgo - number of <minimal coin name> in 1 Ergo
  * @param blocksPerHour - munber of blocks per hour. Should be correlated with LinearDifficultyControl.desiredInterval
  */
class CoinsEmission(val coinsInOneErgo: Long = 100000000, val blocksPerHour: Int = 30) {

  // Number of blocks per year on average
  private val blocksPerYear: Int = 365 * 24 * blocksPerHour

  // 8 years of emission
  val blocksTotal: Int = blocksPerYear * 8

  // reward reduction every 3 month
  private lazy val rewardReductionPeriod: Int = 90 * 24 * blocksPerHour

  // Number of blocks for initial fixed rate
  private lazy val fixedRatePeriod = 2 * blocksPerYear - rewardReductionPeriod

  // NUmber of coins issued per block during FixedRatePeriod
  private val fixedRate = 2250 * coinsInOneErgo / blocksPerHour

  //Total number of epochs with different number of coins issued
  private val decreasingEpochs = (blocksTotal - fixedRatePeriod) / rewardReductionPeriod

  def emissionAtHeight(h: Long): Long = {
    if (h <= fixedRatePeriod) fixedRate
    else if (h > blocksTotal) 0
    else {
      val epoch: Int = ((h - fixedRatePeriod) / rewardReductionPeriod).toInt
      fixedRate - fixedRate * epoch / decreasingEpochs
    }
  }.ensuring(_ >= 0, s"Negative at $h")


}

