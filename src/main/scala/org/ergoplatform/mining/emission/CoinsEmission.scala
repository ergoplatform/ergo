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
  // 2 years of fixed rate
  private val fixedRatePeriod = blocksPerHour * 2 * 24 * 365
  // 75 coins per block
  private val fixedRate = 2250 * coinsInOneErgo / blocksPerHour
  // 3 months of epoch
  private val epochLength: Int = 90 * 24 * blocksPerHour
  // 3 coins reduction every epoch
  private val oneEpochReduction = 3 * coinsInOneErgo
  val blocksTotal = fixedRatePeriod * 4


  def emissionAtHeight(h: Long): Long = {
    if (h < fixedRatePeriod) {
      fixedRate
    } else {
      val epoch = 1 + (h - fixedRatePeriod) / epochLength
      Math.max(fixedRate - oneEpochReduction * epoch, 0)
    }
  }.ensuring(_ >= 0, s"Negative at $h")

}

