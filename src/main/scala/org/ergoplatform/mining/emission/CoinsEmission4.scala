package org.ergoplatform.mining.emission

/**
  * Ergo coin emission curve, proposal #4.
  * Properties:
  * block every 2 minutes
  * fixed rate during first 2 years
  * 19710000 coins after the first year
  * 97739925 coins total
  * no slow start period
  * reward reduction every 3 month
  */
object CoinsEmission4 {

  // 1 Ergo = 100 000 000 <minimal coin name>
  val CoinsInOneErgo: Long = 100000000

  // Number of blocks per hour on average
  val BlocksPerHour: Int = 30

  // Number of blocks per year on average
  val BlocksPerYear: Int = 365 * 24 * BlocksPerHour

  // reward reduction every 3 month
  val RewardReductionPeriod: Int = 90 * 24 * BlocksPerHour

  // Number of blocks for initial fixed rate
  val FixedRatePeriod = 2 * BlocksPerYear - RewardReductionPeriod

  // NUmber of coins issued per block during FixedRatePeriod
  val FixedRate = 75  * CoinsInOneErgo

  // slow period + 8 years of emission
  val BlocksTotal: Int = BlocksPerYear * 8

  //Total number of epochs with different number of coins issued
  val decreasingEpochs = (BlocksTotal - FixedRatePeriod) / RewardReductionPeriod

  def emissionAtHeight(h: Long): Long = {
    if (h <= FixedRatePeriod) FixedRate
    else if (h > BlocksTotal) 0
    else {
      val epoch: Int = ((h - FixedRatePeriod) / RewardReductionPeriod).toInt
      FixedRate - FixedRate * epoch / decreasingEpochs
    }
  }.ensuring(_ >= 0, s"Negative at $h")


}

