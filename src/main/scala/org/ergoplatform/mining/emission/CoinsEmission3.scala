package org.ergoplatform.mining.emission

/**
  * Ergo coin emission curve, proposal #2.
  * Properties:
  * block every 2 minutes
  * 20320182 coins after the first year
  * 99937106 coins total
  * ~3 months slow start period
  * reward reduction every 3 month
  */
object CoinsEmission3 {

  // 1 Ergo = 100 000 000 <minimal coin name>
  val CoinsInOneErgo: Long = 100000000

  // Number of blocks per hour on average
  val BlocksPerHour: Int = 30

  // Number of blocks per year on average
  val BlocksPerYear: Int = 365 * 24 * BlocksPerHour

  // 3 months slow start period
  val SlowStartPeriod: Int = 90 * 24 * BlocksPerHour

  // Number of coins isseud per block at the end of slow start period
  lazy val SlowStartFinalRate: Long = slowStartFunction(SlowStartPeriod)

  // reward reduction every 3 month
  val RewardReductionPeriod: Int = 90 * 24 * BlocksPerHour

  // slow period + 8 years of emission
  val BlocksTotal: Int = BlocksPerYear * 8 + SlowStartPeriod

  //Total number of epochs with different number of coins issued
  val epochsTotal = (BlocksTotal - SlowStartPeriod) / RewardReductionPeriod

  def emissionAtHeight(h: Long): Long = {
    if (h <= SlowStartPeriod) slowStartFunction(h)
    else if (h > BlocksTotal) 0
    else {
      val epoch: Int = ((h - SlowStartPeriod) / RewardReductionPeriod).toInt
      SlowStartFinalRate - SlowStartFinalRate * epoch / epochsTotal
    }
  }.ensuring(_ >= 0, s"Negative at $h")


  def slowStartFunction(h: Long): Long = {
    140000 * h
  }
}

