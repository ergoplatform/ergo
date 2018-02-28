package org.ergoplatform.mining.emission


/**
  * Ergo coin emission curve, proposal #2.
  * Properties:
  * block every 2 minutes
  * ~25M coins after the first year
  * ~100M coins total
  * ~3 months slow start period
  * periodical reduction (smoother than halving)
  */
object CoinsEmission2 {

  // 1 Ergo = 100 000 000 <minimal coin name>
  val CoinsInOneErgo: Long = 100000000

  // Number of blocks per hour on average
  val BlocksPerHour: Int = 30

  // Number of blocks per year on average
  val BlocksPerYear: Int = 365 * 24 * BlocksPerHour

  // 3 months slow start period
  val SlowStartPeriod: Int = 90 * 24 * BlocksPerHour

  // slow period + 8 years of emission
  val BlocksTotal: Int = BlocksPerYear * 8 + SlowStartPeriod

  def emissionAtHeight(h: Long): Long = {
    if (h <= SlowStartPeriod) slowStartFunction(h) * CoinsInOneErgo
    else if (h > BlocksTotal) 0
    else {
      val c1 = 8 - (h - SlowStartPeriod) / BlocksPerYear
      c1 * c1 * 65536 * 29 / 8192 / 16 / 8 * CoinsInOneErgo
    }
  }.ensuring(_ >= 0, s"Negative at $h")


  def slowStartFunction(h: Long): Long = {
    val c1 = (h + (65536 - SlowStartPeriod)) * 29 / 1024 / 16
    c1
  }

}