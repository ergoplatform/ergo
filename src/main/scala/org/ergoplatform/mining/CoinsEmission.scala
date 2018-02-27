package org.ergoplatform.mining

/**
  * Ergo coin emission curve, proposal #1.
  * Goals:
  * block every minute
  * ~25M coins after the first year
  * ~100M coins total
  * ~1 month slow start period
  * smooth graph
  */
object CoinsEmission {

  // 1 Ergo = 100 000 000 <minimal coin name>
  val CoinsInOneErgo: Long = 100000000

  // Number of blocks per hour on average
  val BlocksPerHour: Int = 60

  // Number of blocks per year on average
  val BlocksPerYear: Int = 365 * 24 * BlocksPerHour

  // 8 years of emission
  val BlocksTotal: Int = BlocksPerYear * 8

  // 1 Month slow start period
  val SlowStartPeriod: Int = 30 * 24 * BlocksPerHour

  // Number of blocks issued at the end of slow start period
  lazy val SlowStartFinalRate: Long = slowStartFunction(SlowStartPeriod)

  // Number of coins issued after slow start period
  lazy val SlowStartFinalSupply: Long = (0 until SlowStartPeriod).map(h => emissionAtHeight(h)).sum

  // 99999773 coins total supply
  lazy val TotalSupply: Long = FirstYearSupply + (BlocksPerYear until BlocksTotal).map(h => emissionAtHeight(h)).sum

  // 22382800 coins first year supply
  lazy val FirstYearSupply: Long = (0 until BlocksPerYear).map(h => emissionAtHeight(h)).sum

  def emissionAtHeight(h: Long): Long = {
    if (h <= SlowStartPeriod) slowStartFunction(h)
    else if (h > BlocksTotal) 0
    else -SlowStartFinalRate * (h - SlowStartPeriod) / (BlocksTotal - SlowStartPeriod) + SlowStartFinalRate
  }.ensuring(_ >= 0, s"Negative at $h")


  def slowStartFunction(h: Long): Long = h * h * 250 / 100 + (h + 1) * 2473


  def main(args: Array[String]): Unit ={
    println("First year supply: " + FirstYearSupply)
    println("total supply: " + TotalSupply)
  }
}




/**
  * Ergo coin emission curve, proposal #2.
  * Goals:
  * block every 2 minutes
  * ~25M coins after the first year
  * ~100M coins total
  * ~3 months slow start period
  * periodical reduction (smoother than halving)
  */
object CoinsEmission2 {

  // Number of blocks per hour on average
  val BlocksPerHour: Int = 30

  // Number of blocks per year on average
  val BlocksPerYear: Int = 365 * 24 * BlocksPerHour

  // 3 months slow start period
  val SlowStartPeriod: Int = 90 * 24 * BlocksPerHour

  // slow period + 8 years of emission
  val BlocksTotal: Int = BlocksPerYear * 8 + SlowStartPeriod

  // 100215692 coins total supply
  lazy val TotalSupply: Long = (1 to BlocksTotal).map(h => emissionAtHeight(h)).sum

  // 26736208 coins first year supply
  lazy val FirstYearSupply: Long = (1 to BlocksPerYear).map(h => emissionAtHeight(h)).sum

  def emissionAtHeight(h: Long): Long = {
    if (h <= SlowStartPeriod) slowStartFunction(h)
    else if (h > BlocksTotal) 0
    else {
      val c1 = 8 - (h - SlowStartPeriod) / BlocksPerYear
      c1 * c1 * 65536 * 29 / 8192 / 16 / 8
    }
  }.ensuring(_ >= 0, s"Negative at $h")


  def slowStartFunction(h: Long): Long = {
    val c1 = (h + (65536 - SlowStartPeriod)) * 29 / 1024 / 16
    c1
  }

  def main(args: Array[String]): Unit = {
   // (1 to 1000).foreach(h => println(s"height $h emission: ${emissionAtHeight(h)}"))
    println("first year supply: " + FirstYearSupply)
    println("total supply: " + TotalSupply)
  }
}