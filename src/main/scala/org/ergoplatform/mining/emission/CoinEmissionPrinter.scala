package org.ergoplatform.mining.emission


import scala.annotation.tailrec

object CoinEmissionPrinter extends App {

  val emissionCurve = CoinsEmission2

  // Number of coins issued after slow start period
  lazy val SlowStartFinalSupply: Long = (0 until emissionCurve.SlowStartPeriod)
    .map(h => emissionCurve.emissionAtHeight(h)).sum

  // 100215692 coins total supply
  lazy val TotalSupply: Long = (1 to emissionCurve.BlocksTotal).map(h => emissionCurve.emissionAtHeight(h)).sum

  // 26736208 coins first year supply
  lazy val FirstYearSupply: Long = (1 to emissionCurve.BlocksPerYear).map(h => emissionCurve.emissionAtHeight(h)).sum

  println(s"BlocksTotal = ${emissionCurve.BlocksTotal}")
  println(s"BlocksPerYear = ${emissionCurve.BlocksPerYear}")
  println(s"SlowStartPeriod = ${emissionCurve.SlowStartPeriod}")
  println(s"SlowStartFinalSupply = ${SlowStartFinalSupply / emissionCurve.CoinsInOneErgo}")
  println("First year supply")
  println(25000000)
  println(FirstYearSupply / emissionCurve.CoinsInOneErgo)
  println("Total supply")
  println(100000000)
  println(TotalSupply / emissionCurve.CoinsInOneErgo)

  println("================")
  println("age (years), total coins, current rate")

  @tailrec
  def loop(height: Int, supply: Long): Unit = if (height < emissionCurve.BlocksTotal) {
    val currentSupply = emissionCurve.emissionAtHeight(height)
    if (height % (emissionCurve.BlocksPerHour * 60) == 0) println(s"${height.toDouble / emissionCurve.BlocksPerYear}, ${supply / emissionCurve.CoinsInOneErgo}, ${currentSupply.toDouble / emissionCurve.CoinsInOneErgo}")
    loop(height + 1, supply + currentSupply)
  }

  loop(0, 0)
}
