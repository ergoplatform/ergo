package org.ergoplatform.tools

import org.ergoplatform.mining.emission.CoinsEmission

import scala.annotation.tailrec

object CoinEmissionPrinter extends App {

  val emissionCurve = new CoinsEmission()

  //  // Number of coins issued after slow start period
  //  lazy val SlowStartFinalSupply: Long = (0 until emissionCurve.SlowStartPeriod)
  //    .map(h => emissionCurve.emissionAtHeight(h)).sum
  val blocksPerYear = 365 * 24 * emissionCurve.blocksPerHour

  // 100215692 coins total supply
  lazy val TotalSupply: Long = (1 to emissionCurve.blocksTotal).map(h => emissionCurve.emissionAtHeight(h)).sum

  // 26736208 coins first year supply
  lazy val FirstYearSupply: Long = (1 to blocksPerYear).map(h => emissionCurve.emissionAtHeight(h)).sum

  println(s"BlocksTotal = ${emissionCurve.blocksTotal}")
  println(s"BlocksPerYear = ${blocksPerYear}")
  //  println(s"SlowStartPeriod = ${emissionCurve.SlowStartPeriod}")
  //  println(s"SlowStartFinalSupply = ${SlowStartFinalSupply / emissionCurve.CoinsInOneErgo}")
  //  println(s"SlowStartFinalRate = ${emissionCurve.emissionAtHeight(emissionCurve.SlowStartPeriod) / emissionCurve.CoinsInOneErgo}")
  println(s"EndRate = ${emissionCurve.emissionAtHeight(emissionCurve.blocksTotal).toDouble / emissionCurve.coinsInOneErgo}")
  println("First year supply")
  println(25000000)
  println(FirstYearSupply / emissionCurve.coinsInOneErgo)
  println("Total supply")
  println(100000000)
  println(TotalSupply / emissionCurve.coinsInOneErgo)

  println("================")
  println("age (years), total coins, current rate")

  @tailrec
  def loop(height: Int, supply: Long): Unit = if (height < emissionCurve.blocksTotal) {
    val currentSupply = emissionCurve.emissionAtHeight(height)
    if (height % (emissionCurve.blocksPerHour * 60) == 0) println(s"${height.toDouble / blocksPerYear}, ${supply / emissionCurve.coinsInOneErgo}, ${currentSupply.toDouble / emissionCurve.coinsInOneErgo}")
    loop(height + 1, supply + currentSupply)
  }

  loop(0, 0)
}