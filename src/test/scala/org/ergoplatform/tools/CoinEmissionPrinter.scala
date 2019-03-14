package org.ergoplatform.tools

import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.settings.{Constants, MonetarySettings}

import scala.annotation.tailrec

object CoinEmissionPrinter extends App {

  val emissionCurve = new EmissionRules(MonetarySettings(10080, 2160, 7500000000L, 300000000, 720, 750000000L))
  val blocksPerHour = 30

  //  // Number of coins issued after slow start period
  //  lazy val SlowStartFinalSupply: Long = (0 until emissionCurve.SlowStartPeriod)
  //    .map(h => emissionCurve.emissionAtHeight(h)).sum
  val blocksPerYear = 365 * 24 * blocksPerHour

  // 100215692 coins total supply
  lazy val TotalSupply: Long = (1 to emissionCurve.blocksTotal).map(h => emissionCurve.emissionAtHeight(h)).sum

  // 26736208 coins first year supply
  lazy val FirstYearSupply: Long = (1 to blocksPerYear).map(h => emissionCurve.emissionAtHeight(h)).sum

  println(s"BlocksTotal = ${emissionCurve.blocksTotal}")
  println(s"BlocksPerYear = ${blocksPerYear}")
  //  println(s"SlowStartPeriod = ${emissionCurve.SlowStartPeriod}")
  //  println(s"SlowStartFinalSupply = ${SlowStartFinalSupply / Constants.CoinsInOneErgo}")
  //  println(s"SlowStartFinalRate = ${emissionCurve.emissionAtHeight(emissionCurve.SlowStartPeriod) / Constants.CoinsInOneErgo}")
  println(s"EndRate = ${emissionCurve.emissionAtHeight(emissionCurve.blocksTotal).toDouble / Constants.CoinsInOneErgo}")
  println("First year supply")
  println(25000000)
  println(FirstYearSupply / Constants.CoinsInOneErgo)
  println("Total supply")
  println(100000000)
  println(TotalSupply / Constants.CoinsInOneErgo)

  println("================")
  println("age (years), total coins, current rate")

  @tailrec
  def loop(height: Int, supply: Long): Unit = if (height < emissionCurve.blocksTotal) {
    val currentSupply = emissionCurve.emissionAtHeight(height)
    assert(supply + currentSupply == emissionCurve.issuedCoinsAfterHeight(height),
      s"$height: $supply == ${emissionCurve.issuedCoinsAfterHeight(height - 1)} => " +
        s"${supply + currentSupply} == ${emissionCurve.issuedCoinsAfterHeight(height)}")
    if (height % (blocksPerHour * 60) == 0) println(s"${height.toDouble / blocksPerYear}, ${supply / Constants.CoinsInOneErgo}, ${currentSupply.toDouble / Constants.CoinsInOneErgo}")
    loop(height + 1, supply + currentSupply)
  }

  loop(0, 0)
}