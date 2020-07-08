package org.ergoplatform.tools

import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.settings.{Constants, MonetarySettings}

import scala.annotation.tailrec

object CoinEmissionPrinter extends App {

  val emissionCurve = new EmissionRules(MonetarySettings(525600, 64800, 75000000000L, 3000000000L, 720, 7500000000L))
  val blocksPerHour = 30

  //  // Number of coins issued after slow start period
  //  lazy val SlowStartFinalSupply: Long = (0 until emissionCurve.SlowStartPeriod)
  //    .map(h => emissionCurve.emissionAtHeight(h)).sum
  val blocksPerYear = 365 * 24 * blocksPerHour

  // 97739925 coins total supply
  lazy val TotalSupply: Long = (1 to emissionCurve.blocksTotal).map(h => emissionCurve.emissionAtHeight(h)).sum

  // 19710000 coins first year supply
  lazy val FirstYearSupply: Long = (1 to blocksPerYear).map(h => emissionCurve.emissionAtHeight(h)).sum

  println(s"BlocksTotal = ${emissionCurve.blocksTotal}")
  println(s"BlocksPerYear = $blocksPerYear")
  println(s"EndRate = ${emissionCurve.emissionAtHeight(emissionCurve.blocksTotal).toDouble / Constants.CoinsInOneErgo}")
  println("First year supply")
  println(20000000)
  println(FirstYearSupply / Constants.CoinsInOneErgo)
  println("Total supply")
  println(100000000)
  println(TotalSupply / Constants.CoinsInOneErgo)

  println("================")
  println("age (years), foundation coins, miners coins, coins total")
  println("0, 0, 0, 0")

  @tailrec
  def loop(height: Int, totalMinersReward: Long, totalFoundationReward: Long): Unit = if (height <= emissionCurve.blocksTotal) {
    val currentMinersReward = emissionCurve.minersRewardAtHeight(height)
    val currentFoundationReward = emissionCurve.foundationRewardAtHeight(height)
    val newTotalMinersReward = totalMinersReward + currentMinersReward
    val newTotalFoundationReward = totalFoundationReward + currentFoundationReward

    if (emissionCurve.foundationRewardAtHeight(height - 1) != currentFoundationReward ||
      emissionCurve.minersRewardAtHeight(height - 1) != currentMinersReward ||
      height == emissionCurve.blocksTotal) {
      // rate changed, print points
      val supply = newTotalMinersReward + newTotalFoundationReward
      println(s"" +
        s"${height.toDouble / blocksPerYear}," +
        s" ${newTotalFoundationReward / Constants.CoinsInOneErgo}," +
        s" ${newTotalMinersReward / Constants.CoinsInOneErgo}," +
        s" ${supply / Constants.CoinsInOneErgo}")
    }
    loop(height + 1, newTotalMinersReward, newTotalFoundationReward)
  }

  loop(1, 0, 0)
}
