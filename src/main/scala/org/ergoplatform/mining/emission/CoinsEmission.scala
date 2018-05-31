package org.ergoplatform.mining.emission

import org.ergoplatform.settings.MonetarySettings

import scala.annotation.tailrec

/**
  * Ergo coin emission curve.
  *
  * Mainnet properties:
  * 100000000 parts of one coin
  * block every 2 minutes
  * fixed rate 75 coins during first 2 years
  * reward reduction for 3 coins every 3 month after that
  * 19710000 coins after the first year
  * 97739925 coins total
  *
  * @param settings - network settings
  */
class CoinsEmission(val settings: MonetarySettings) {

  val coinsInOneErgo: Long = 100000000

  lazy val (coinsTotal, blocksTotal) = {
    @tailrec
    def loop(height: Int, acc: Long): (Long, Int) = {
      val currentRate = emissionAtHeight(height)
      if (currentRate > 0) {
        loop(height + 1, acc + currentRate)
      } else {
        (acc, height - 1)
      }
    }

    loop(0, 0)
  }


  def emissionAtHeight(h: Long): Long = {
    if (h < settings.fixedRatePeriod) {
      settings.fixedRate
    } else {
      val epoch = 1 + (h - settings.fixedRatePeriod) / settings.epochLength
      Math.max(settings.fixedRate - settings.oneEpochReduction * epoch, 0)
    }
  }.ensuring(_ >= 0, s"Negative at $h")

}

