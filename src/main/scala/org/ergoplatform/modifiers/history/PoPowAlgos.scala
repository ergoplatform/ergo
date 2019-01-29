package org.ergoplatform.modifiers.history

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import scorex.util.ModifierId

object PoPowAlgos {

  def maxLevelOf(header: Header): Int = {
    if (!header.isGenesis) {
      def log2(x: Double) = math.log(x) / math.log(2)
      val requiredTarget = org.ergoplatform.mining.q / RequiredDifficulty.decodeCompactBits(header.nBits)
      val realTarget = header.powSolution.d
      val level = log2(requiredTarget.doubleValue) - log2(realTarget.doubleValue)
      level.toInt
    } else {
      Int.MaxValue
    }
  }

  /**
    * Computes interlinks vector for the next level after `prevHeader`.
    */
  def updateInterlinks(prevHeader: Header, prevInterlinks: Seq[ModifierId]): Seq[ModifierId] = {
    if (!prevHeader.isGenesis) {
      assert(prevInterlinks.nonEmpty, "Interlinks vector could not be empty in case of non-genesis header")
      val genesis = prevInterlinks.head
      val tail = prevInterlinks.tail
      val prevLevel = maxLevelOf(prevHeader)
      if (prevLevel > 0) {
        (genesis +: tail.dropRight(prevLevel)) ++ Seq.fill(prevLevel)(prevHeader.id)
      } else {
        prevInterlinks
      }
    } else {
      Seq(prevHeader.id)
    }
  }

}
