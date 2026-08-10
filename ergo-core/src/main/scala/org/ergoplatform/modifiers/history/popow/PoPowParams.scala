package org.ergoplatform.modifiers.history.popow

import scala.util.Try

/**
  * NiPoPoW proof params from the KMZ17 paper
  *
  * @param m - minimal superchain length
  * @param k - suffix length
  * @param continuous - there are two proof modes, for continuous use and one-shot use. Continuous use means
  *                     validating and adding headers is possible after the proof (which requires for headers needed
  *                     to calculate difficulty to be added to the proof). One-shot use means using the proof to just
  *                     to prove that a best chain contains some header (e.g. to work with a transaction corresponding
  *                     to the block header)
  *
  */
final class PoPowParams private (val m: Int, val k: Int, val continuous: Boolean, val minChainLength: Int)

object PoPowParams {
  final val MaxProofElements: Int = 20000

  def isValidM(m: Int): Boolean = m >= 1 && m <= MaxProofElements

  def isValidK(k: Int): Boolean = k >= 1 && k <= MaxProofElements

  def isValid(m: Int, k: Int): Boolean =
    isValidM(m) && isValidK(k) && m.toLong + k.toLong <= Int.MaxValue

  def areValid(m: Int, k: Int): Boolean = isValid(m, k)

  def requireValidM(m: Int): Unit =
    require(isValidM(m), s"m parameter $m must be in 1..=$MaxProofElements")

  def requireValidK(k: Int): Unit =
    require(isValidK(k), s"k parameter $k must be in 1..=$MaxProofElements")

  def requireValid(m: Int, k: Int): Unit = {
    requireValidM(m)
    requireValidK(k)
    require(m.toLong + k.toLong <= Int.MaxValue,
      s"NiPoPoW parameter sum exceeds Int range: m=$m, k=$k")
  }

  def apply(m: Int, k: Int, continuous: Boolean): Try[PoPowParams] = Try {
    requireValid(m, k)
    new PoPowParams(m, k, continuous, m + k)
  }
}

