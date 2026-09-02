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
  * @param minChainLength - minimal length of a proof's header chain acceptable for the params, always m + k.
  *                     Kept as groundwork for the proof-length checks of the NiPoPoW parsing rework (#2461),
  *                     not read by validation yet
  *
  */
final class PoPowParams private (val m: Int, val k: Int, val continuous: Boolean, val minChainLength: Int)

object PoPowParams {
  def isValid(m: Int, k: Int): Boolean =
    m >= 1 && k >= 1 && m.toLong + k.toLong <= Int.MaxValue

  def apply(m: Int, k: Int, continuous: Boolean): Try[PoPowParams] = Try {
    require(isValid(m, k), s"Invalid NiPoPoW parameters: m=$m, k=$k")
    new PoPowParams(m, k, continuous, m + k)
  }
}

