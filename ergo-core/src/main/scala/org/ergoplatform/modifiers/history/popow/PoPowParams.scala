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
  def apply(m: Int, k: Int, continuous: Boolean): Try[PoPowParams] = Try {
    require(m >= 1, s"$m < 1")
    require(k >= 1, s"$k < 1")
    new PoPowParams(m, k, continuous, Math.addExact(m, k))
  }
}

