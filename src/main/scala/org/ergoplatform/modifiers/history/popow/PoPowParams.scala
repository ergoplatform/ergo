package org.ergoplatform.modifiers.history.popow

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
case class PoPowParams(m: Int, k: Int, continuous: Boolean)

