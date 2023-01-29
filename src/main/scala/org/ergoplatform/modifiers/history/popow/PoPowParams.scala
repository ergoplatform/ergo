package org.ergoplatform.modifiers.history.popow

/**
  * NiPoPoW proof params from the KMZ17 paper
  *
  * @param m - minimal superchain length
  * @param k - suffix length
  * @param continuous - // todo:
  */
case class PoPowParams(m: Int, k: Int, continuous: Boolean)

