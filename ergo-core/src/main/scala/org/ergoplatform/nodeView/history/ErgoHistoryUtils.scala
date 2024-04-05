package org.ergoplatform.nodeView.history

import org.ergoplatform.ErgoLikeContext
import org.ergoplatform.modifiers.history.header.Header

/**
  * Repository for types, constants and functions related to blockchain database (ErgoHistory)
  */
object ErgoHistoryUtils {
  /**
   * Type for time, represents machine-specific timestamp of a transaction
   * or block section, as milliseconds passed since beginning of UNIX
   * epoch on the machine
   */
  type Time = Long

  type Height = ErgoLikeContext.Height // Int
  type Score = BigInt
  type Difficulty = BigInt
  type NBits = Long

  val CharsetName = "UTF-8"

  val EmptyHistoryHeight: Int = 0
  val GenesisHeight: Int = EmptyHistoryHeight + 1 // first block has height == 1
  /**
   * Minimal superchain length ('m' in KMZ17 paper) value used in NiPoPoW proofs for bootstrapping
   */
  val P2PNipopowProofM = 6

  /**
   * Suffix length ('k' in KMZ17 paper) value used in NiPoPoW proofs for bootstrapping
   */
  val P2PNipopowProofK = 10
  def heightOf(headerOpt: Option[Header]): Int = headerOpt.map(_.height).getOrElse(EmptyHistoryHeight)
}
