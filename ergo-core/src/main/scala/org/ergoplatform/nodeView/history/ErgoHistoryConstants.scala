package org.ergoplatform.nodeView.history

import org.ergoplatform.ErgoLikeContext

object ErgoHistoryConstants {
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
}
