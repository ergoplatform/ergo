package org.ergoplatform.settings

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.nodeView.history.ErgoHistory.Difficulty


object Constants {
  val hashLength: Int = 32
  val MaxTarget: BigInt = BigInt(1, Array.fill(hashLength)((-1).toByte))
  val InitialDifficulty: Difficulty = BigInt(1)
  val InitialNBits: Long = RequiredDifficulty.encodeCompactBits(InitialDifficulty)
  val ModifierIdSize: Int = hashLength
  val MaxTransactionCost: Long = 1000000    //todo: move to config
}
