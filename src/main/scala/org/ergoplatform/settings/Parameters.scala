package org.ergoplatform.settings

object Parameters {
  // Max total computation cost of a block.
  val MaxBlockCost: Long = 1000000

  // Max size of transactions section of a block.
  val MaxBlockSize: Int = 512 * 1024

  // Cost of storing 1 byte per block, in nanoErgs
  // with default value of 12 nanoErgs, storage cost for an (ordinary) output of 80 bytes would be ~1.01 Ergo per 4 years
  // max should be about 24 probably
  val K: Long = 12
}
