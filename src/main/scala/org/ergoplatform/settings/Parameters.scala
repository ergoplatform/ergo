package org.ergoplatform.settings


/**
  * System parameters which could be readjusted via collective miners decision.
  */
object Parameters {

  // Max total computation cost of a block.
  val MaxBlockCost: Long = 1000000

  // Max size of transactions section of a block.
  val MaxBlockSize: Int = 512 * 1024

  val Kdefault = 12
  val Kmax = 24
  val Kmin = 0
  val Kstep = 1

  /** Cost of storing 1 byte per block, in nanoErgs
    * with default value of 12 nanoErgs, storage cost for an (ordinary) output of 80 bytes would be ~1.01 Ergo per 4 years
    * max should be about 24 probably
    */
  val K: Long = Kdefault

  /** To prevent creation of dust which is not profitable to charge storage fee from, we have this min-value per-byte
    * parameter.
    */
  val MinValuePerByte: Long = Constants.BlocksPerHour * 24 * Kdefault
  val MinValueStep = 100
  val MinValueMin = 0
}
