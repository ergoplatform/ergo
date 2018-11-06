package org.ergoplatform.settings


/**
  * System parameters which could be readjusted via collective miners decision.
  */
object Parameters {

  // Max size of transactions section of a block.
  lazy val MaxBlockSize: Int = parametersTable(MaxBlockSizeIncrease)

  // Max total computation cost of a block.
  lazy val MaxBlockCost: Long = parametersTable(MaxBlockCostIncrease)

  val Kdefault = 1250000
  val Kmax = 2500000
  val Kmin = 0
  val Kstep = 50000

  /** Cost of storing 1 byte per Constants.StoragePeriod blocks, in nanoErgs
    */
  lazy val K: Int = parametersTable(KIncrease)

  /** To prevent creation of dust which is not profitable to charge storage fee from, we have this min-value per-byte
    * parameter.
    */
  val MinValuePerByteDefault: Int = 30 * 12
  val MinValueStep = 10
  val MinValueMin = 0
  val MinValueMax = 100000000 //0.1 Erg

  lazy val MinValuePerByte: Int = parametersTable(MinValuePerByteIncrease)

  //A vote for nothing
  val NoParameter = 0: Byte

  //Parameter identifiers
  val KIncrease = 1: Byte
  val KDecrease = -KIncrease

  val MinValuePerByteIncrease = 2: Byte
  val MinValuePerByteDecrease = -MinValuePerByteIncrease

  val MaxBlockSizeIncrease = 3: Byte
  val MaxBlockSizeDecrease = -MaxBlockSizeIncrease

  val MaxBlockCostIncrease = 4: Byte
  val MaxBlockCostDecrease = -MaxBlockCostIncrease

  val parametersTable: Map[Byte, Int] = Map(
    KIncrease -> Kdefault,
    MinValuePerByteIncrease -> MinValuePerByteDefault,
    MaxBlockSizeIncrease -> 512 * 1024,
    MaxBlockCostIncrease -> 1000000
  )

 /*
  def changeParameter(paramId: Byte) = {
    ???
  }*/
}
