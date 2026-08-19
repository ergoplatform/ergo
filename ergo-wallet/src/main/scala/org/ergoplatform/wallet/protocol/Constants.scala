package org.ergoplatform.wallet.protocol

object Constants {

  val HashLength: Int = 32

  val BlocksPerHour = 30

  val BlocksPerDay: Int = BlocksPerHour * 24

  val BlocksPerWeek: Int = BlocksPerDay * 7

  val BlocksPerMonth: Int = BlocksPerDay * 30

  val BlocksPerYear: Int = BlocksPerDay * 365

  //For how many blocks a box could be put into the state with no paying storage rent.
  //4 years
  val StoragePeriod: Int = 4 * BlocksPerYear

  val StorageContractCost: Long = 50

  val StorageIndexVarId: Byte = Byte.MaxValue

  /**
    * Block (protocol) version from which the storage-rent repairs apply
    * (matches `Header.Interpreter70Version` on the node side):
    *
    *  - the storage fee is computed in 64-bit arithmetic instead of the
    *    historical wrapping `Int` multiplication, and
    *  - EIP-27 re-emission tokens carried by an expired box are dropped
    *    from the recreated box, with 1 nanoErg per token released from the
    *    recreation floor to pay the burn obligation.
    *
    * See `ErgoInterpreter.checkExpiredBox`.
    */
  val StorageRentRepairsBlockVersion: Byte = 5
}
