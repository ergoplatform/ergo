package org.ergoplatform.wallet

object Constants {

  // part of protocol, do not change
  val KeyLen = 32
  // part of protocol, do not change
  val ModifierIdLength = 32

  val Encoding = "UTF-8"

  val BitcoinSeed: Array[Byte] = "Bitcoin seed".getBytes(Encoding)

  // SimplePayments application identifier
  val PaymentsAppId: Short = 10

  // Application which is checking mining rewards
  val MiningRewardsQueueId: Short = 9
}
