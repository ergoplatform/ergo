package org.ergoplatform.wallet

import supertagged.TaggedType

object Constants {
  object ApplicationId extends TaggedType[Short]
  type ApplicationId = ApplicationId.Type

  // part of the protocol, do not change
  val SecretKeyLength = 32

  // part of the protocol, do not change
  val ModifierIdLength = 32

  val Encoding = "UTF-8"

  val BitcoinSeed: Array[Byte] = "Bitcoin seed".getBytes(Encoding)

  // Identifiers for system applications of Ergo node
  // Ids below 9 are reserved. Ids from 11 (inclusive) are for user applications

  // SimplePayments application identifier
  val PaymentsAppId: ApplicationId = ApplicationId @@ 10.toShort

  // Application which is checking mining rewards
  val MiningRewardsAppId: ApplicationId = ApplicationId @@ 9.toShort

}
