package org.ergoplatform.wallet

import supertagged.TaggedType

object Constants {
  object ScanId extends TaggedType[Short]
  type ScanId = ScanId.Type

  // part of the protocol, do not change
  val SecretKeyLength = 32

  // part of the protocol, do not change
  val ModifierIdLength = 32

  val Encoding = "UTF-8"

  val BitcoinSeed: Array[Byte] = "Bitcoin seed".getBytes(Encoding)

  // Identifiers for system applications of Ergo node
  // Ids below 9 are reserved. Ids from 11 (inclusive) are for user scans

  // SimplePayments scan identifier
  val PaymentsScanId: ScanId = ScanId @@ 10.toShort

  // Scan which is checking mining rewards
  val MiningScanId: ScanId = ScanId @@ 9.toShort

}
