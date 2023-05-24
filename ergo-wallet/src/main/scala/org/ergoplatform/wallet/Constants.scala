package org.ergoplatform.wallet

import supertagged.TaggedType

object Constants {
  object ScanId extends TaggedType[Short]
  type ScanId = ScanId.Type

  // Identifiers for system applications of Ergo node
  // Ids below 9 are reserved. Ids from 11 (inclusive) are for user scans

  /** SimplePayments scan identifier */
  val PaymentsScanId: ScanId = ScanId @@ 10.toShort

  /** Scan which is checking mining rewards */
  val MiningScanId: ScanId = ScanId @@ 9.toShort

}
