package org.ergoplatform.wallet

import scorex.util.serialization.{Reader, Writer}
import supertagged.TaggedType

object Constants {
  object ScanId extends TaggedType[Long]
  type ScanId = ScanId.Type

  // Identifiers for system applications of Ergo node
  // Ids below 9 are reserved. Ids from 11 (inclusive) are for user scans

  /** SimplePayments scan identifier */
  val PaymentsScanId: ScanId = ScanId @@ 10L

  /** Scan which is checking mining rewards */
  val MiningScanId: ScanId = ScanId @@ 9L

  /**
    * Non-negative scan IDs within the legacy `Short` range (`0 .. Short.MaxValue`)
    * use the legacy 2-byte encoding so existing wallet databases remain compatible.
    * Scan IDs outside `0 .. Short.MaxValue` are prefixed with `Short.MinValue` (`0x8000`)
    * followed by their 8-byte `Long` representation.
    */
  @inline def isLegacyShortScanId(scanId: Long): Boolean =
    scanId >= 0L && scanId <= Short.MaxValue

  def putScanId(w: Writer, scanId: ScanId): Unit = {
    if (isLegacyShortScanId(scanId)) {
      w.putShort(scanId.toShort)
    } else {
      w.putShort(Short.MinValue)
      w.putLong(scanId)
    }
  }

  def getScanId(r: Reader): ScanId = {
    val first = r.getShort()
    val id = if (first == Short.MinValue) {
      r.getLong()
    } else {
      first.toLong
    }
    ScanId @@ id
  }

}
