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

  // Legacy putShort and putInt both use signed ZigZag VLQ. An Int outside
  // the Short range is an unambiguous escape, even for wrapped legacy IDs.
  private val LongScanIdMarker: Int = Short.MaxValue.toInt + 1

  @inline def isLegacyShortScanId(scanId: Long): Boolean =
    scanId >= Short.MinValue && scanId <= Short.MaxValue

  def putScanId(w: Writer, scanId: ScanId): Unit = {
    if (isLegacyShortScanId(scanId)) {
      w.putShort(scanId.toShort)
    } else {
      w.putInt(LongScanIdMarker)
      w.putLong(scanId)
    }
  }

  def getScanId(r: Reader): ScanId = {
    val first = r.getInt()
    if (first == LongScanIdMarker) ScanId @@ r.getLong()
    else {
      require(isLegacyShortScanId(first.toLong), s"Invalid scan id prefix: $first")
      ScanId @@ first.toLong
    }
  }
}
