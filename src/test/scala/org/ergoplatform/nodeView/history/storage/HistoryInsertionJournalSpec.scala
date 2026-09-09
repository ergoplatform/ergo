package org.ergoplatform.nodeView.history.storage

import java.nio.ByteBuffer
import java.security.MessageDigest
import org.ergoplatform.utils.ErgoCorePropertyTest

class HistoryInsertionJournalSpec extends ErgoCorePropertyTest {
  import HistoryInsertionJournal.{Intent, decode, encode}

  private val emptyIntent = Intent(Vector.empty, Vector.empty)
  private def rejects(record: Array[Byte], reason: String): Unit =
    intercept[IllegalArgumentException](decode(record)).getMessage should include(reason)
  private def signed(payload: Array[Byte]): Array[Byte] =
    payload ++ MessageDigest.getInstance("SHA-256").digest(payload)
  private def changedInt(record: Array[Byte], offset: Int, value: Int): Array[Byte] = {
    val payload = record.dropRight(32)
    ByteBuffer.wrap(payload).putInt(offset, value)
    signed(payload)
  }

  property("journal round trip preserves exact object and index byte rows") {
    val intent = Intent(Vector(Array.fill[Byte](32)(1) -> Array[Byte](2, 3)),
      Vector(Array.fill[Byte](32)(4) -> Array[Byte](5, 6)))
    val decoded = decode(encode(intent))
    decoded.objects.map { case (k, v) => k.toSeq -> v.toSeq } shouldBe
      intent.objects.map { case (k, v) => k.toSeq -> v.toSeq }
    decoded.indexes.map { case (k, v) => k.toSeq -> v.toSeq } shouldBe
      intent.indexes.map { case (k, v) => k.toSeq -> v.toSeq }
  }

  property("journal rejects truncated bytes and checksum changes") {
    val record = encode(emptyIntent)
    rejects(record.take(20), "Incomplete history insertion journal")
    record(record.length - 1) = (record.last ^ 1).toByte
    rejects(record, "checksum mismatch")
  }

  property("journal rejects unknown magic and version with otherwise valid checksums") {
    rejects(changedInt(encode(emptyIntent), 0, 0), "Unknown history insertion journal magic")
    rejects(changedInt(encode(emptyIntent), 4, 2), "Unsupported history insertion journal version")
  }

  property("journal bounds row counts and field lengths against available bytes") {
    rejects(changedInt(encode(emptyIntent), 8, -1), "row count")
    rejects(changedInt(encode(emptyIntent), 8, Int.MaxValue), "row count")
    val row = encode(Intent(Vector(Array.fill[Byte](32)(1) -> Array[Byte](2)), Vector.empty))
    rejects(changedInt(row, 12, -1), "field length")
    rejects(changedInt(row, 12, Int.MaxValue), "field length")
  }

  property("journal rejects trailing bytes, non-object keys and reserved index keys") {
    rejects(signed(encode(emptyIntent).dropRight(32) ++ Array[Byte](0)), "Trailing history insertion journal bytes")
    rejects(encode(Intent(Vector(Array[Byte](1) -> Array[Byte](2)), Vector.empty)), "Invalid history object key")
    rejects(encode(Intent(Vector.empty, Vector(HistoryInsertionJournal.key -> Array[Byte](1)))), "Reserved history index key")
  }
}
