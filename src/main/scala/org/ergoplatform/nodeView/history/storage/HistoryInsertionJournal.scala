package org.ergoplatform.nodeView.history.storage

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream}
import java.nio.charset.StandardCharsets
import java.security.MessageDigest

/** Private redo record; existing history objects and index rows retain their byte formats. */
private[storage] object HistoryInsertionJournal {
  // Ordinary history index keys are 32-byte hashes/constants. This distinct key is also explicitly reserved.
  def key: Array[Byte] = "history-storage/pending-insertion/v1".getBytes(StandardCharsets.US_ASCII)
  def isReserved(bytes: Array[Byte]): Boolean = java.util.Arrays.equals(bytes, key)

  final case class Intent(objects: Vector[(Array[Byte], Array[Byte])], indexes: Vector[(Array[Byte], Array[Byte])])

  private val Magic = 0x4853494a
  private val Version = 1
  private val DigestLength = 32
  private def digest(bytes: Array[Byte]): Array[Byte] = MessageDigest.getInstance("SHA-256").digest(bytes)

  def encode(intent: Intent): Array[Byte] = {
    val bytes = new ByteArrayOutputStream()
    val out = new DataOutputStream(bytes)
    out.writeInt(Magic)
    out.writeInt(Version)
    def rows(values: Vector[(Array[Byte], Array[Byte])]): Unit = {
      out.writeInt(values.size)
      values.foreach { case (key, value) =>
        out.writeInt(key.length)
        out.write(key)
        out.writeInt(value.length)
        out.write(value)
      }
    }
    rows(intent.objects)
    rows(intent.indexes)
    out.flush()
    val payload = bytes.toByteArray
    payload ++ digest(payload)
  }

  def decode(bytes: Array[Byte]): Intent = {
    require(bytes.length >= 16 + DigestLength, "Incomplete history insertion journal")
    val payload = bytes.dropRight(DigestLength)
    require(MessageDigest.isEqual(digest(payload), bytes.takeRight(DigestLength)), "History insertion journal checksum mismatch")
    val in = new DataInputStream(new ByteArrayInputStream(payload))
    require(in.readInt() == Magic, "Unknown history insertion journal magic")
    require(in.readInt() == Version, "Unsupported history insertion journal version")
    def field(): Array[Byte] = {
      val length = in.readInt()
      require(length >= 0 && length <= in.available(), "Invalid history insertion journal field length")
      val value = new Array[Byte](length)
      in.readFully(value)
      value
    }
    def rows(): Vector[(Array[Byte], Array[Byte])] = {
      val count = in.readInt()
      require(count >= 0 && count <= in.available() / 8, "Invalid history insertion journal row count")
      Vector.fill(count)(field() -> field())
    }
    val intent = Intent(rows(), rows())
    require(in.available() == 0, "Trailing history insertion journal bytes")
    require(intent.objects.forall(_._1.length == 32), "Invalid history object key")
    require(intent.indexes.forall(row => !isReserved(row._1)), "Reserved history index key")
    intent
  }
}
