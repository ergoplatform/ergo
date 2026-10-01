package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{ExtraIndexTypeId, fastIdToBytes}
import org.ergoplatform.serialization.ErgoSerializer
import scorex.util.{ModifierId, bytesToId}
import scorex.util.serialization.{Reader, Writer}

import java.nio.ByteBuffer

/**
  * Storage-rent eligibility index entry: one row per currently-unspent box, ordered by the
  * box's creation height. Mirrors the `UNSPENT_BY_CREATION_HEIGHT` table of the Rust
  * reference node (`ergo-indexer/src/store/storage_rent.rs`).
  *
  * Rows are keyed `marker || creationHeight || globalIndex` (big-endian, so the natural key
  * order is `creationHeight ASC, globalIndex ASC`), which allows an ascending range scan for
  * all unspent boxes old enough to be rent-eligible. The entry is inserted on output creation
  * and deleted on spend (rollback re-derives both from the unchanged [[IndexedErgoBox]] rows).
  *
  * @param creationHeight - creation height of the box (its R3 height)
  * @param globalIndex    - serial number of the box counting from genesis box
  * @param boxId          - id of the box
  * @param value          - monetary value of the box
  * @param bytesLen       - canonical serialized length of the box (needed for the storage fee)
  */
class StorageRentBox(val creationHeight: Int,
                     val globalIndex: Long,
                     val boxId: ModifierId,
                     val value: Long,
                     val bytesLen: Int) extends ExtraIndex {

  override lazy val id: ModifierId = bytesToId(serializedId)

  /**
    * Index key: marker byte, then creation height and global box index, both big-endian.
    */
  override def serializedId: Array[Byte] = StorageRentBox.key(creationHeight, globalIndex)
}

object StorageRentBox {

  val extraIndexTypeId: ExtraIndexTypeId = 40.toByte

  /**
    * Marker byte prefixing every index key, to give the rent index its own namespace inside
    * the shared extra-index store (other keys are 32-byte ids or short progress keys).
    */
  val KeyMarker: Byte = 14.toByte

  val KeyLength: Int = 1 + 4 + 8

  def key(creationHeight: Int, globalIndex: Long): Array[Byte] =
    ByteBuffer.allocate(KeyLength).put(KeyMarker).putInt(creationHeight).putLong(globalIndex).array

  def apply(box: IndexedErgoBox): StorageRentBox =
    new StorageRentBox(box.box.creationHeight, box.globalIndex, box.id, box.box.value, box.box.bytes.length)
}

object StorageRentBoxSerializer extends ErgoSerializer[StorageRentBox] {

  override def serialize(srb: StorageRentBox, w: Writer): Unit = {
    w.putInt(srb.creationHeight)
    w.putLong(srb.globalIndex)
    w.putBytes(fastIdToBytes(srb.boxId))
    w.putLong(srb.value)
    w.putInt(srb.bytesLen)
  }

  override def parse(r: Reader): StorageRentBox = {
    val creationHeight: Int = r.getInt()
    val globalIndex: Long = r.getLong()
    val boxId: ModifierId = bytesToId(r.getBytes(32))
    val value: Long = r.getLong()
    val bytesLen: Int = r.getInt()
    new StorageRentBox(creationHeight, globalIndex, boxId, value, bytesLen)
  }
}
