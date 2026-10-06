package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ExtraIndexTypeId
import scorex.util.{ModifierId, bytesToId}

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
  * Rows carry no payload: everything the entry holds is already in the key, and the box
  * itself (id, value, serialized size) is resolvable through the always-maintained
  * [[NumericBoxIndex]] at claim time.
  *
  * @param creationHeight - creation height of the box (its R3 height)
  * @param globalIndex    - serial number of the box counting from genesis box
  */
class StorageRentBox(val creationHeight: Int,
                     val globalIndex: Long) extends ExtraIndex {

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

  /**
    * Reconstruct an entry from its index key (the row value is empty, see class doc).
    */
  def fromKey(key: Array[Byte]): StorageRentBox = {
    val bb = ByteBuffer.wrap(key)
    bb.get() // marker
    new StorageRentBox(bb.getInt, bb.getLong)
  }

  def apply(box: IndexedErgoBox): StorageRentBox =
    new StorageRentBox(box.box.creationHeight, box.globalIndex)
}
