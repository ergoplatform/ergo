package org.ergoplatform.db

import org.rocksdb.{RocksDB, WriteBatch, WriteOptions}

/**
  * A LevelDB wrapper providing a convenient db interface.
  */
final class LDBKVStore(protected val db: RocksDB) extends KVStore {

  def update(toInsert: Seq[(K, V)], toRemove: Seq[K]): Unit = {
    val wo = new WriteOptions()
    val batch = new WriteBatch()
    try {
      toInsert.foreach { case (k, v) => batch.put(k, v) }
      toRemove.foreach(batch.delete)
      db.write(wo, batch)
    } finally {
      batch.close()
    }
  }

  def insert(values: Seq[(K, V)]): Unit = update(values, Seq.empty)

  def remove(keys: Seq[K]): Unit = update(Seq.empty, keys)

}
