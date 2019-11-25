package org.ergoplatform.db

import org.ergoplatform.utils.ByteArrayUtils
import org.iq80.leveldb.DB


/**
  * A LevelDB wrapper providing a convenient db interface.
  */
class LDBKVStore(protected val db: DB) extends KVStore {

  def update(toInsert: Seq[(K, V)], toRemove: Seq[K]): Unit = {
    val batch = db.createWriteBatch()
    try {
      toInsert.foreach { case (k, v) => batch.put(k, v) }
      toRemove.foreach(batch.delete)
      db.write(batch)
    } finally {
      batch.close()
    }
  }

  def insert(values: Seq[(K, V)]): Unit = update(values, Seq.empty)

  def remove(keys: Seq[K]): Unit = update(Seq.empty, keys)

  /**
    * Get last key within some range by used comparator. Could be useful for applications with sequential ids.
    */
  def lastKeyInRange(first: Array[Byte], last:Array[Byte]): Option[Array[Byte]] = {
    val i = db.iterator()
    i.seek(first)
    if(i.hasNext) {
      val key = i.peekNext().getKey
      if(ByteArrayUtils.compare(key, last) < 0) Some(key) else None
    } else {
      None
    }
  }

}
