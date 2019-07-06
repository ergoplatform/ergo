package org.ergoplatform.db

import akka.util.ByteString
import org.iq80.leveldb.DB

/**
  * A LevelDB wrapper providing a convenient db interface.
  */
final class LDBKVStore(protected val db: DB) extends KVStore {

  def update(toInsert: Seq[(ByteString, ByteString)], toRemove: Seq[ByteString]): Unit = {
    val batch = db.createWriteBatch()
    try {
      toInsert.foreach { case (k, v) => batch.put(k.toArray, v.toArray) }
      toRemove.foreach(x => batch.delete(x.toArray))
      db.write(batch)
    } finally {
      batch.close()
    }
  }

  def put(values: (ByteString, ByteString)*): Unit = update(values, Seq.empty)

  def delete(keys: ByteString*): Unit = update(Seq.empty, keys)

}
