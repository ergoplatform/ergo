package org.ergoplatform.db

import akka.util.ByteString
import org.iq80.leveldb.{DB, ReadOptions}

import scala.collection.mutable

/**
  * A LevelDB wrapper providing a convenient db interface.
  */
final class LDBKVStore(db: DB) extends KVStore[ByteString, ByteString] {

  override def get(key: ByteString): Option[ByteString] =
    Option(db.get(key.toArray)).map(ByteString.apply)

  override def getAll: Iterable[(ByteString, ByteString)] = {
    val ro = new ReadOptions()
    ro.snapshot(db.getSnapshot)
    val iter = db.iterator(ro)
    try {
      iter.seekToFirst()
      val bf = mutable.ArrayBuffer.empty[(ByteString, ByteString)]
      while (iter.hasNext) {
        val next = iter.next()
        bf += (ByteString(next.getKey) -> ByteString(next.getValue))
      }
      bf
    } finally {
      iter.close()
      ro.snapshot().close()
    }
  }

  override def close(): Unit = db.close()

  def update(toInsert: Iterable[(ByteString, ByteString)], toRemove: Iterable[ByteString]): Unit = {
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
