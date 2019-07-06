package org.ergoplatform.db

import akka.util.ByteString
import org.iq80.leveldb.{DB, ReadOptions}

import scala.collection.mutable

trait KVStore extends AutoCloseable {

  type K = ByteString
  type V = ByteString

  protected val db: DB

  def get(key: ByteString): Option[ByteString] =
    Option(db.get(key.toArray)).map(ByteString.apply)

  def getAll: Seq[(ByteString, ByteString)] = {
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

  def getOrElse(key: K, default: => V): V =
    get(key).getOrElse(default)

  def get(keys: Iterable[K]): Iterable[(K, Option[V])] = {
    val bf = mutable.ArrayBuffer.empty[(K, Option[V])]
    keys.foreach(k => bf += (k -> get(k)))
    bf
  }

  override def close(): Unit = db.close()

}
