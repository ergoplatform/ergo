package org.ergoplatform.db

import org.rocksdb.{ReadOptions, RocksDB}

import scala.collection.mutable

trait KVStore extends AutoCloseable {

  type K = Array[Byte]
  type V = Array[Byte]

  protected val db: RocksDB

  def get(key: K): Option[V] =
    Option(db.get(key))

  def getAll(cond: (K, V) => Boolean): Seq[(K, V)] = {
    val ro = new ReadOptions()
    ro.setSnapshot(db.getSnapshot)
    val iter = db.newIterator(ro)
    try {
      iter.seekToFirst()
      val bf = mutable.ArrayBuffer.empty[(K, V)]
      while (iter.isValid) {
        iter.next()
        val key = iter.key()
        val value = db.get(ro, key)
        if (cond(key, value)) bf += (key -> value)
      }
      bf.toList
    } finally {
      iter.close()
      ro.snapshot().close()
    }
  }

  def getAll: Seq[(K, V)] = getAll((_, _) => true)

  def getOrElse(key: K, default: => V): V =
    get(key).getOrElse(default)

  def get(keys: Seq[K]): Seq[(K, Option[V])] = {
    val bf = mutable.ArrayBuffer.empty[(K, Option[V])]
    keys.foreach(k => bf += (k -> get(k)))
    bf
  }

  override def close(): Unit = db.close()

}
