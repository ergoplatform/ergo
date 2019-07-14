package org.ergoplatform.db

import org.iq80.leveldb.{DB, ReadOptions}

import scala.collection.mutable

trait KVStore extends AutoCloseable {

  type K = Array[Byte]
  type V = Array[Byte]

  protected val db: DB

  def get(key: K): Option[V] =
    Option(db.get(key))

  def getAll(cond: (K, V) => Boolean): Seq[(K, V)] = {
    val ro = new ReadOptions()
    ro.snapshot(db.getSnapshot)
    val iter = db.iterator(ro)
    try {
      iter.seekToFirst()
      val bf = mutable.ArrayBuffer.empty[(K, V)]
      while (iter.hasNext) {
        val next = iter.next()
        val key = next.getKey
        val value = next.getValue
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
