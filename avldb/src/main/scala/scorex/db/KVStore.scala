package scorex.db

import java.util.concurrent.locks.ReentrantReadWriteLock

import org.iq80.leveldb.{DB, ReadOptions}

import scala.collection.mutable

trait KVStore extends AutoCloseable {

  type K = Array[Byte]
  type V = Array[Byte]

  protected val db: DB

  protected val lock = new ReentrantReadWriteLock()

  def get(key: K): Option[V] =
    Option(db.get(key))

  def getWithFilter(cond: (K, V) => Boolean): Iterator[(K, V)] = {
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
      bf.toIterator
    } finally {
      iter.close()
      ro.snapshot().close()
    }
  }

  def getAll: Iterator[(K, V)] = getWithFilter((_, _) => true)

  /** Returns value associated with the key, or default value from user
    */
  def getOrElse(key: K, default: => V): V =
    get(key).getOrElse(default)

  /**
    * Batch get.
    *
    * Finds all keys from given iterable.
    * Result is returned in an iterable of key-value pairs.
    * If key is not found, null value is included in result pair.
    *
    *
    * @param keys keys to lookup
    * @return iterable over key-value pairs found in store
    */
  def get(keys: Iterable[K]): Iterable[(K, Option[V])] = {
    val ret = scala.collection.mutable.ArrayBuffer.empty[(K, Option[V])]
    keys.foreach { key =>
      ret += key -> get(key)
    }
    ret
  }


  def close(): Unit = {
    lock.writeLock().lock()
    try {
      db.close()
    } finally {
      lock.writeLock().unlock()
    }
  }

}
