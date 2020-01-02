package scorex.db

import java.util.concurrent.locks.ReentrantReadWriteLock

import org.iq80.leveldb.{DB, ReadOptions}

import scala.collection.mutable

/**
  * Basic interface for key-value storage. Both keys and values are var-sized byte arrays.
  */
trait KVStore extends AutoCloseable {

  type K = Array[Byte]
  type V = Array[Byte]

  protected val db: DB

  protected val lock = new ReentrantReadWriteLock()

  /**
    * Read database element by its key
    * @param key - key
    * @return element if exists, None otherwise
    */
  def get(key: K): Option[V] = {
    lock.readLock().lock()
    try {
      Option(db.get(key))
    } finally {
      lock.readLock().unlock()
    }
  }


  /**
    * Iterate through the database to read elements according to a filter function.
    * @param cond - the filter function
    * @return iterator over elements satisfying the filter function
    */
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

  /**
    * Read all the database elements.
    * @return iterator over database contents
    */
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
    * If key is not found, None value is included in a resulting pair.
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

  /**
    * Get keys in range
    * @param start - beginning of the range (inclusive)
    * @param end - end of the range (inclusive)
    * @return
    */
  def getRange(start: K, end: K): Seq[(K, V)] = {
    val ro = new ReadOptions()
    ro.snapshot(db.getSnapshot)
    val iter = db.iterator(ro)
    try {
      def check(key:Array[Byte]) = {
        if (ByteArrayUtils.compare(key, end) <= 0) {
          true
        } else {
          false
        }
      }
      iter.seek(start)
      val bf = mutable.ArrayBuffer.empty[(K, V)]
      while (iter.hasNext && check(iter.peekNext.getKey)) {
        val next = iter.next()
        val key = next.getKey
        val value = next.getValue
        bf += (key -> value)
      }
      bf.toList
    } finally {
      iter.close()
      ro.snapshot().close()
    }
  }

  /**
    * Close the database
    */
  def close(): Unit = {
    lock.writeLock().lock()
    try {
      db.close()
    } finally {
      lock.writeLock().unlock()
    }
  }

}
