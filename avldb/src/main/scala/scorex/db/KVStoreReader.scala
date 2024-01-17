package scorex.db

import java.util.concurrent.locks.ReentrantReadWriteLock
import org.rocksdb.{ReadOptions, RocksDB}

import scala.collection.mutable

/**
  * Basic interface for reading from LevelDB key-value storage.
  * Both keys and values are var-sized byte arrays.
  */
trait KVStoreReader extends AutoCloseable {

  type K = Array[Byte]
  type V = Array[Byte]

  protected val db: RocksDB

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
    ro.setSnapshot(db.getSnapshot)
    val iter = db.newIterator(ro)
    try {
      iter.seekToFirst()
      val bf = mutable.ArrayBuffer.empty[(K, V)]
      while (iter.isValid) {
        val key = iter.key()
        val value = iter.value()
        if (cond(key, value)) bf += (key -> value)
        iter.next()
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
  def getRange(start: K, end: K, limit: Int = Int.MaxValue): Array[(K, V)] = {
    val ro = new ReadOptions()
    ro.setSnapshot(db.getSnapshot)
    val iter = db.newIterator(ro)
    try {
      iter.seek(start)
      val bf = mutable.ArrayBuffer.empty[(K, V)]
      var elemCounter = 0
      while (iter.isValid && elemCounter < limit) {
        if(ByteArrayUtils.compare(iter.key(), end) <= 0) {
          elemCounter += 1
          bf += (iter.key() -> iter.value())
        } else elemCounter = limit // break
        iter.next()
      }
      bf.toArray[(K,V)]
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
