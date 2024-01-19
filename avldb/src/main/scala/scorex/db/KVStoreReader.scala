package scorex.db

import org.rocksdb.ReadOptions
import scorex.db.LDBFactory.RegisteredDB

import java.util.concurrent.locks.ReentrantReadWriteLock
import scala.collection.mutable

/**
  * Basic interface for reading from LevelDB key-value storage.
  * Both keys and values are var-sized byte arrays.
  */
trait KVStoreReader extends AutoCloseable {

  type K = Array[Byte]
  type V = Array[Byte]

  protected val db: RegisteredDB

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
   * Query if database contains key
   * @param key - key
   * @return true if key exists, false otherwise
   */
  def contains(key: K): Boolean = {
    lock.readLock().lock()
    try {
      db.contains(key)
    } finally {
      lock.readLock().unlock()
    }
  }

  /**
    * Read all the database elements.
    * @return iterator over database contents
    */
  def getAll: Iterator[(K, V)] = {
    val ro = new ReadOptions()
    ro.setSnapshot(db.getSnapshot)
    val iter = db.iterator(ro)
    try {
      iter.seekToFirst()
      val bf = mutable.ArrayBuffer.empty[(K, V)]
      while (iter.isValid) {
        bf += (iter.key() -> iter.value())
        iter.next()
      }
      bf.toIterator
    } finally {
      iter.close()
      db.releaseSnapshot(ro.snapshot())
      ro.close()
    }
  }

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
    val iter = db.iterator(ro)
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
      db.releaseSnapshot(ro.snapshot())
      ro.close()
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
