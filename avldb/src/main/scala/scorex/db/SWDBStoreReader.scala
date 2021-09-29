package scorex.db


import swaydb._

/**
  * Basic interface for reading from LevelDB key-value storage.
  * Both keys and values are var-sized byte arrays.
  */
trait SWDBStoreReader extends AutoCloseable {

  type K = Array[Byte]
  type V = Array[Byte]

  protected val db:Map[Array[Byte], Array[Byte], Nothing, IO.ApiIO]


  /**
    * Read database element by its key
    * @param key - key
    * @return element if exists, None otherwise
    */
  def get(key: K): Option[V] = {
    db.get(key).get
  }

  /**
    * Iterate through the database to read elements according to a filter function.
    * @param cond - the filter function
    * @return iterator over elements satisfying the filter function
    */
  def getWithFilter(cond: ((K, V)) => Boolean): Iterator[(K, V)] = {
    db.filter(cond).iterator(Bag.glass)
  }

  /**
    * Read all the database elements.
    * @return iterator over database contents
    */
  def getAll: Iterator[(K, V)] = {
    db.iterator(Bag.glass)
  }

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

  /** Returns value associated with the key, or default value from user
    */
  def getOrElse(key: K, default: => V): V =
    get(key).getOrElse(default)

  /**
    * Get keys in range
    * @param start - beginning of the range (inclusive)
    * @param end - end of the range (inclusive)
    * @return
    */
  def getRange(start: K, end: K): Seq[(K, V)] = {
    db.from(start).takeWhile(kv => ByteArrayUtils.compare(kv._1, end) <= 0).materialize.get
  }

  /**
    * Get last key within some range (inclusive) by used comparator.
    * Could be useful for applications with sequential ids.
    * The method iterates over all the keys so could be slow if there are many keys in the range.
    */
  def lastKeyInRange(first: Array[Byte], last: Array[Byte]): Option[K] = {
    db.fromOrBefore(last).takeWhile(kv => ByteArrayUtils.compare(kv._1, first) >= 0).map(_._1).head.get
  }

  /**
    * Close the database
    */
  def close(): Unit = {
    db.close()
  }
}
