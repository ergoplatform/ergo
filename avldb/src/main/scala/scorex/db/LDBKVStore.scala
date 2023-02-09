package scorex.db

import org.iq80.leveldb.DB
import scorex.util.ScorexLogging

import scala.util.{Failure, Success, Try}
import spire.syntax.all.cfor


/**
  * A LevelDB wrapper providing a convenient non-versioned database interface.
  *
  * Both keys and values are var-sized byte arrays.
  */
class LDBKVStore(protected val db: DB) extends KVStoreReader with ScorexLogging {

  def update(toInsert: Array[(K, V)], toRemove: Array[K]): Try[Unit] = {
    val batch = db.createWriteBatch()
    val insertLen = toInsert.length
    val removeLen = toRemove.length
    try {
      cfor(0)(_ < insertLen, _ + 1) { i => batch.put(toInsert(i)._1, toInsert(i)._2)}
      cfor(0)(_ < removeLen, _ + 1) { i => batch.delete(toRemove(i))}
      db.write(batch)
      Success(())
    } catch {
      case t: Throwable => Failure(t)
    } finally {
      batch.close()
    }
  }

  /**
    * Insert single key-value into database
    * @param id - key to insert
    * @param value - value to insert
    * @return - Success(()) in case of successful insertion, Failure otherwise
    */
  def insert(id: K,  value: V): Try[Unit] = {
    try {
      db.put(id, value)
      Success(())
    } catch {
      case t: Throwable => Failure(t)
    }
  }

  def insert(values: Array[(K, V)]): Try[Unit] = update(values, Array.empty)

  def remove(keys: Array[K]): Try[Unit] = update(Array.empty, keys)

  /**
    * Get last key within some range (inclusive) by used comparator.
    * Could be useful for applications with sequential ids.
    * The method iterates over all the keys so could be slow if there are many keys in the range.
    */
  def lastKeyInRange(first: Array[Byte], last: Array[Byte]): Option[K] = {
    import util.control.Breaks._

    val i = db.iterator()
    var res: Option[K] = None
    i.seek(first)
    breakable {
      while (i.hasNext) {
        val key = i.next().getKey
        if (ByteArrayUtils.compare(key, last) <= 0) res = Some(key) else break
      }
    }
    res
  }

}
