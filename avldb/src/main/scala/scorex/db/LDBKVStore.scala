package scorex.db

import org.iq80.leveldb.DB
import scorex.util.ScorexLogging


/**
  * A LevelDB wrapper providing a convenient non-versioned database interface.
  *
  * Both keys and values are var-sized byte arrays.
  */
class LDBKVStore(protected val db: DB) extends KVStoreReader with ScorexLogging {

  def update(toInsert: Seq[(K, V)], toRemove: Seq[K]): Unit = {
    val batch = db.createWriteBatch()
    try {
      toInsert.foreach { case (k, v) => batch.put(k, v) }
      toRemove.foreach(batch.delete)
      db.write(batch)
    } catch {
      case t: Throwable => log.error("DB error: ", t)
    } finally {
      batch.close()
    }
  }

  def insert(values: Seq[(K, V)]): Unit = update(values, Seq.empty)

  def remove(keys: Seq[K]): Unit = update(Seq.empty, keys)

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
