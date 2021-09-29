package scorex.db

import swaydb._
import scorex.util.ScorexLogging
import swaydb.serializers.Default._

/**
  * A LevelDB wrapper providing a convenient non-versioned database interface.
  *
  * Both keys and values are var-sized byte arrays.
  */
class SWDBStore(protected val db: Map[Array[Byte], Array[Byte], Nothing, IO.ApiIO]) extends SWDBStoreReader with ScorexLogging {
  def update(toInsert: Seq[(K, V)], toRemove: Seq[K]): Unit = {
    db.put(toInsert)
    db.remove(toRemove)
  }

  def insert(values: Seq[(K, V)]): Unit = db.put(values)

  def remove(keys: Seq[K]): Unit = db.remove(keys)
}
