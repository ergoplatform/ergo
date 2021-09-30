package scorex.db

import swaydb._
import scorex.util.ScorexLogging

import scala.util.{Success, Try}

/**
  * A LevelDB wrapper providing a convenient non-versioned database interface.
  *
  * Both keys and values are var-sized byte arrays.
  */
class SWDBStore(val db: Map[Array[Byte], Array[Byte], Nothing, IO.ApiIO]) extends SWDBStoreReader with ScorexLogging {

  def update(toInsert: Seq[(K, V)], toRemove: Seq[K]): Try[Unit] = Try {
    insert(toInsert).flatMap(_ => remove(toRemove))
  }

  def insert(values: Seq[(K, V)]): Try[Unit] = {
    if (!values.isEmpty) {
      db.put(values).toTry.map(_ => ())
    } else {
      Success(())
    }
  }

  def remove(keys: Seq[K]): Try[Unit] = {
    if (!keys.isEmpty) {
      db.remove(keys).toTry.map(_ => ())
    } else {
      Success(())
    }
  }
}
