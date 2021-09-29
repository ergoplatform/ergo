package scorex.db

import swaydb._
import scorex.util.ScorexLogging

import scala.util.Try

/**
  * A LevelDB wrapper providing a convenient non-versioned database interface.
  *
  * Both keys and values are var-sized byte arrays.
  */
class SWDBStore(protected val db: Map[Array[Byte], Array[Byte], Nothing, IO.ApiIO]) extends SWDBStoreReader with ScorexLogging {

  def update(toInsert: Seq[(K, V)], toRemove: Seq[K]): Try[Unit] = Try {
    db.put(toInsert).toTry.flatMap(_ => db.remove(toRemove).toTry).map(_ => ())
  }

  def insert(values: Seq[(K, V)]): Try[Unit] = db.put(values).toTry.map(_ => ())

  def remove(keys: Seq[K]): Try[Unit] = db.remove(keys).toTry.map(_ => ())

}
