package scorex.db

import org.rocksdb.{WriteBatch, WriteOptions}
import scorex.db.LDBFactory.RegisteredDB
import scorex.util.ScorexLogging
import spire.syntax.all.cfor

import scala.util.{Failure, Success, Try}


/**
  * A LevelDB wrapper providing a convenient non-versioned database interface.
  *
  * Both keys and values are var-sized byte arrays.
  */
class LDBKVStore(protected val db: RegisteredDB) extends KVStoreReader with ScorexLogging {
  /** Immutable empty array can be shared to avoid allocations. */
  private val emptyArrayOfByteArray = Array.empty[Array[Byte]]

  /** default write options, snyc enabled */
  private val wo: WriteOptions = new WriteOptions().setSync(true)

  /**
    * Update this database atomically with a batch of insertion and removal operations
    *
    * @param toInsertKeys - keys of key-value pairs to insert into database
    * @param toInsertValues - values of key-value pairs to insert into database
    * @param toRemove - keys of key-value pairs to remove from the database
    * @return - error if it happens, or success status
    */
  def update(toInsertKeys: Array[K], toInsertValues: Array[V], toRemove: Array[K]): Try[Unit] = {
    val batch = new WriteBatch()
    try {
      require(toInsertKeys.length == toInsertValues.length)
      cfor(0)(_ < toInsertKeys.length, _ + 1) { i => batch.put(toInsertKeys(i), toInsertValues(i))}
      cfor(0)(_ < toRemove.length, _ + 1) { i => batch.delete(toRemove(i))}
      db.write(wo, batch)
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

  /**
    * `update` variant where we only insert values into this database
    */
  def insert(keys: Array[K], values: Array[V]): Try[Unit] = {
    update(keys, values, emptyArrayOfByteArray)
  }

  /**
    * `update` variant where we only remove values from this database
    */
  def remove(keys: Array[K]): Try[Unit] = {
    update(emptyArrayOfByteArray, emptyArrayOfByteArray, keys)
  }

}
