package scorex.db

import org.rocksdb.{ReadOptions, WriteBatch, WriteOptions}
import scorex.crypto.hash.Blake2b256
import scorex.db.LDBFactory.RegisteredDB
import scorex.db.LDBVersionedStore.SnapshotReadInterface
import scorex.util.ScorexLogging

import java.io.File
import java.nio.ByteBuffer
import java.util.concurrent.locks.ReentrantReadWriteLock
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}


/**
  * Implementation of versioned storage on top of LevelDB.
  *
  * LevelDB implementation of versioned store is based on maintaining "compensating transaction" list,
  * list of reverse operations needed to undo changes of applied transactions.
  * This list is stored in separate LevelDB database (undo) and size of list is limited by the keepVersions parameter.
  * If keepVersions == 0, then undo list is not maintained and rollback of the committed transactions is not possible.
  *
  * @param dir - folder to store data
  * @param initialKeepVersions - number of versions to keep when the store is created. Can be changed after.
  *
  */
class LDBVersionedStore(protected val dir: File, val initialKeepVersions: Int)
  extends KVStoreReader with ScorexLogging {

  type VersionID = Array[Byte]

  type LSN = Long // logical serial number: type used to provide order of records in undo list

  private val last_version_key = Blake2b256("last_version")

  private var keepVersions: Int = initialKeepVersions

  override val db: RegisteredDB = createDB(dir, "ldb_main") // storage for main data
  override val lock = new ReentrantReadWriteLock()

  private val undo: RegisteredDB = createDB(dir, "ldb_undo") // storage for undo data
  private var lsn: LSN = getLastLSN // last assigned logical serial number
  private var versionLsn = ArrayBuffer.empty[LSN] // LSNs of versions (var because we need to invert this array)

  // mutable array of all the kept versions
  private val versions: ArrayBuffer[VersionID] = getAllVersions
  private var lastVersion: Option[VersionID] = versions.lastOption


  //default write options, no sync!
  private val writeOptions = new WriteOptions()

  private def createDB(dir: File, storeName: String): RegisteredDB =
    LDBFactory.open(new File(dir, storeName))

  /** Set new keep versions threshold, remove not needed versions and return old value of keep versions */
  def setKeepVersions(newKeepVersions: Int): Int = {
    lock.writeLock().lock()
    val oldKeepVersions = keepVersions
    try {
      if (newKeepVersions < oldKeepVersions) {
        cleanStart(newKeepVersions)
      }
      keepVersions = newKeepVersions
    } finally {
      lock.writeLock().unlock()
    }
    oldKeepVersions
  }

  def getKeepVersions: Int = keepVersions

  /** returns value associated with the key or throws `NoSuchElementException` */
  def apply(key: K): V = getOrElse(key, {
    throw new NoSuchElementException()
  })


  /**
    * Batch get with callback for result value.
    *
    * Finds all keys from given iterable.
    * Results are passed to callable consumer.
    *
    * It uses latest (most recent) version available in store
    *
    * @param keys     keys to lookup
    * @param consumer callback method to consume results
    */
  def get(keys: Iterable[K], consumer: (K, Option[V]) => Unit): Unit = {
    for (key <- keys) {
      val value = get(key)
      consumer(key, value)
    }
  }

  def processAll(consumer: (K, V) => Unit): Unit = {
    lock.readLock().lock()
    val iterator = db.iterator
    try {
      iterator.seekToFirst()
      while (iterator.isValid) {
        consumer(iterator.key(), iterator.value())
        iterator.next()
      }
    } finally {
      iterator.close()
      lock.readLock().unlock()
    }
  }

  private def newLSN(): Array[Byte] = {
    lsn += 1
    encodeLSN(lsn)
  }

  /**
    * Invert word to provide descending key order.
    * Java implementation of LevelDB org.iq80.leveldb doesn't support iteration in backward direction.
    */
  private def decodeLSN(lsn: Array[Byte]): LSN = {
    ~ByteBuffer.wrap(lsn).getLong
  }

  private def encodeLSN(lsn: LSN): Array[Byte] = {
    val buf = ByteBuffer.allocate(8)
    buf.putLong(~lsn)
    buf.array()
  }

  private def getLastLSN: LSN = {
    val iterator = undo.iterator
    try {
      iterator.seekToFirst()
      if (iterator.isValid) {
        decodeLSN(iterator.key())
      } else {
        0
      }
    } finally {
      iterator.close()
    }
  }

  def lastVersionID: Option[VersionID] = {
    lastVersion
  }

  def versionIdExists(versionID: VersionID): Boolean = {
    lock.readLock().lock()
    try {
      versions.exists(_.sameElements(versionID))
    } finally {
      lock.readLock().unlock()
    }
  }

  private def getAllVersions: ArrayBuffer[VersionID] = {
    val versions = ArrayBuffer.empty[VersionID]
    var lastVersion: Option[VersionID] = None
    var lastLsn: LSN = 0
    // We iterate in LSN descending order
    val iterator = undo.iterator
    iterator.seekToFirst()
    while (iterator.isValid) {
      val currVersion = deserializeUndo(iterator.value()).versionID
      lastLsn = decodeLSN(iterator.key())
      if (!lastVersion.exists(_.sameElements(currVersion))) {
        versionLsn += lastLsn + 1 // this is first LSN of successor version
        versions += currVersion
        lastVersion = Some(currVersion)
      }
      iterator.next()
    }
    iterator.close()
    // As far as org.iq80.leveldb doesn't support iteration in reverse order, we have to iterate in the order
    // of decreasing LSNs and then revert version list. For each version we store first (smallest) LSN.
    versionLsn += lastLsn // first LSN of oldest version
    versionLsn = versionLsn.reverse // LSNs should be in ascending order
    versionLsn.remove(versionLsn.size - 1) // remove last element which corresponds to next assigned LSN

    if (versions.nonEmpty) {
      versions.reverse
    } else {
      val dbVersion = db.get(last_version_key)
      if (dbVersion != null) {
        versions += dbVersion
        versionLsn += lastLsn
      }
      versions
    }
  }

  /**
    * Undo action. To implement recovery to the specified version, we store in the separate undo database
    * sequence of undo operations corresponding to the updates done by the committed transactions.
    */
  case class Undo(versionID: VersionID, key: Array[Byte], value: Array[Byte])

  private def serializeUndo(versionID: VersionID, key: Array[Byte], value: Array[Byte]): Array[Byte] = {
    val valueSize = if (value != null) value.length else 0
    val versionSize = versionID.length
    val keySize = key.length
    val packed = new Array[Byte](2 + versionSize + keySize + valueSize)
    require(keySize <= 0xFF)
    packed(0) = versionSize.asInstanceOf[Byte]
    packed(1) = keySize.asInstanceOf[Byte]
    Array.copy(versionID, 0, packed, 2, versionSize)
    Array.copy(key, 0, packed, 2 + versionSize, keySize)
    if (value != null) {
      Array.copy(value, 0, packed, 2 + versionSize + keySize, valueSize)
    }
    packed
  }

  private def deserializeUndo(undo: Array[Byte]): Undo = {
    val versionSize = undo(0) & 0xFF
    val keySize = undo(1) & 0xFF
    val valueSize = undo.length - versionSize - keySize - 2
    val versionID = undo.slice(2, 2 + versionSize)
    val key = undo.slice(2 + versionSize, 2 + versionSize + keySize)
    val value = if (valueSize == 0){
      null
    } else{
      undo.slice(2 + versionSize + keySize, undo.length)
    }
    Undo(versionID, key, value)
  }

  /**
    * Write versioned batch update to the database, removing keys from the database and adding new key -> value pairs
    */
  def update(versionID: VersionID,
             toRemove: TraversableOnce[Array[Byte]],
             toUpdate: TraversableOnce[(Array[Byte], Array[Byte])]): Try[Unit] = Try {
    lock.writeLock().lock()
    val lastLsn = lsn // remember current LSN value
    val batch = new WriteBatch()
    val undoBatch = new WriteBatch()
    try {
      toRemove.foreach(key => {
        batch.delete(key)
        if (keepVersions > 0) {
          val value = db.get(key)
          if (value != null) { // attempt to delete not existed key
            undoBatch.put(newLSN(), serializeUndo(versionID, key, value))
          }
        }
      })
      for ((key, v) <- toUpdate) {
        require(key.length != 0) // empty keys are not allowed
        if (keepVersions > 0) {
          val old = db.get(key)
          undoBatch.put(newLSN(), serializeUndo(versionID, key, old))
        }
        batch.put(key, v)
      }

      if (keepVersions > 0) {
        if (lsn == lastLsn) { // no records were written for this version: generate dummy record
          undoBatch.put(newLSN(), serializeUndo(versionID, new Array[Byte](0), null))
        }
        undo.write(writeOptions, undoBatch)
        if (lastVersion.isEmpty || !versionID.sameElements(lastVersion.get)) {
          versions += versionID
          versionLsn += lastLsn + 1 // first LSN for this version
          cleanStart(keepVersions)
        }
      } else {
        //keepVersions = 0
        if (lastVersion.isEmpty || !versionID.sameElements(lastVersion.get)) {
          batch.put(last_version_key, versionID)
          versions.clear()
          versions += versionID
          if (versionLsn.isEmpty) {
            versionLsn += lastLsn
          }
        }
      }

      db.write(writeOptions, batch)
      lastVersion = Some(versionID)
    } finally {
      // Make sure you close the batch to avoid resource leaks.
      batch.close()
      undoBatch.close()
      lock.writeLock().unlock()
    }
  }

  def insert(versionID: VersionID, toInsert: Seq[(K, V)]): Try[Unit] = update(versionID, Seq.empty, toInsert)

  def remove(versionID: VersionID, toRemove: Seq[K]): Try[Unit] = update(versionID, toRemove, Seq.empty)


  // Keep last "count"+1 versions and remove undo information for older versions
  private def cleanStart(count: Int): Unit = {
    val deteriorated = versions.size - count - 1
    if (deteriorated >= 0) {
      val fromLsn = versionLsn(0)
      val tillLsn = if (deteriorated+1 < versions.size) versionLsn(deteriorated+1) else lsn+1
      val batch = new WriteBatch()
      try {
        for (lsn <- fromLsn until tillLsn) {
          batch.delete(encodeLSN(lsn))
        }
        undo.write(writeOptions, batch)
      } finally {
        batch.close()
      }

      versions.remove(0, deteriorated)
      versionLsn.remove(0, deteriorated)
      if (count == 0) {
        db.put(last_version_key, versions(0))
      }
    }
  }

  def clean(count: Int): Unit = {
    lock.writeLock().lock()
    try {
      cleanStart(count)
    } finally {
      lock.writeLock().unlock()
    }
  }

  override def close(): Unit = {
    lock.writeLock().lock()
    try {
      undo.close()
      db.close()
    } finally {
      lock.writeLock().unlock()
    }
  }

  // Rollback to the specified version: undo all changes done after specified version
  def rollbackTo(versionID: VersionID): Try[Unit] = Try {
    lock.writeLock().lock()
    try {
      val versionIndex = versions.indexWhere(_.sameElements(versionID))
      if (versionIndex >= 0) {
        if (versionIndex != versions.size-1) {
          val batch = new WriteBatch()
          val undoBatch = new WriteBatch()
          var nUndoRecords: Long = 0
          val iterator = undo.iterator
          var lastLsn: LSN = 0
          try {
            var undoing = true
            iterator.seekToFirst()
            while (undoing && iterator.isValid) {
              val undo = deserializeUndo(iterator.value())
              if (undo.versionID.sameElements(versionID)) {
                undoing = false
                lastLsn = decodeLSN(iterator.key())
              } else {
                undoBatch.delete(iterator.key())
                nUndoRecords += 1
                if (undo.value == null) {
                  if (undo.key.length != 0) { // dummy record
                    batch.delete(undo.key)
                  }
                } else {
                  batch.put(undo.key, undo.value)
                }
              }
              iterator.next()
            }
            db.write(writeOptions, batch)
            undo.write(writeOptions, undoBatch)
          } finally {
            // Make sure you close the batch to avoid resource leaks.
            iterator.close()
            batch.close()
            undoBatch.close()
          }
          val nVersions = versions.size
          require((versionIndex + 1 == nVersions && nUndoRecords == 0) || (versionIndex + 1 < nVersions && lsn - versionLsn(versionIndex + 1) + 1 == nUndoRecords))
          versions.remove(versionIndex + 1, nVersions - versionIndex - 1)
          versionLsn.remove(versionIndex + 1, nVersions - versionIndex - 1)
          lsn -= nUndoRecords // reuse deleted LSN to avoid holes in LSNs
          require(lastLsn == 0 || lsn == lastLsn)
          require(versions.last.sameElements(versionID))
          lastVersion = Some(versionID)
        } else {
          require(lastVersion.get.sameElements(versionID))
        }
      } else {
        throw new NoSuchElementException("versionID not found, can not rollback")
      }
    } finally {
      lock.writeLock().unlock()
    }
  }

  def rollbackVersions(): Iterable[VersionID] = {
    versions.reverse
  }

  /**
    * Take database snapshot, process it, and then close the snapshot.
    * Could be useful when it is needed to process current state of database without blocking database (and so threads
    * possibly working with it).
    *
    * @param logic - processing logic which is getting access to `get` function to read from database snapshot
    */
  def processSnapshot[T](logic: SnapshotReadInterface => T): Try[T] = {
    val ro = new ReadOptions()
    try {
      lock.writeLock().lock()
      ro.setSnapshot(db.getSnapshot)
      lock.writeLock().unlock()

      object readInterface extends SnapshotReadInterface {
        def get(key: Array[Byte]): Array[Byte] = db.get(ro, key)
      }
      Success(logic(readInterface))
    } catch {
      case t: Throwable =>
        log.info("Error during snapshot processing: ", t)
        Failure(t)
    } finally {
      // Close the snapshot to avoid resource leaks
      db.releaseSnapshot(ro.snapshot())
      ro.close()
    }
  }

}

object LDBVersionedStore {

  /**
    * Interface to read from versioned database snapshot which can be provided to clients in order to serve them with
    * snapshot. Contains only reader function.
    */
  trait SnapshotReadInterface {
    /**
      * Read value by key. Client should care about key existence on its side. If key does not exist in database,
      * an exception will be thrown.
      */
    def get(key: Array[Byte]): Array[Byte]
  }

}
