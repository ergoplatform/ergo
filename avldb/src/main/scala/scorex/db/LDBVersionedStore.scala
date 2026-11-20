package scorex.db

import java.io.File
import scorex.db.LDBFactory.factory
import org.iq80.leveldb._

import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.locks.ReentrantReadWriteLock
import scorex.crypto.hash.Blake2b256
import scorex.db.LDBVersionedStore.SnapshotReadInterface
import scorex.util.ScorexLogging
import scorex.util.encode.Base16

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
  private val committed_state_key = Blake2b256("committed_state")

  private def isInternalKey(key: Array[Byte]): Boolean =
    java.util.Arrays.equals(key, last_version_key) ||
      java.util.Arrays.equals(key, committed_state_key)

  private var keepVersions: Int = initialKeepVersions

  override val db: DB = createDB(dir, "ldb_main") // storage for main data
  override val lock = new ReentrantReadWriteLock()

  private val undo: DB = createDB(dir, "ldb_undo") // storage for undo data

  //default write options, no sync!
  private val writeOptions = new WriteOptions()

  recoverInterruptedWrite()

  private var lsn: LSN = getLastLSN // last assigned logical serial number
  private var versionLsn = ArrayBuffer.empty[LSN] // LSNs of versions (var because we need to invert this array)

  // mutable array of all the kept versions
  private val versions: ArrayBuffer[VersionID] = getAllVersions
  private var lastVersion: Option[VersionID] = versions.lastOption

  // With keepVersions = 0 no undo records are kept, so a store opened with 0 drops the ones an earlier run with
  // keepVersions > 0 left, as setKeepVersions(0) does. Otherwise they would still name the last version on the
  // next open, while updates with keepVersions = 0 move only last_version_key (#1631). Runs only while undo
  // records exist, so once. Keep it after writeOptions: cleanStart writes with it. The log keeps the
  // last_version_key it replaces: in a store an earlier release updated this way, that value is the right one.
  if (keepVersions == 0 && lsn > 0) {
    val lastVersionKey = Option(db.get(last_version_key)).map(Base16.encode).getOrElse("none")
    log.info(s"$dir opened with keepVersions = 0 over undo records of ${versions.size} version(s): removing them, " +
      s"last_version_key $lastVersionKey set to ${Base16.encode(versions.last)}")
    cleanStart(0)
  }

  ensureCommittedStateInitialized()

  private def createDB(dir: File, storeName: String): DB = {
    val op = new Options()
    op.createIfMissing(true)
    op.paranoidChecks(true)
    factory.open(new File(dir, storeName), op)
  }

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

  override def getWithFilter(cond: (K, V) => Boolean): Iterator[(K, V)] =
    super.getWithFilter((k, v) => !isInternalKey(k) && cond(k, v))

  override def getRange(start: K, end: K, limit: Int = Int.MaxValue): Array[(K, V)] = {
    val ro = new ReadOptions()
    ro.snapshot(db.getSnapshot)
    val iter = db.iterator(ro)
    try {
      iter.seek(start)
      val bf = ArrayBuffer.empty[(K, V)]
      var elemCounter = 0
      while (iter.hasNext && elemCounter < limit) {
        val next = iter.next()
        if (ByteArrayUtils.compare(next.getKey, end) <= 0) {
          if (!isInternalKey(next.getKey)) {
            elemCounter += 1
            bf += (next.getKey -> next.getValue)
          }
        } else {
          elemCounter = limit
        }
      }
      bf.toArray[(K, V)]
    } finally {
      iter.close()
      ro.snapshot().close()
    }
  }

  def processAll(consumer: (K, V) => Unit): Unit = {
    lock.readLock().lock()
    val iterator = db.iterator()
    try {
      iterator.seekToFirst()
      while (iterator.hasNext) {
        val n = iterator.next()
        if (!isInternalKey(n.getKey)) {
          consumer(n.getKey, n.getValue)
        }
      }
    } finally {
      iterator.close()
      lock.readLock().unlock()
    }
  }

  private def encodeCommittedState(committedLsn: LSN, versionOpt: Option[VersionID]): Array[Byte] = {
    val versionBytes = versionOpt.getOrElse(Array.emptyByteArray)
    val buf = ByteBuffer.allocate(8 + versionBytes.length)
    buf.putLong(committedLsn)
    buf.put(versionBytes)
    buf.array()
  }

  private def decodeCommittedState(bytes: Array[Byte]): (LSN, Option[VersionID]) = {
    val committedLsn = ByteBuffer.wrap(bytes, 0, 8).getLong
    val versionOpt = if (bytes.length > 8) Some(bytes.slice(8, bytes.length)) else None
    (committedLsn, versionOpt)
  }

  private def isMainDbEmpty: Boolean = {
    val iterator = db.iterator()
    try {
      iterator.seekToFirst()
      !iterator.hasNext
    } finally {
      iterator.close()
    }
  }

  /**
    * Reconciles `ldb_undo` and `ldb_main` when an earlier process stopped between their two non-atomic writes
    * (either inside `update` after writing `ldb_undo` and before writing `ldb_main`, or inside `rollbackTo`
    * after writing `ldb_main` and before pruning `ldb_undo`).
    */
  private def recoverInterruptedWrite(): Unit = {
    val rawMeta = db.get(committed_state_key)
    val undoMaxLsn = getLastLSN
    if (rawMeta == null) {
      if (undoMaxLsn == 0 && db.get(last_version_key) == null && isMainDbEmpty) {
        db.put(committed_state_key, encodeCommittedState(0L, None), writeOptions)
      }
    } else {
      val (committedLsn, committedVersionOpt) = decodeCommittedState(rawMeta)
      if (undoMaxLsn > committedLsn) {
        log.warn(
          s"$dir has uncommitted undo records up to LSN $undoMaxLsn above committed LSN $committedLsn " +
            s"(version ${committedVersionOpt.map(Base16.encode).getOrElse("none")}): rolling back uncommitted tail"
        )
        val mainBatch = db.createWriteBatch()
        val undoBatch = undo.createWriteBatch()
        val iterator = undo.iterator()
        try {
          iterator.seekToFirst()
          var undoing = true
          while (undoing && iterator.hasNext) {
            val entry = iterator.next()
            val entryLsn = decodeLSN(entry.getKey)
            if (entryLsn > committedLsn) {
              undoBatch.delete(entry.getKey)
              val u = deserializeUndo(entry.getValue)
              if (u.value == null) {
                if (u.key.length != 0) {
                  mainBatch.delete(u.key)
                }
              } else {
                mainBatch.put(u.key, u.value)
              }
            } else {
              undoing = false
            }
          }
          if (committedLsn == 0L) {
            committedVersionOpt match {
              case Some(v) => mainBatch.put(last_version_key, v)
              case None    => mainBatch.delete(last_version_key)
            }
          }
          db.write(mainBatch, writeOptions)
          undo.write(undoBatch, writeOptions)
        } finally {
          iterator.close()
          mainBatch.close()
          undoBatch.close()
        }
      }
    }
  }

  private def ensureCommittedStateInitialized(): Unit = {
    val rawMeta = db.get(committed_state_key)
    val currentUndoLsn = getLastLSN
    if (rawMeta == null) {
      db.put(committed_state_key, encodeCommittedState(currentUndoLsn, lastVersion), writeOptions)
    } else if (currentUndoLsn == 0L && decodeCommittedState(rawMeta)._1 > 0L) {
      db.put(committed_state_key, encodeCommittedState(0L, lastVersion), writeOptions)
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
      if (iterator.hasNext) {
        decodeLSN(iterator.peekNext().getKey)
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
    val iterator = undo.iterator()
    iterator.seekToFirst()
    while (iterator.hasNext) {
      val entry = iterator.next
      val currVersion = deserializeUndo(entry.getValue).versionID
      lastLsn = decodeLSN(entry.getKey)
      if (!lastVersion.exists(_.sameElements(currVersion))) {
        versionLsn += lastLsn + 1 // this is first LSN of successor version
        versions += currVersion
        lastVersion = Some(currVersion)
      }
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
    var undoWritten = false
    val batch = db.createWriteBatch()
    val undoBatch = undo.createWriteBatch()
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
        batch.put(committed_state_key, encodeCommittedState(lsn, Some(versionID)))
        undo.write(undoBatch, writeOptions)
        undoWritten = true
        db.write(batch, writeOptions)
        if (lastVersion.isEmpty || !versionID.sameElements(lastVersion.get)) {
          versions += versionID
          versionLsn += lastLsn + 1 // first LSN for this version
          cleanStart(keepVersions)
        }
      } else {
        //keepVersions = 0
        batch.put(committed_state_key, encodeCommittedState(0L, Some(versionID)))
        if (lastVersion.isEmpty || !versionID.sameElements(lastVersion.get)) {
          batch.put(last_version_key, versionID)
        }
        db.write(batch, writeOptions)
        if (lastVersion.isEmpty || !versionID.sameElements(lastVersion.get)) {
          versions.clear()
          versions += versionID
          if (versionLsn.isEmpty) {
            versionLsn += lastLsn
          }
        }
      }

      lastVersion = Some(versionID)
    } catch {
      case t: Throwable =>
        if (!undoWritten) {
          lsn = lastLsn
        }
        throw t
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
      if (count == 0) {
        db.put(last_version_key, versions.last, writeOptions)
      }
      val batch = undo.createWriteBatch()
      try {
        for (lsn <- fromLsn until tillLsn) {
          batch.delete(encodeLSN(lsn))
        }
        undo.write(batch, writeOptions)
      } finally {
        batch.close()
      }

      if (count == 0) {
        db.put(committed_state_key, encodeCommittedState(0L, Some(versions.last)), writeOptions)
      }
      versions.remove(0, deteriorated)
      versionLsn.remove(0, deteriorated)
    }
  }

  def clean(count: Int): Unit = {
    lock.writeLock().lock()
    try {
      cleanStart(count)
    } finally {
      lock.writeLock().unlock()
    }
    undo.resumeCompactions()
    db.resumeCompactions()
  }

  def cleanStop(): Unit = {
    undo.suspendCompactions()
    db.suspendCompactions()
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
          val batch = db.createWriteBatch()
          val undoBatch = undo.createWriteBatch()
          var nUndoRecords: Long = 0
          val iterator = undo.iterator()
          var lastLsn: LSN = 0
          try {
            var undoing = true
            iterator.seekToFirst()
            while (undoing && iterator.hasNext) {
              val entry = iterator.next()
              val undo = deserializeUndo(entry.getValue)
              if (undo.versionID.sameElements(versionID)) {
                undoing = false
                lastLsn = decodeLSN(entry.getKey)
              } else {
                undoBatch.delete(entry.getKey)
                nUndoRecords += 1
                if (undo.value == null) {
                  if (undo.key.length != 0) { // dummy record
                    batch.delete(undo.key)
                  }
                } else {
                  batch.put(undo.key, undo.value)
                }
              }
            }
            batch.put(committed_state_key, encodeCommittedState(lastLsn, Some(versionID)))
            if (lastLsn == 0L) {
              batch.put(last_version_key, versionID)
            }
            db.write(batch, writeOptions)
            undo.write(undoBatch, writeOptions)
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
      ro.snapshot(db.getSnapshot)
      lock.writeLock().unlock()

      object readInterface extends SnapshotReadInterface {
        def get(key: Array[Byte]): Array[Byte] = db.get(key, ro)
      }
      Success(logic(readInterface))
    } catch {
      case t: Throwable =>
        log.info("Error during snapshot processing: ", t)
        Failure(t)
    } finally {
      // Close the snapshot to avoid resource leaks
      ro.snapshot().close()
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
