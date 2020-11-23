package scorex.db

import java.io.File

import scorex.db.LDBFactory.factory
import org.iq80.leveldb._
import java.nio.ByteBuffer

import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.locks.ReentrantReadWriteLock

import scala.util.Try


/**
  * Implementation of versioned storage on top of LevelDB.
  *
  * LevelDB implementation of versioned store is based on maintaining "compensating transaction" list,
  * list of reverse operations needed to undo changes of applied transactions.
  * This list is stored in separate LevelDB database (undo) and size of list is limited by the keepVersions parameter.
  * If keepVersions == 0, then undo list is not maintained and rollback of the committed transactions is not possible.
  *
  * @param dir - folder to store data
  * @param keepVersions - number of versions to keep
  *
  */
class LDBVersionedStore(protected val dir: File, val keepVersions: Int) extends KVStoreReader {
  type VersionID = Array[Byte]

  type LSN = Long // logical serial number: type used to provide order of records in undo list

  override val db: DB = createDB(dir, "ldb_main") // storage for main data
  override val lock = new ReentrantReadWriteLock()

  private val undo: DB = createDB(dir, "ldb_undo") // storage for undo data
  private var lsn: LSN = getLastLSN // last assigned logical serial number
  private var versionLsn = ArrayBuffer.empty[LSN] // LSNs of versions (var because we need to invert this array)

  // mutable array of all the kept versions
  private val versions: ArrayBuffer[VersionID] = getAllVersions
  private var lastVersion: Option[VersionID] = versions.lastOption

  //default write options, no sync!
  private val writeOptions = new WriteOptions()

  private def createDB(dir: File, storeName: String): DB = {
    val op = new Options()
    op.createIfMissing(true)
    op.paranoidChecks(true)
    factory.open(new File(dir, storeName), op)
  }

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
    val iterator = db.iterator()
    try {
      iterator.seekToFirst()
      while (iterator.hasNext) {
        val n = iterator.next()
        consumer(n.getKey, n.getValue)
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
    versions.reverse
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
    assert(keySize <= 0xFF)
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
    val value = if (valueSize == 0) null else undo.slice(2 + versionSize + keySize, undo.length)
    Undo(versionID, key, value)
  }

  def update(versionID: VersionID, toRemove: Iterable[Array[Byte]], toUpdate: Iterable[(Array[Byte], Array[Byte])]): Unit = {
    lock.writeLock().lock()
    val lastLsn = lsn // remember current LSN value
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
        assert(key.length != 0) // empty keys are not allowed
        if (keepVersions > 0) {
          val old = db.get(key)
          undoBatch.put(newLSN(), serializeUndo(versionID, key, old))
        }
        batch.put(key, v)
      }
      db.write(batch, writeOptions)
      if (keepVersions > 0) {
        if (lsn == lastLsn) { // no records were written for this version: generate dummy record
          undoBatch.put(newLSN(), serializeUndo(versionID, new Array[Byte](0), null))
        }
        undo.write(undoBatch, writeOptions)
        if (lastVersion.isEmpty || !versionID.sameElements(lastVersion.get)) {
          versions += versionID
          versionLsn += lastLsn + 1 // first LSN for this version
          cleanStart(keepVersions)
        }
      }
      lastVersion = Some(versionID)
    } finally {
      // Make sure you close the batch to avoid resource leaks.
      batch.close()
      undoBatch.close()
      lock.writeLock().unlock()
    }
  }

  def insert(versionID: VersionID, toInsert: Seq[(K, V)]): Unit = update(versionID, Seq.empty, toInsert)

  def remove(versionID: VersionID, toRemove: Seq[K]): Unit = update(versionID, toRemove, Seq.empty)


  // Keep last "count" versions and remove undo information for older versions
  private def cleanStart(count: Int): Unit = {
    val deteriorated = versions.size - count
    if (deteriorated > 0) {
      val fromLsn = versionLsn(0)
      val tillLsn = versionLsn(deteriorated)
      val batch = undo.createWriteBatch()
      try {
        for (lsn <- fromLsn until tillLsn) {
          batch.delete(encodeLSN(lsn))
        }
        undo.write(batch, writeOptions)
      } finally {
        batch.close()
      }
      versions.remove(0, deteriorated)
      versionLsn.remove(0, deteriorated)
      lastVersion = versions.lastOption
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
        val batch = db.createWriteBatch()
        val undoBatch = undo.createWriteBatch()
        var nUndoRecords: Long = 0
        val iterator = undo.iterator()
        var lastLsn: LSN = 0
        try {
          var undoing = true
          iterator.seekToFirst()
          while (undoing) {
            assert(iterator.hasNext)
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
          db.write(batch, writeOptions)
          undo.write(undoBatch, writeOptions)
        } finally {
          // Make sure you close the batch to avoid resource leaks.
          iterator.close()
          batch.close()
          undoBatch.close()
        }
        val nVersions = versions.size
        assert((versionIndex + 1 == nVersions && nUndoRecords == 0) || (versionIndex + 1 < nVersions && lsn - versionLsn(versionIndex + 1) + 1 == nUndoRecords))
        versions.remove(versionIndex + 1, nVersions - versionIndex - 1)
        versionLsn.remove(versionIndex + 1, nVersions - versionIndex - 1)
        lsn -= nUndoRecords // reuse deleted LSN to avoid holes in LSNs
        assert(lsn == lastLsn)
        assert(versions.last.sameElements(versionID))
        lastVersion = Some(versionID)
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
}
