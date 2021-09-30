package scorex.db

import swaydb._
import swaydb.serializers.Default._
import java.io.File
import java.util.concurrent.locks.ReentrantReadWriteLock

import swaydb.IO.ApiIO

import scala.collection.mutable.ArrayBuffer
import scala.util.Try
import scala.util.control.Breaks._

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
class SWDBVersionedStore(protected val dir: File, val keepVersions: Int) extends SWDBStoreReader {
  type VersionID = Array[Byte]

  type LSN = Long // logical serial number: type used to provide order of records in undo list

  override val db: Map[K, V, Nothing, ApiIO] = persistent.Map[K, V, Nothing, IO.ApiIO](dir = new File(dir, "ldb_main").toPath).get
  private val lock = new ReentrantReadWriteLock()

  private val undo = {
    persistent.Map[LSN, V, Nothing, IO.ApiIO](dir = new File(dir, "ldb_undo").toPath).get
  }
  private var lsn: LSN = getLastLSN // last assigned logical serial number
  private val versionLsn = ArrayBuffer.empty[LSN] // LSNs of versions

  // mutable array of all the kept versions
  private val versions: ArrayBuffer[VersionID] = getAllVersions
  private var lastVersion: Option[VersionID] = versions.lastOption

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
    try {
      for (n <- db) {
        consumer(n._1, n._2)
      }
    } finally {
      lock.readLock().unlock()
    }
  }

  private def newLSN(): LSN = {
    lsn += 1
    lsn
  }


  private def getLastLSN: LSN = {
    undo.last.get.map(_._1).getOrElse(0)
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
    for (entry <- undo) {
      val currVersion = deserializeUndo(entry._2).versionID
      lastLsn = entry._1
      if (!lastVersion.exists(_.sameElements(currVersion))) {
        versionLsn += lastLsn
        versions += currVersion
        lastVersion = Some(currVersion)
      }
    }
    versions
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

  def update(versionID: VersionID, toRemove: Iterable[Array[Byte]], toUpdate: Iterable[(Array[Byte], Array[Byte])]): Try[Unit] = Try{
    lock.writeLock().lock()
    val lastLsn = lsn // remember current LSN value
    try {
      toRemove.foreach(key => {
         if (keepVersions > 0) {
          db.get(key).get.foreach { value =>
            undo.put(newLSN(), serializeUndo(versionID, key, value))
          }
        }
      })
      for ((key, v) <- toUpdate) {
        assert(key.length != 0) // empty keys are not allowed
        if (keepVersions > 0) {
          val old = db.get(key).get.getOrElse(null)
          undo.put(newLSN(), serializeUndo(versionID, key, old))
        }
      }
      db.remove(toRemove)
      db.put(toUpdate)
      if (keepVersions > 0) {
        if (lsn == lastLsn) { // no records were written for this version: generate dummy record
          undo.put(newLSN(), serializeUndo(versionID, new Array[Byte](0), null))
        }
        if (lastVersion.isEmpty || !versionID.sameElements(lastVersion.get)) {
          versions += versionID
          versionLsn += lastLsn + 1 // first LSN for this version
          cleanStart(keepVersions)
        }
      }
      lastVersion = Some(versionID)
    } finally {
      lock.writeLock().unlock()
    }
  }

  def insert(versionID: VersionID, toInsert: Seq[(K, V)]): Try[Unit] = {
    update(versionID, Seq.empty, toInsert)
  }

  def remove(versionID: VersionID, toRemove: Seq[K]): Try[Unit] = {
    update(versionID, toRemove, Seq.empty)
  }

  // Keep last "count" versions and remove undo information for older versions
  private def cleanStart(count: Int): Unit = {
    val deteriorated = versions.size - count
    if (deteriorated > 0) {
      val fromLsn = versionLsn(0)
      val tillLsn = versionLsn(deteriorated)
      undo.remove(fromLsn, tillLsn-1)
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
  }

  def cleanStop(): Unit = {
  }

  override def close(): Unit = {
    lock.writeLock().lock()
    try {
      undo.close()
      super.close()
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
        var nUndoRecords: Long = 0
        var lastLsn: LSN = 0
        breakable {
          for (entry <- undo.reverse) {
            val undo = deserializeUndo(entry._2)
            if (undo.versionID.sameElements(versionID)) {
              lastLsn = entry._1
              break
            } else {
              nUndoRecords += 1
              if (undo.value == null) {
                if (undo.key.length != 0) { // dummy record
                  db.remove(undo.key)
                }
              } else {
                db.put(undo.key, undo.value)
              }
            }
          }
        }
        undo.remove(lastLsn + 1, lsn)
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
