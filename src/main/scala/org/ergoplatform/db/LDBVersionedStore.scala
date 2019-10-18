package org.ergoplatform.db

import org.ergoplatform.db.LDBFactory.factory
import io.iohk.iodb.Store.{K, V, VersionID}
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.iq80.leveldb._
import java.nio.ByteBuffer
import java.io._
import scala.collection.mutable.ArrayBuffer

class LDBVersionedStore(val dir: File, val keepVersions: Int = 0) extends Store {
  private val db: DB = createDB(dir, "ldb_main")
  private val undo: DB = createDB(dir, "ldb_undo")
  private var lsn : Long = getLastLSN
  private val versions : ArrayBuffer[VersionID] = getAllVersions

  private def createDB(dir: File, storeName: String): DB = {
    val op = new Options()
    op.createIfMissing(true)
    factory.open(new File(dir, storeName), op)
  }

  def get(key: K): Option[V] = {
    val b = db.get(key.data)
    if (b == null) None else Some(ByteArrayWrapper(b))
  }

  def getAll(consumer: (K, V) => Unit): Unit = {
    val iterator = db.iterator()
    iterator.seekToFirst()
    try {
      while (iterator.hasNext) {
        val n = iterator.next()
        consumer(ByteArrayWrapper(n.getKey), ByteArrayWrapper(n.getValue))
      }
    } finally {
      iterator.close()
    }
  }

  private def newLSN() : Array[Byte] = {
    val buf = ByteBuffer.allocate(8)
    lsn += 1
    buf.putLong(lsn)
    buf.array()
  }

  def getLastLSN: Long = {
    val iterator = undo.iterator
    try {
      iterator.seekToLast()
      if (iterator.hasPrev) {
        ByteBuffer.wrap(iterator.peekPrev().getKey).getLong
      } else {
        0
      }
    } finally {
      iterator.close()
    }
  }

  def lastVersionID: Option[VersionID] = {
    versions.lastOption
  }
  
  def versionIDExists(versionID: VersionID): Boolean = {
    versions.contains(versionID)
  }
  
  private def getAllVersions: ArrayBuffer[VersionID] = {
    val versions = ArrayBuffer.empty[VersionID]
    var lastVersion : VersionID = null
    undo.forEach(entry => {
      val currVersion = deserializeUndo(entry.getValue).versionID
      if (!currVersion.equals(lastVersion)) {
        versions += currVersion
        lastVersion = currVersion
      }
    })
    versions
  }

  case class Undo(versionID: VersionID, key: Array[Byte], value : Array[Byte])

  private def serializeUndo(versionID: VersionID, key: Array[Byte], value : Array[Byte]): Array[Byte] = {
    val valueSize = if (value != null) value.length else 0
    val versionSize = versionID.size
    val keySize = key.length
    val packed = new Array[Byte](2 + versionSize + keySize + valueSize)
    assert(keySize <= 0xFF)
    packed(0) = versionSize.asInstanceOf[Byte]
    packed(1) = keySize.asInstanceOf[Byte]
    Array.copy(versionID.data, 0, packed, 2, versionSize)
    Array.copy(key, 0, packed, 2 + versionSize, keySize)
    if (value != null) {
      Array.copy(value, 0, packed, 2 + versionSize + keySize, valueSize)
    }
    packed
  }

  private def deserializeUndo(undo : Array[Byte]): Undo = {
    val versionSize = undo(0) & 0xFF
    val keySize = undo(1) & 0xFF
    val valueSize = undo.length - versionSize - keySize - 2
    val versionID = ByteArrayWrapper(undo.slice(2, 2 + versionSize))
    val key = undo.slice(2 + versionSize, 2 + versionSize + keySize)
    val value = if (valueSize == 0) null else undo.slice(2 + versionSize + keySize, undo.length)
    Undo(versionID, key, value)
  }

  def update(versionID: VersionID, toRemove: Iterable[K], toUpdate: Iterable[(K, V)]): Unit = {
    val batch = db.createWriteBatch()
    val undoBatch = undo.createWriteBatch()
    try {
      toRemove.foreach(b => {
        val key = b.data
        batch.delete(key)
        if (keepVersions > 0) {
          val value = db.get(key)
          if (value != null) {
            undoBatch.put(newLSN(), serializeUndo(versionID, key, value))
          }
        }
      })
      for ((k, v) <- toUpdate) {
        val key = k.data
        if (keepVersions > 0) {
          val old = db.get(key)
          undoBatch.put(newLSN(), serializeUndo(versionID, key, old))
        }
        batch.put(key, v.data)
      }
      db.write(batch)
      if (keepVersions > 0) {
        undo.write(undoBatch)
        if (lastVersionID.isEmpty || !versionID.equals(lastVersionID.get)) {
          versions += versionID
          cleanStart(keepVersions)
        }
      }
    } finally {
      // Make sure you close the batch to avoid resource leaks.
      batch.close()
      undoBatch.close()
    }

  }

  def cleanStart(count: Int): Unit = {
    if (versions.size > count) {
      rollback(versions(count))
    }
  }

  def clean(count: Int): Unit = {
    cleanStart(count)
    undo.resumeCompactions()
    db.resumeCompactions()
  }

  def cleanStop(): Unit = {
    undo.suspendCompactions()
    db.suspendCompactions()
  }

  def close(): Unit = {
    undo.close()
    db.close()
  }

  def rollback(versionID: VersionID): Unit = {
    val versionIndex = versions.indexOf(versionID)
    if (versionIndex >= 0) {
      var found = false
      var undoing = true
      val batch = db.createWriteBatch()
      val undoBatch = undo.createWriteBatch()
      val iterator = undo.iterator()
      try {
        iterator.seekToLast()
        while (undoing && iterator.hasPrev) {
          val entry = iterator.prev()
          val undo = deserializeUndo(entry.getValue)
          if (undo.versionID.equals(versionID)) {
            found = true
          } else {
            undoing = !found
          }
          if (undoing) {
            undoBatch.delete(entry.getKey)
            if (undo.value == null) {
              batch.delete(undo.key)
            } else {
              batch.put(undo.key, undo.value)
            }
          }
        }
        db.write(batch)
        undo.write(undoBatch)
      } finally {
        // Make sure you close the batch to avoid resource leaks.
        iterator.close()
        batch.close()
        undoBatch.close()
      }
      versions.remove(versionIndex, versions.size - versionIndex)
    }
  }

  def rollbackVersions(): Iterable[VersionID] = {
    versions
  }
}
