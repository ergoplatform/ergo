package org.ergoplatform.nodeView.history.storage

import com.github.benmanes.caffeine.cache.Caffeine
import org.ergoplatform.modifiers.{BlockSection, NetworkObjectTypeId}
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, ExtraIndexSerializer, IndexedErgoBox, Segment, StorageRentBox}
import org.ergoplatform.settings.{Algos, CacheSettings, ErgoSettings}
import org.ergoplatform.utils.ScorexEncoding
import scorex.db.{ByteArrayWrapper, LDBFactory, LDBKVStore}
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}
import spire.syntax.all.cfor

import java.io.{File, IOException}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.concurrent.locks.ReentrantReadWriteLock
import java.util.zip.CRC32
import scala.jdk.CollectionConverters.asScalaIteratorConverter

/**
  * Storage for Ergo history
  *
  * @param indexStore     - Additional key-value storage for indexes, required by History for efficient work.
  *                       contains links to bestHeader, bestFullBlock, heights and scores for different blocks, etc.
  * @param objectsStore   - key-value store, where key is id of ErgoPersistentModifier and value is it's bytes
  * @param extraStore     - key-value store, where key is id of Index and value is it's bytes
  * @param config         - cache configs
  * @param journalPathOpt - optional undo-journal path used to ensure crash-safe cross-store atomicity
  *                       between objectsStore and indexStore
  */
class HistoryStorage(indexStore: LDBKVStore,
                     objectsStore: LDBKVStore,
                     extraStore: LDBKVStore,
                     config: CacheSettings,
                     journalPathOpt: Option[Path])
  extends ScorexLogging
    with AutoCloseable
    with ScorexEncoding {

  def this(indexStore: LDBKVStore,
           objectsStore: LDBKVStore,
           extraStore: LDBKVStore,
           config: CacheSettings) =
    this(indexStore, objectsStore, extraStore, config, None)

  private lazy val headersCache =
    Caffeine.newBuilder()
      .maximumSize(config.history.headersCacheSize)
      .build[String, BlockSection]()

  private lazy val blockSectionsCache =
    Caffeine.newBuilder()
      .maximumSize(config.history.blockSectionsCacheSize)
      .build[String, BlockSection]()

  private lazy val extraCache =
    Caffeine.newBuilder()
      .maximumSize(config.history.extraCacheSize)
      .build[String, ExtraIndex]()

  private lazy val indexCache =
    Caffeine.newBuilder()
      .maximumSize(config.history.indexesCacheSize)
      .build[ByteArrayWrapper, Array[Byte]]

  private val historyLock = new ReentrantReadWriteLock()
  private val extraCacheLock = new ReentrantReadWriteLock()
  private val pendingJournalOpt: Option[HistoryStorage.PendingBatchJournal] =
    journalPathOpt.map(new HistoryStorage.PendingBatchJournal(_))

  private def withHistoryReadLock[A](body: => A): A = {
    historyLock.readLock().lock()
    try body
    finally historyLock.readLock().unlock()
  }

  private def withHistoryWriteLock[A](body: => A): A = {
    historyLock.writeLock().lock()
    try body
    finally historyLock.writeLock().unlock()
  }

  private def withExtraCacheReadLock[A](body: => A): A = {
    extraCacheLock.readLock().lock()
    try body
    finally extraCacheLock.readLock().unlock()
  }

  private def withExtraCacheWriteLock[A](body: => A): A = {
    extraCacheLock.writeLock().lock()
    try body
    finally extraCacheLock.writeLock().unlock()
  }

  private def cacheModifier(mod: BlockSection): Unit = mod.modifierTypeId match {
    case Header.modifierTypeId => headersCache.put(mod.id, mod)
    case _ => blockSectionsCache.put(mod.id, mod)
  }

  private def lookupModifier(id: ModifierId): Option[BlockSection] =
    Option(headersCache.getIfPresent(id)) orElse Option(blockSectionsCache.getIfPresent(id))

  private def removeModifier(id: ModifierId): Unit = {
    headersCache.invalidate(id)
    blockSectionsCache.invalidate(id)
    extraCache.invalidate(id)
  }

  private def invalidateHistoryModifierCaches(id: ModifierId): Unit = {
    headersCache.invalidate(id)
    blockSectionsCache.invalidate(id)
  }

  private def snapshotCachedObjectEntries(objectsToInsert: Array[BlockSection],
                                          objectKeys: Array[Array[Byte]]): Array[(Array[Byte], Option[Array[Byte]])] = {
    val seen = mutable.HashSet.empty[ByteArrayWrapper]
    val entries = mutable.ArrayBuffer.empty[(Array[Byte], Option[Array[Byte]])]
    cfor(0)(_ < objectsToInsert.length, _ + 1) { i =>
      val key = objectKeys(i)
      if (seen.add(ByteArrayWrapper(key))) {
        val priorBytesOpt = lookupModifier(objectsToInsert(i).id).flatMap(m => Try(HistoryModifierSerializer.toBytes(m)).toOption)
        entries += ((key, priorBytesOpt))
      }
    }
    entries.toArray
  }

  def modifierBytesById(id: ModifierId): Option[Array[Byte]] = withHistoryReadLock {
    objectsStore.get(idToBytes(id)).map(_.tail).orElse(extraStore.get(idToBytes(id))) // removing modifier type byte with .tail (only in objectsStore)
  }

  /**
    * @return bytes and type of a network object stored in the database with identifier `id`
    */
  def modifierTypeAndBytesById(id: ModifierId): Option[(NetworkObjectTypeId.Value, Array[Byte])] = withHistoryReadLock {
    objectsStore.get(idToBytes(id)).map(bs => (NetworkObjectTypeId.fromByte(bs.head), bs.tail)) // first byte is type id, tail is modifier bytes
  }

  def modifierById(id: ModifierId): Option[BlockSection] = withHistoryReadLock {
    lookupModifier(id) orElse objectsStore.get(idToBytes(id)).flatMap { bytes =>
      HistoryModifierSerializer.parseBytesTry(bytes) match {
        case Success(pm) =>
          log.trace(s"Cache miss for existing modifier $id")
          cacheModifier(pm)
          Some(pm)
        case Failure(e) =>
          log.warn(s"Failed to parse modifier ${encoder.encode(id)} from db (bytes are: ${Algos.encode(bytes)})", e)
          None
      }
    }
  }

  def getExtraIndex(id: ModifierId): Option[ExtraIndex] = withExtraCacheReadLock {
    Option(extraCache.getIfPresent(id)) orElse extraStore.get(idToBytes(id)).flatMap { bytes =>
      ExtraIndexSerializer.parseBytesTry(bytes) match {
        case Success(pm) =>
          log.trace(s"Cache miss for existing index $id")
          if(!pm.isInstanceOf[Segment[_]]){
            extraCache.put(pm.id, pm) // cache non-segment objects
          }
          Some(pm)
        case Failure(_) =>
          log.warn(s"Failed to parse index ${encoder.encode(id)} from db (bytes are: ${Algos.encode(bytes)})")
          None
      }
    }
  }

  def getIndex(id: ByteArrayWrapper): Option[Array[Byte]] = withHistoryReadLock {
    Option(indexCache.getIfPresent(id)).orElse {
      indexStore.get(id.data).map { value =>
        indexCache.put(id, value)
        value
      }
    }
  }

  /**
    * Remove storage-rent eligibility entries of the given boxes, e.g. when a miner self-claim
    * transaction spending them failed validation during block assembly, so retrying the claim
    * is futile. An entry whose [[IndexedErgoBox]] is not in the extra index can not be
    * located and is left in place.
    */
  def removeStorageRentBoxes(boxIds: Seq[ModifierId]): Unit = {
    val keys = boxIds.flatMap { boxId =>
      getExtraIndex(boxId) match {
        case Some(iEb: IndexedErgoBox) => Some(StorageRentBox(iEb).id)
        case _ =>
          log.warn(s"Can not remove storage-rent eligibility entry of box $boxId, box not indexed")
          None
      }
    }
    if (keys.nonEmpty) removeExtra(keys.toArray)
  }

  /**
    * Read up to `limit` storage-rent eligibility entries for currently-unspent boxes created
    * at or before `creationHeight`, in ascending (creationHeight, globalIndex) order.
    * Returns an empty array when the extra index is disabled or does not cover the height.
    */
  def storageRentBoxesAtOrBefore(creationHeight: Int, limit: Int): Array[StorageRentBox] = {
    val start = StorageRentBox.key(0, 0L)
    extraStore.scanFrom(
      start,
      limit,
      keyFilter = key =>
        key.length == StorageRentBox.KeyLength &&
          key(0) == StorageRentBox.KeyMarker &&
          java.nio.ByteBuffer.wrap(key, 1, 4).getInt <= creationHeight,
      // stop when the marker byte changes (the iterator left the rent namespace) or at
      // the first in-namespace key whose height bytes pass the cutoff. Any key with height
      // bytes > cutoff sorts after every key with height bytes <= cutoff, so no eligible
      // rent row can come after it; foreign 32-byte ids sharing the marker byte are
      // skipped over (excluded by keyFilter) while their height bytes are within the
      // cutoff, and end the scan when beyond it.
      continueScan = key =>
        key.nonEmpty && key(0) == StorageRentBox.KeyMarker &&
          (key.length < 5 || java.nio.ByteBuffer.wrap(key, 1, 4).getInt <= creationHeight)
    ).map { case (key, _) => StorageRentBox.fromKey(key) }
  }

  /**
    * @return object with `id` if it is in the objects database
    */
  def get(id: ModifierId): Option[Array[Byte]] = withHistoryReadLock {
    val idBytes = idToBytes(id)
    objectsStore.get(idBytes).orElse(extraStore.get(idBytes))
  }
  def get(id: Array[Byte]): Option[Array[Byte]] = withHistoryReadLock {
    objectsStore.get(id).orElse(extraStore.get(id))
  }

  /**
    * @return if object with `id` is in the objects database
    */
  def contains(id: Array[Byte]): Boolean = get(id).isDefined
  def contains(id: ModifierId): Boolean = get(id).isDefined

  def insert(indexesToInsert: Array[(ByteArrayWrapper, Array[Byte])],
             objectsToInsert: Array[BlockSection],
             indexesToRemove: Array[ByteArrayWrapper] = Array.empty): Try[Unit] = {
    Try {
      val objectKeys = new Array[Array[Byte]](objectsToInsert.length)
      val objectValues = new Array[Array[Byte]](objectsToInsert.length)
      cfor(0)(_ < objectsToInsert.length, _ + 1) { i =>
        val mod = objectsToInsert(i)
        objectKeys(i) = mod.serializedId
        objectValues(i) = HistoryModifierSerializer.toBytes(mod)
      }
      (objectKeys, objectValues)
    }.flatMap { case (objectKeys, objectValues) =>
      withHistoryWriteLock {
        val hasObjects = objectsToInsert.nonEmpty
        val hasIndexes = indexesToInsert.nonEmpty || indexesToRemove.nonEmpty
        val isPairedWrite = hasObjects && hasIndexes

        val snapshotAndStage: Try[Array[(Array[Byte], Option[Array[Byte]])]] =
          if (!isPairedWrite) {
            Success(Array.empty)
          } else {
            pendingJournalOpt match {
              case Some(journal) =>
                Try {
                  val objUndo = HistoryStorage.snapshotObjectStoreEntries(objectsStore, objectKeys)
                  val idxUndo = HistoryStorage.snapshotIndexStoreEntries(indexStore, indexesToInsert, indexesToRemove)
                  (objUndo, idxUndo)
                }.flatMap { case (objUndo, idxUndo) =>
                  journal.stage(HistoryStorage.UndoRecord(objUndo, idxUndo)).map(_ => objUndo)
                }
              case None =>
                Success(snapshotCachedObjectEntries(objectsToInsert, objectKeys))
            }
          }

        snapshotAndStage.flatMap { objectUndoEntries =>
          val objectsWrite: Try[Unit] =
            if (hasObjects) Try(objectsStore.insert(objectKeys, objectValues)).flatten
            else Success(())

          objectsWrite match {
            case Failure(objErr) =>
              pendingJournalOpt.foreach(_.clearCommitted())
              Failure(objErr)

            case Success(_) =>
              if (!hasIndexes) {
                cfor(0)(_ < objectsToInsert.length, _ + 1) { i => cacheModifier(objectsToInsert(i)) }
                Success(())
              } else {
                val insertedKeys = indexesToInsert.map(_._1.data)
                val insertedValues = indexesToInsert.map(_._2)
                val indexesWrite = Try {
                  if (indexesToRemove.nonEmpty) {
                    indexStore.update(insertedKeys, insertedValues, indexesToRemove.map(_.data))
                  } else {
                    indexStore.insert(insertedKeys, insertedValues)
                  }
                }.flatten

                indexesWrite match {
                  case Success(_) =>
                    pendingJournalOpt.foreach(_.clearCommitted())
                    cfor(0)(_ < objectsToInsert.length, _ + 1) { i => cacheModifier(objectsToInsert(i)) }
                    cfor(0)(_ < indexesToInsert.length, _ + 1) { i =>
                      indexCache.put(indexesToInsert(i)._1, indexesToInsert(i)._2)
                    }
                    cfor(0)(_ < indexesToRemove.length, _ + 1) { i =>
                      indexCache.invalidate(indexesToRemove(i))
                    }
                    Success(())

                  case Failure(idxErr) =>
                    val rollbackRes =
                      if (hasObjects) Try(HistoryStorage.applyUndoEntries(objectsStore, objectUndoEntries)).flatten
                      else Success(())
                    cfor(0)(_ < objectsToInsert.length, _ + 1) { i =>
                      invalidateHistoryModifierCaches(objectsToInsert(i).id)
                    }
                    cfor(0)(_ < indexesToInsert.length, _ + 1) { i =>
                      indexCache.invalidate(indexesToInsert(i)._1)
                    }
                    cfor(0)(_ < indexesToRemove.length, _ + 1) { i =>
                      indexCache.invalidate(indexesToRemove(i))
                    }
                    rollbackRes match {
                      case Success(_) =>
                        pendingJournalOpt.foreach(_.clearCommitted())
                      case Failure(rbErr) =>
                        log.error(
                          "Failed to roll back objectsStore after indexStore write failure; " +
                            "retaining pending batch journal for startup recovery",
                          rbErr
                        )
                    }
                    Failure(idxErr)
                }
              }
          }
        }
      }
    }
  }

  def insertExtra(indexesToInsert: Array[(Array[Byte], Array[Byte])],
                  objectsToInsert: Array[ExtraIndex]): Unit = {
    insertExtraTry(indexesToInsert, objectsToInsert).failed.foreach { error =>
      log.error("Failed to insert extra indexes", error)
    }
  }

  private[history] def invalidateExtraCache(ids: Iterable[ModifierId]): Unit = withExtraCacheWriteLock {
    ids.foreach(extraCache.invalidate)
  }

  def insertExtraTry(indexesToInsert: Array[(Array[Byte], Array[Byte])],
                     objectsToInsert: Array[ExtraIndex]): Try[Unit] =
    insertExtraTry(indexesToInsert, objectsToInsert, Array.empty)

  /**
    * Insert indexes and objects and remove `indexesToRemove` in a single store batch, so a
    * crash can not leave inserts (e.g. the indexer progress marker) committed without the
    * removals (e.g. spent storage-rent rows) or vice versa.
    */
  def insertExtraTry(indexesToInsert: Array[(Array[Byte], Array[Byte])],
                     objectsToInsert: Array[ExtraIndex],
                     indexesToRemove: Array[ModifierId]): Try[Unit] = {
    val objectIds = objectsToInsert.iterator.flatMap(obj => Try(obj.id).toOption).toArray
    Try {
      val keys = objectsToInsert.map(_.serializedId) ++ indexesToInsert.map(_._1)
      val values = objectsToInsert.map(ExtraIndexSerializer.toBytes) ++ indexesToInsert.map(_._2)
      keys -> values
    }.flatMap { case (keys, values) =>
      withExtraCacheWriteLock {
        extraStore.update(keys, values, indexesToRemove.map(idToBytes)).map { _ =>
          objectIds.foreach(extraCache.invalidate)
          cfor(0)(_ < indexesToRemove.length, _ + 1) { i => removeModifier(indexesToRemove(i)) }
        }
      }
    }.recoverWith { case error =>
      invalidateExtraCache(objectIds)
      Failure(error)
    }
  }

  def removeExtra(indexesToRemove: Array[ModifierId]) : Unit = {
    removeExtraTry(indexesToRemove).failed.foreach { error =>
      log.error("Failed to remove extra indexes", error)
    }
  }

  def removeExtraTry(indexesToRemove: Array[ModifierId]): Try[Unit] = {
    withExtraCacheWriteLock {
      extraStore.remove(indexesToRemove.map(idToBytes)).map { _ =>
        cfor(0)(_ < indexesToRemove.length, _ + 1) { i => removeModifier(indexesToRemove(i)) }
      }
    }
  }

  /**
    * Insert single object to database. This version allows for efficient insert
    * when identifier and bytes of object (i.e. modifier, a block section) are known.
    *
    * @param objectIdToInsert - object id to insert
    * @param objectToInsert - object bytes to insert
    * @return - Success if insertion was successful, Failure otherwise
    */
  def insert(objectIdToInsert: Array[Byte],
             objectToInsert: Array[Byte]): Try[Unit] = withHistoryWriteLock {
    objectsStore.insert(objectIdToInsert, objectToInsert).map { _ =>
      if (objectIdToInsert.length == 32) {
        invalidateHistoryModifierCaches(bytesToId(objectIdToInsert))
      }
    }
  }

  /**
    * Remove elements from stored indices and modifiers
    *
    * @param indicesToRemove - indices keys to remove
    * @param idsToRemove - identifiers of modifiers to remove
    * @return
    */
  def remove(indicesToRemove: Array[ByteArrayWrapper],
             idsToRemove: Array[ModifierId]): Try[Unit] = withHistoryWriteLock {
    val hasObjects = idsToRemove.nonEmpty
    val hasIndexes = indicesToRemove.nonEmpty
    pendingJournalOpt match {
      case Some(journal) if hasObjects && hasIndexes =>
        val objectKeys = idsToRemove.map(idToBytes)
        val stageAndSnapshot = Try {
          val objectUndo = HistoryStorage.snapshotObjectStoreEntries(objectsStore, objectKeys)
          val indexUndo = HistoryStorage.snapshotIndexStoreEntries(indexStore, Array.empty, indicesToRemove)
          (objectUndo, indexUndo)
        }.flatMap { case (objectUndo, indexUndo) =>
          journal.stage(HistoryStorage.UndoRecord(objectUndo, indexUndo)).map(_ => objectUndo)
        }

        stageAndSnapshot.flatMap { objectUndo =>
          Try(objectsStore.remove(objectKeys)).flatten match {
            case Failure(objErr) =>
              journal.clearCommitted()
              Failure(objErr)
            case Success(_) =>
              Try(indexStore.remove(indicesToRemove.map(_.data))).flatten match {
                case Success(_) =>
                  journal.clearCommitted()
                  cfor(0)(_ < idsToRemove.length, _ + 1) { i => removeModifier(idsToRemove(i)) }
                  cfor(0)(_ < indicesToRemove.length, _ + 1) { i => indexCache.invalidate(indicesToRemove(i)) }
                  Success(())
                case Failure(idxErr) =>
                  val rollbackRes = Try(HistoryStorage.applyUndoEntries(objectsStore, objectUndo)).flatten
                  cfor(0)(_ < idsToRemove.length, _ + 1) { i => removeModifier(idsToRemove(i)) }
                  cfor(0)(_ < indicesToRemove.length, _ + 1) { i => indexCache.invalidate(indicesToRemove(i)) }
                  rollbackRes match {
                    case Success(_) =>
                      journal.clearCommitted()
                    case Failure(rbErr) =>
                      log.error(
                        "Failed to roll back objectsStore after indexStore remove failure; " +
                          "retaining pending batch journal for startup recovery",
                        rbErr
                      )
                  }
                  Failure(idxErr)
              }
          }
        }

      case _ =>
        objectsStore.remove(idsToRemove.map(idToBytes)).flatMap { _ =>
          cfor(0)(_ < idsToRemove.length, _ + 1) { i => removeModifier(idsToRemove(i)) }
          indexStore.remove(indicesToRemove.map(_.data)).map { _ =>
            cfor(0)(_ < indicesToRemove.length, _ + 1) { i => indexCache.invalidate(indicesToRemove(i)) }
            ()
          }
        }
    }
  }

  override def close(): Unit = withHistoryWriteLock {
    log.warn("Closing history storage...")
    pendingJournalOpt.foreach(_.close())
    extraStore.close()
    indexStore.close()
    objectsStore.close()
  }

  /**
    * Delete the extra index database and reopen it.
    *
    * @param ergoSettings - settings to use
    * @return new HistoryStorage instance with an empty extra database
    */
  def deleteExtraDB(ergoSettings: ErgoSettings): HistoryStorage =
    deleteExtraDBTry(ergoSettings).get

  /**
    * Delete the extra index database and reopen it, preserving deletion failures.
    */
  def deleteExtraDBTry(ergoSettings: ErgoSettings): Try[HistoryStorage] = {
    log.warn("Removing extra index database after an invalid checkpoint.")
    val root = new File(s"${ergoSettings.directory}/history/extra")
    Try(close()).flatMap { _ =>
      HistoryStorage.deleteRecursively(root.toPath, Files.delete)
    }.map { _ =>
      log.info(s"Deleted ${root.toString}")
      HistoryStorage.apply(ergoSettings)
    }
  }

}

object HistoryStorage extends ScorexLogging {
  private[storage] val PendingJournalFileName: String = "pending-batch.journal"
  private val JournalMagic: Int = 0x4548534A // "EHSJ"
  private val JournalVersion: Byte = 1.toByte
  private val JournalHeaderSize: Int = 14 // magic(4) + version(1) + active(1) + payloadLen(4) + crc32(4)

  private[storage] final case class UndoRecord(
    objectEntries: Array[(Array[Byte], Option[Array[Byte]])],
    indexEntries: Array[(Array[Byte], Option[Array[Byte]])]
  )

  private[storage] def snapshotObjectStoreEntries(objectsStore: LDBKVStore,
                                                  objectKeys: Array[Array[Byte]]): Array[(Array[Byte], Option[Array[Byte]])] = {
    val seen = mutable.HashSet.empty[ByteArrayWrapper]
    val entries = mutable.ArrayBuffer.empty[(Array[Byte], Option[Array[Byte]])]
    cfor(0)(_ < objectKeys.length, _ + 1) { i =>
      val key = objectKeys(i)
      if (seen.add(ByteArrayWrapper(key))) {
        entries += ((key.clone(), objectsStore.get(key)))
      }
    }
    entries.toArray
  }

  private[storage] def snapshotIndexStoreEntries(indexStore: LDBKVStore,
                                                 indexesToInsert: Array[(ByteArrayWrapper, Array[Byte])],
                                                 indexesToRemove: Array[ByteArrayWrapper]): Array[(Array[Byte], Option[Array[Byte]])] = {
    val seen = mutable.HashSet.empty[ByteArrayWrapper]
    val entries = mutable.ArrayBuffer.empty[(Array[Byte], Option[Array[Byte]])]
    cfor(0)(_ < indexesToInsert.length, _ + 1) { i =>
      val wrapper = indexesToInsert(i)._1
      if (seen.add(wrapper)) {
        entries += ((wrapper.data.clone(), indexStore.get(wrapper.data)))
      }
    }
    cfor(0)(_ < indexesToRemove.length, _ + 1) { i =>
      val wrapper = indexesToRemove(i)
      if (seen.add(wrapper)) {
        entries += ((wrapper.data.clone(), indexStore.get(wrapper.data)))
      }
    }
    entries.toArray
  }

  private[storage] def applyUndoEntries(store: LDBKVStore,
                                        entries: Array[(Array[Byte], Option[Array[Byte]])]): Try[Unit] = {
    val toRemove = mutable.ArrayBuffer.empty[Array[Byte]]
    val toInsertKeys = mutable.ArrayBuffer.empty[Array[Byte]]
    val toInsertValues = mutable.ArrayBuffer.empty[Array[Byte]]
    cfor(0)(_ < entries.length, _ + 1) { i =>
      entries(i)._2 match {
        case Some(prevValue) =>
          toInsertKeys += entries(i)._1
          toInsertValues += prevValue
        case None =>
          toRemove += entries(i)._1
      }
    }
    val removeRes = if (toRemove.nonEmpty) store.remove(toRemove.toArray) else Success(())
    removeRes.flatMap { _ =>
      if (toInsertKeys.nonEmpty) store.insert(toInsertKeys.toArray, toInsertValues.toArray) else Success(())
    }
  }

  private def entriesByteSize(entries: Array[(Array[Byte], Option[Array[Byte]])]): Int = {
    var size = 4
    cfor(0)(_ < entries.length, _ + 1) { i =>
      val (k, vOpt) = entries(i)
      size += 4 + k.length + 1
      vOpt match {
        case Some(v) => size += 4 + v.length
        case None => ()
      }
    }
    size
  }

  private def writeEntries(buf: ByteBuffer, entries: Array[(Array[Byte], Option[Array[Byte]])]): Unit = {
    buf.putInt(entries.length)
    cfor(0)(_ < entries.length, _ + 1) { i =>
      val (k, vOpt) = entries(i)
      buf.putInt(k.length)
      buf.put(k)
      vOpt match {
        case Some(v) =>
          buf.put(1.toByte)
          buf.putInt(v.length)
          buf.put(v)
        case None =>
          buf.put(0.toByte)
      }
    }
  }

  private def encodePayload(record: UndoRecord): Array[Byte] = {
    val totalLen = entriesByteSize(record.objectEntries) + entriesByteSize(record.indexEntries)
    val payload = new Array[Byte](totalLen)
    val buf = ByteBuffer.wrap(payload)
    writeEntries(buf, record.objectEntries)
    writeEntries(buf, record.indexEntries)
    payload
  }

  private def readEntries(buf: ByteBuffer): Array[(Array[Byte], Option[Array[Byte]])] = {
    if (buf.remaining() < 4) throw new IOException("Truncated undo journal entry count")
    val count = buf.getInt()
    if (count < 0 || count > buf.remaining()) throw new IOException(s"Invalid undo journal entry count: $count")
    val entries = new Array[(Array[Byte], Option[Array[Byte]])](count)
    cfor(0)(_ < count, _ + 1) { i =>
      if (buf.remaining() < 4) throw new IOException("Truncated undo journal key length")
      val keyLen = buf.getInt()
      if (keyLen < 0 || keyLen > buf.remaining()) throw new IOException(s"Invalid undo journal key length: $keyLen")
      val key = new Array[Byte](keyLen)
      buf.get(key)
      if (buf.remaining() < 1) throw new IOException("Truncated undo journal presence flag")
      val valOpt = buf.get() match {
        case 0 => None
        case 1 =>
          if (buf.remaining() < 4) throw new IOException("Truncated undo journal value length")
          val valLen = buf.getInt()
          if (valLen < 0 || valLen > buf.remaining()) throw new IOException(s"Invalid undo journal value length: $valLen")
          val value = new Array[Byte](valLen)
          buf.get(value)
          Some(value)
        case other =>
          throw new IOException(s"Invalid undo journal presence flag: $other")
      }
      entries(i) = (key, valOpt)
    }
    entries
  }

  private def decodePayload(payload: Array[Byte]): Try[UndoRecord] = Try {
    val buf = ByteBuffer.wrap(payload)
    val objectEntries = readEntries(buf)
    val indexEntries = readEntries(buf)
    if (buf.hasRemaining) throw new IOException("Trailing bytes in undo journal payload")
    UndoRecord(objectEntries, indexEntries)
  }

  private def computeCrc32(version: Byte, payloadLen: Int, payload: Array[Byte]): Int = {
    val crc = new CRC32()
    crc.update(version.toInt & 0xff)
    crc.update((payloadLen >>> 24) & 0xff)
    crc.update((payloadLen >>> 16) & 0xff)
    crc.update((payloadLen >>> 8) & 0xff)
    crc.update(payloadLen & 0xff)
    crc.update(payload, 0, payloadLen)
    crc.getValue.toInt
  }

  private[storage] final class PendingBatchJournal(val path: Path) extends AutoCloseable {
    private var channelOpt: Option[FileChannel] = None
    private var hasActiveRecord: Boolean = false

    private def ensureOpen(): FileChannel = channelOpt match {
      case Some(ch) if ch.isOpen => ch
      case _ =>
        Option(path.getParent).foreach(parent => Files.createDirectories(parent))
        val ch = FileChannel.open(
          path,
          StandardOpenOption.CREATE,
          StandardOpenOption.READ,
          StandardOpenOption.WRITE
        )
        channelOpt = Some(ch)
        ch
    }

    private def writeAll(ch: FileChannel, buf: ByteBuffer, startPos: Long): Unit = {
      var pos = startPos
      while (buf.hasRemaining) {
        val written = ch.write(buf, pos)
        if (written < 0) throw new IOException("Unexpected negative byte count writing history journal")
        pos += written
      }
    }

    def stage(record: UndoRecord): Try[Unit] = Try {
      val ch = ensureOpen()
      val payload = encodePayload(record)
      val payloadLen = payload.length
      val crc = computeCrc32(JournalVersion, payloadLen, payload)

      if (hasActiveRecord) {
        writeAll(ch, ByteBuffer.wrap(Array[Byte](0.toByte)), 5L)
        ch.force(false)
        hasActiveRecord = false
      }

      val bodyBytes = new Array[Byte](8 + payloadLen)
      val bodyBuf = ByteBuffer.wrap(bodyBytes)
      bodyBuf.putInt(payloadLen)
      bodyBuf.putInt(crc)
      bodyBuf.put(payload)
      writeAll(ch, ByteBuffer.wrap(bodyBytes), 6L)
      ch.force(false)

      val headerBytes = new Array[Byte](6)
      val headerBuf = ByteBuffer.wrap(headerBytes)
      headerBuf.putInt(JournalMagic)
      headerBuf.put(JournalVersion)
      headerBuf.put(1.toByte)
      writeAll(ch, ByteBuffer.wrap(headerBytes), 0L)
      ch.force(false)
      hasActiveRecord = true
    }

    def clearCommitted(): Unit = {
      if (hasActiveRecord) {
        Try {
          channelOpt.foreach { ch =>
            if (ch.isOpen) {
              writeAll(ch, ByteBuffer.wrap(Array[Byte](0.toByte)), 5L)
              ch.force(false)
            }
          }
          hasActiveRecord = false
        }.failed.foreach { err =>
          log.warn(s"Failed to mark pending history batch journal committed at $path", err)
        }
      }
    }

    override def close(): Unit = {
      channelOpt.foreach { ch =>
        Try(ch.close())
      }
      channelOpt = None
      if (!hasActiveRecord) {
        Try(Files.deleteIfExists(path))
      }
    }
  }

  private[storage] def recoverPendingJournal(journalPath: Path,
                                             objectsStore: LDBKVStore,
                                             indexStore: LDBKVStore): Try[Unit] = Try {
    if (Files.exists(journalPath)) {
      val bytes = Files.readAllBytes(journalPath)
      if (bytes.length < JournalHeaderSize) {
        log.warn(s"Discarding incomplete history batch journal ($journalPath, ${bytes.length} bytes)")
        Files.deleteIfExists(journalPath)
      } else {
        val header = ByteBuffer.wrap(bytes, 0, JournalHeaderSize)
        val magic = header.getInt()
        val version = header.get()
        val active = header.get()
        val payloadLen = header.getInt()
        val expectedCrc = header.getInt()
        if (magic != JournalMagic || version != JournalVersion) {
          log.warn(s"Discarding unrecognized history batch journal header at $journalPath")
          Files.deleteIfExists(journalPath)
        } else if (active == 0.toByte) {
          Files.deleteIfExists(journalPath)
        } else if (active != 1.toByte || payloadLen < 0 || bytes.length - JournalHeaderSize < payloadLen) {
          log.warn(s"Discarding truncated history batch journal at $journalPath")
          Files.deleteIfExists(journalPath)
        } else {
          val payload = java.util.Arrays.copyOfRange(bytes, JournalHeaderSize, JournalHeaderSize + payloadLen)
          val actualCrc = computeCrc32(version, payloadLen, payload)
          if (actualCrc != expectedCrc) {
            log.warn(s"Discarding history batch journal with mismatched checksum at $journalPath")
            Files.deleteIfExists(journalPath)
          } else {
            val record = decodePayload(payload).get
            log.warn(
              s"Recovering interrupted history batch from $journalPath " +
                s"(${record.objectEntries.length} objects, ${record.indexEntries.length} indexes)"
            )
            applyUndoEntries(objectsStore, record.objectEntries).get
            applyUndoEntries(indexStore, record.indexEntries).get
            Files.deleteIfExists(journalPath)
          }
        }
      }
    }
  }

  private[storage] def deleteRecursively(root: Path, deletePath: Path => Unit): Try[Unit] = Try {
    if (Files.exists(root)) {
      val paths = Files.walk(root)
      try paths.iterator().asScala.toSeq.reverse.foreach(deletePath)
      finally paths.close()
    }
    require(!Files.exists(root), s"Could not delete $root")
  }

  def apply(ergoSettings: ErgoSettings): HistoryStorage = {
    val indexStore = LDBFactory.createKvDb(s"${ergoSettings.directory}/history/index")
    val objectsStore = LDBFactory.createKvDb(s"${ergoSettings.directory}/history/objects")
    val extraStore = LDBFactory.createKvDb(s"${ergoSettings.directory}/history/extra")
    val journalPath = Paths.get(ergoSettings.directory, "history", PendingJournalFileName)
    try {
      recoverPendingJournal(journalPath, objectsStore, indexStore).get
      new HistoryStorage(indexStore, objectsStore, extraStore, ergoSettings.cacheSettings, Some(journalPath))
    } catch {
      case t: Throwable =>
        Try(extraStore.close())
        Try(indexStore.close())
        Try(objectsStore.close())
        throw t
    }
  }
}
