package org.ergoplatform.nodeView.history.storage

import com.github.benmanes.caffeine.cache.Caffeine
import org.ergoplatform.modifiers.{BlockSection, NetworkObjectTypeId}
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, ExtraIndexSerializer, Segment}
import org.ergoplatform.settings.{Algos, CacheSettings, ErgoSettings}
import org.ergoplatform.utils.ScorexEncoding
import scorex.db.{ByteArrayWrapper, LDBFactory, LDBKVStore}
import scorex.util.{ModifierId, ScorexLogging, idToBytes}

import scala.util.{Failure, Success, Try}
import spire.syntax.all.cfor

import java.io.File
import java.nio.file.{Files, Path}
import java.util.concurrent.locks.ReentrantReadWriteLock
import scala.jdk.CollectionConverters.asScalaIteratorConverter

/**
  * Storage for Ergo history
  *
  * @param indexStore   - Additional key-value storage for indexes, required by History for efficient work.
  *                     contains links to bestHeader, bestFullBlock, heights and scores for different blocks, etc.
  * @param objectsStore - key-value store, where key is id of ErgoPersistentModifier and value is it's bytes
  * @param extraStore   - key-value store, where key is id of Index and value is it's bytes
  * @param config       - cache configs
  */
class HistoryStorage(indexStore: LDBKVStore, objectsStore: LDBKVStore, extraStore: LDBKVStore, config: CacheSettings)
  extends ScorexLogging
    with AutoCloseable
    with ScorexEncoding {

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

  private val extraCacheLock = new ReentrantReadWriteLock()

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

  def modifierBytesById(id: ModifierId): Option[Array[Byte]] = {
    objectsStore.get(idToBytes(id)).map(_.tail).orElse(extraStore.get(idToBytes(id))) // removing modifier type byte with .tail (only in objectsStore)
  }

  /**
    * @return bytes and type of a network object stored in the database with identifier `id`
    */
  def modifierTypeAndBytesById(id: ModifierId): Option[(NetworkObjectTypeId.Value, Array[Byte])] = {
    objectsStore.get(idToBytes(id)).map(bs => (NetworkObjectTypeId.fromByte(bs.head), bs.tail)) // first byte is type id, tail is modifier bytes
  }

  def modifierById(id: ModifierId): Option[BlockSection] =
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

  def getIndex(id: ByteArrayWrapper): Option[Array[Byte]] =
    Option(indexCache.getIfPresent(id)).orElse {
      indexStore.get(id.data).map { value =>
        indexCache.put(id, value)
        value
      }
    }

  /**
    * @return object with `id` if it is in the objects database
    */
  def get(id: ModifierId): Option[Array[Byte]] = {
    val idBytes = idToBytes(id)
    objectsStore.get(idBytes).orElse(extraStore.get(idBytes))
  }
  def get(id: Array[Byte]): Option[Array[Byte]] = objectsStore.get(id).orElse(extraStore.get(id))

  /**
    * @return if object with `id` is in the objects database
    */
  def contains(id: Array[Byte]): Boolean = get(id).isDefined
  def contains(id: ModifierId): Boolean = get(id).isDefined

  def insert(indexesToInsert: Array[(ByteArrayWrapper, Array[Byte])],
             objectsToInsert: Array[BlockSection]): Try[Unit] = {
    objectsStore.insert(
      objectsToInsert.map(mod => mod.serializedId),
      objectsToInsert.map(mod => HistoryModifierSerializer.toBytes(mod))
    ).flatMap { _ =>
      cfor(0)(_ < objectsToInsert.length, _ + 1) { i => cacheModifier(objectsToInsert(i))}
      if (indexesToInsert.nonEmpty) {
        indexStore.insert(
          indexesToInsert.map(_._1.data),
          indexesToInsert.map(_._2)
        ).map { _ =>
          cfor(0)(_ < indexesToInsert.length, _ + 1) { i =>
            indexCache.put(indexesToInsert(i)._1, indexesToInsert(i)._2)
          }
        }
      } else Success(())
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
                     objectsToInsert: Array[ExtraIndex]): Try[Unit] = {
    val objectIds = objectsToInsert.iterator.flatMap(obj => Try(obj.id).toOption).toArray
    Try {
      val keys = objectsToInsert.map(_.serializedId) ++ indexesToInsert.map(_._1)
      val values = objectsToInsert.map(ExtraIndexSerializer.toBytes) ++ indexesToInsert.map(_._2)
      keys -> values
    }.flatMap { case (keys, values) =>
      withExtraCacheWriteLock {
        extraStore.insert(keys, values).map { _ =>
          objectIds.foreach(extraCache.invalidate)
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
             objectToInsert: Array[Byte]): Try[Unit] = {
    objectsStore.insert(objectIdToInsert, objectToInsert)
  }

  /**
    * Remove elements from stored indices and modifiers
    *
    * @param indicesToRemove - indices keys to remove
    * @param idsToRemove - identifiers of modifiers to remove
    * @return
    */
  def remove(indicesToRemove: Array[ByteArrayWrapper],
             idsToRemove: Array[ModifierId]): Try[Unit] = {

      objectsStore.remove(idsToRemove.map(idToBytes)).map { _ =>
        cfor(0)(_ < idsToRemove.length, _ + 1) { i => removeModifier(idsToRemove(i))}
        indexStore.remove(indicesToRemove.map(_.data)).map { _ =>
          cfor(0)(_ < indicesToRemove.length, _ + 1) { i => indexCache.invalidate(indicesToRemove(i))}
          ()
        }
      }
  }

  override def close(): Unit = {
    log.warn("Closing history storage...")
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
    log.warn(s"Removing extra index database due to old schema.")
    val root = new File(s"${ergoSettings.directory}/history/extra")
    Try(close()).flatMap { _ =>
      HistoryStorage.deleteRecursively(root.toPath, Files.delete)
    }.map { _ =>
      log.info(s"Deleted ${root.toString}")
      HistoryStorage.apply(ergoSettings)
    }
  }

}

object HistoryStorage {
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
    new HistoryStorage(indexStore, objectsStore, extraStore, ergoSettings.cacheSettings)
  }
}
