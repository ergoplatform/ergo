package org.ergoplatform.nodeView.history.storage

import com.google.common.cache.CacheBuilder
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddress
import org.ergoplatform.settings.{Algos, CacheSettings, ErgoSettings}
import scorex.core.utils.ScorexEncoding
import scorex.db.{ByteArrayWrapper, LDBFactory, LDBKVStore}
import scorex.util.{ModifierId, ScorexLogging, idToBytes}

import scala.util.{Failure, Success, Try}
import spire.syntax.all.cfor

/**
  * Storage for Ergo history
  *
  * @param indexStore   - Additional key-value storage for indexes, required by History for efficient work.
  *                     contains links to bestHeader, bestFullBlock, heights and scores for different blocks, etc.
  * @param objectsStore - key-value store, where key is id of ErgoPersistentModifier and value is it's bytes
  * @param config       - cache configs
  */
class HistoryStorage private(indexStore: LDBKVStore, objectsStore: LDBKVStore, extraStore: LDBKVStore, config: CacheSettings)
  extends ScorexLogging
    with AutoCloseable
    with ScorexEncoding {

  private val headersCache = CacheBuilder.newBuilder()
    .maximumSize(config.history.headersCacheSize)
    .build[String, BlockSection]

  private val blockSectionsCache = CacheBuilder.newBuilder()
    .maximumSize(config.history.blockSectionsCacheSize)
    .build[String, BlockSection]

  private val extraCache = CacheBuilder.newBuilder()
    .maximumSize(config.history.extraCacheSize)
    .build[String, BlockSection]

  private val indexCache = CacheBuilder.newBuilder()
    .maximumSize(config.history.indexesCacheSize)
    .build[ByteArrayWrapper, Array[Byte]]

  private def cacheModifier(mod: BlockSection): Unit = mod.modifierTypeId match {
    case Header.modifierTypeId => headersCache.put(mod.id, mod)
    case IndexedErgoAddress.modifierTypeId => extraCache.put(mod.id, mod) // only cache "big" modifiers
    case _ => blockSectionsCache.put(mod.id, mod)
  }

  private def lookupModifier(id: ModifierId): Option[BlockSection] =
    Option(extraCache.getIfPresent(id)) orElse Option(headersCache.getIfPresent(id)) orElse Option(blockSectionsCache.getIfPresent(id))

  private def removeModifier(id: ModifierId): Unit = {
    headersCache.invalidate(id)
    blockSectionsCache.invalidate(id)
    extraCache.invalidate(id)
  }

  def modifierBytesById(id: ModifierId): Option[Array[Byte]] = {
    objectsStore.get(idToBytes(id)).map(_.tail) // removing modifier type byte with .tail
  }

  def modifierById(id: ModifierId): Option[BlockSection] =
    lookupModifier(id) orElse
      (extraStore.get(idToBytes(id)) orElse objectsStore.get(idToBytes(id))).flatMap { bytes =>
        HistoryModifierSerializer.parseBytesTry(bytes) match {
          case Success(pm) =>
            log.trace(s"Cache miss for existing modifier $id")
            cacheModifier(pm)
            Some(pm)
          case Failure(_) =>
            log.warn(s"Failed to parse modifier ${encoder.encode(id)} from db (bytes are: ${Algos.encode(bytes)})")
            None
        }
      }

  def getIndex(id: ByteArrayWrapper): Option[Array[Byte]] =
    Option(indexCache.getIfPresent(id)).orElse {
      indexStore.get(id.data).map { value =>
        indexCache.put(id, value)
        value
      }
    }

  def get(id: ModifierId): Option[Array[Byte]] = objectsStore.get(idToBytes(id)).orElse(extraStore.get(idToBytes(id)))

  def contains(id: ModifierId): Boolean = objectsStore.get(idToBytes(id)).isDefined

  def insert(indexesToInsert: Array[(ByteArrayWrapper, Array[Byte])],
             objectsToInsert: Array[BlockSection]): Try[Unit] = {
    objectsStore.insert(
      objectsToInsert.map(m => m.serializedId -> HistoryModifierSerializer.toBytes(m))
    ).flatMap { _ =>
      cfor(0)(_ < objectsToInsert.length, _ + 1) { i => cacheModifier(objectsToInsert(i))}
      if (indexesToInsert.nonEmpty) {
        indexStore.insert(indexesToInsert.map { case (k, v) => k.data -> v }).map { _ =>
          cfor(0)(_ < indexesToInsert.length, _ + 1) { i => indexCache.put(indexesToInsert(i)._1, indexesToInsert(i)._2)}
          ()
        }
      } else Success(())
    }
  }

  def insertExtra(indexesToInsert: Array[(Array[Byte], Array[Byte])],
                  objectsToInsert: Array[BlockSection]): Unit = {
    extraStore.insert(objectsToInsert.map(m => m.serializedId -> HistoryModifierSerializer.toBytes(m)))
    cfor(0)(_ < objectsToInsert.length, _ + 1) { i => cacheModifier(objectsToInsert(i))}
    cfor(0)(_ < indexesToInsert.length, _ + 1) { i => extraStore.insert(indexesToInsert(i)._1, indexesToInsert(i)._2)}
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
    indexStore.close()
    objectsStore.close()
  }

}

object HistoryStorage {
  def apply(ergoSettings: ErgoSettings): HistoryStorage = {
    val indexStore = LDBFactory.createKvDb(s"${ergoSettings.directory}/history/index")
    val objectsStore = LDBFactory.createKvDb(s"${ergoSettings.directory}/history/objects")
    val extraStore = LDBFactory.createKvDb(s"${ergoSettings.directory}/history/extra")
    new HistoryStorage(indexStore, objectsStore, extraStore, ergoSettings.cacheSettings)
  }
}
