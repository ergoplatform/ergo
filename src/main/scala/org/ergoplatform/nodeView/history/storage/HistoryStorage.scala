package org.ergoplatform.nodeView.history.storage

import com.github.benmanes.caffeine.cache.Caffeine
import org.ergoplatform.modifiers.{BlockSection, NetworkObjectTypeId}
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.extra.{ExtraIndexSerializer, ExtraIndex, IndexedErgoAddress}
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
  * @param extraStore   - key-value store, where key is id of Index and value is it's bytes
  * @param config       - cache configs
  */
class HistoryStorage private(indexStore: LDBKVStore, objectsStore: LDBKVStore, extraStore: LDBKVStore, config: CacheSettings)
  extends ScorexLogging
    with AutoCloseable
    with ScorexEncoding {

  private val headersCache =
    Caffeine.newBuilder()
      .maximumSize(config.history.headersCacheSize)
      .build[String, BlockSection]()

  private val blockSectionsCache =
    Caffeine.newBuilder()
      .maximumSize(config.history.blockSectionsCacheSize)
      .build[String, BlockSection]()

  private val extraCache =
    Caffeine.newBuilder()
      .maximumSize(config.history.extraCacheSize)
      .build[String, ExtraIndex]()

  private val indexCache =
    Caffeine.newBuilder()
      .maximumSize(config.history.indexesCacheSize)
      .build[ByteArrayWrapper, Array[Byte]]

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
        case Failure(_) =>
          log.warn(s"Failed to parse modifier ${encoder.encode(id)} from db (bytes are: ${Algos.encode(bytes)})")
          None
      }
    }

  def getExtraIndex(id: ModifierId): Option[ExtraIndex] = {
    Option(extraCache.getIfPresent(id)) orElse extraStore.get(idToBytes(id)).flatMap { bytes =>
      ExtraIndexSerializer.parseBytesTry(bytes) match {
        case Success(pm) =>
          log.trace(s"Cache miss for existing index $id")
          if(pm.isInstanceOf[IndexedErgoAddress]){
            extraCache.put(pm.id, pm) // only cache addresses
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

  def get(id: ModifierId): Option[Array[Byte]] = objectsStore.get(idToBytes(id)).orElse(extraStore.get(idToBytes(id)))
  def get(id: Array[Byte]): Option[Array[Byte]] = objectsStore.get(id).orElse(extraStore.get(id))

  def contains(id: Array[Byte]): Boolean = get(id).isDefined
  def contains(id: ModifierId): Boolean = get(id).isDefined

  def insert(indexesToInsert: Array[(ByteArrayWrapper, Array[Byte])],
             objectsToInsert: Array[BlockSection]): Try[Unit] = {
    objectsStore.insert(
      objectsToInsert.map(mod => (mod.serializedId, HistoryModifierSerializer.toBytes(mod)))
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
                  objectsToInsert: Array[ExtraIndex]): Unit = {
    extraStore.insert(objectsToInsert.map(mod => (mod.serializedId, ExtraIndexSerializer.toBytes(mod))))
    cfor(0)(_ < objectsToInsert.length, _ + 1) { i => val ei = objectsToInsert(i); extraCache.put(ei.id, ei)}
    cfor(0)(_ < indexesToInsert.length, _ + 1) { i => extraStore.insert(indexesToInsert(i)._1, indexesToInsert(i)._2)}
  }

  def removeExtra(indexesToRemove: Array[ModifierId]) : Unit = {
    extraStore.remove(indexesToRemove.map(idToBytes))
    cfor(0)(_ < indexesToRemove.length, _ + 1) { i => removeModifier(indexesToRemove(i)) }
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

}

object HistoryStorage {
  def apply(ergoSettings: ErgoSettings): HistoryStorage = {
    val indexStore = LDBFactory.createKvDb(s"${ergoSettings.directory}/history/index")
    val objectsStore = LDBFactory.createKvDb(s"${ergoSettings.directory}/history/objects")
    val extraStore = LDBFactory.createKvDb(s"${ergoSettings.directory}/history/extra")
    new HistoryStorage(indexStore, objectsStore, extraStore, ergoSettings.cacheSettings)
  }
}
