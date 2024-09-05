package org.ergoplatform.nodeView.history.storage

import com.github.benmanes.caffeine.cache.Caffeine
import org.ergoplatform.modifiers.{BlockSection, NetworkObjectTypeId}
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.extra.{ExtraIndex, ExtraIndexSerializer, Segment}
import org.ergoplatform.settings.{Algos, CacheSettings, ErgoSettings}
import org.ergoplatform.utils.ScorexEncoding
import scorex.db.{ByteArrayWrapper, RocksDBFactory, RocksDBKVStore}
import scorex.util.{ModifierId, ScorexLogging, idToBytes}

import scala.util.{Failure, Success, Try}
import spire.syntax.all.cfor

import java.io.File
import java.nio.file.Files
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
class HistoryStorage private(indexStore: RocksDBKVStore, objectsStore: RocksDBKVStore, extraStore: RocksDBKVStore, config: CacheSettings)
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
          if(pm.isInstanceOf[Segment[_]]){
            extraCache.put(pm.id, pm) // cache all segment type objects
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
  def contains(id: Array[Byte]): Boolean = objectsStore.contains(id) || extraStore.contains(id)
  def contains(id: ModifierId): Boolean = {
    val idBytes = idToBytes(id)
    objectsStore.contains(idBytes) || extraStore.contains(idBytes)
  }

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
    extraStore.insert(
      objectsToInsert.map(mod => mod.serializedId),
      objectsToInsert.map(mod => ExtraIndexSerializer.toBytes(mod))
    )
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

  /**
    * Delete the extra index database and reopen it.
    *
    * @param ergoSettings - settings to use
    * @return new HistoryStorage instance with empty extra database, or this instance in case of failure
    */
  def deleteExtraDB(ergoSettings: ErgoSettings): HistoryStorage = {
    log.warn(s"Removing extra index database due to old schema.")
    close()
    // org.ergoplatform.wallet.utils.FileUtils
    val root = new File(s"${ergoSettings.directory}/history/extra")
    if (root.exists()) {
      Files.walk(root.toPath).iterator().asScala.toSeq.reverse.foreach(path => Try(Files.delete(path)))
    }else {
      log.error(s"Could not delete ${root.toString}")
      return this
    }
    log.info(s"Deleted ${root.toString}")
    HistoryStorage.apply(ergoSettings)
  }

}

object HistoryStorage {
  def apply(ergoSettings: ErgoSettings): HistoryStorage = {
    val indexStore = new RocksDBKVStore(RocksDBFactory.open(new File(s"${ergoSettings.directory}/history/index")))
    val objectsStore = new RocksDBKVStore(RocksDBFactory.open(new File(s"${ergoSettings.directory}/history/objects")))
    val extraStore = new RocksDBKVStore(RocksDBFactory.open(new File(s"${ergoSettings.directory}/history/extra")))
    new HistoryStorage(indexStore, objectsStore, extraStore, ergoSettings.cacheSettings)
  }
}
