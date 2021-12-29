package org.ergoplatform.nodeView.history.storage

import com.google.common.cache.CacheBuilder
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.settings.{Algos, CacheSettings}
import scorex.core.utils.ScorexEncoding
import scorex.db.{ByteArrayWrapper, LDBKVStore}
import scorex.util.{ModifierId, ScorexLogging, idToBytes}

import scala.util.{Failure, Success, Try}

/**
  * Storage for Ergo history
  *
  * @param indexStore   - Additional key-value storage for indexes, required by History for efficient work.
  *                     contains links to bestHeader, bestFullBlock, heights and scores for different blocks, etc.
  * @param objectsStore - key-value store, where key is id of ErgoPersistentModifier and value is it's bytes
  * @param config       - cache configs
  */
class HistoryStorage(indexStore: LDBKVStore, objectsStore: LDBKVStore, config: CacheSettings)
  extends ScorexLogging
    with AutoCloseable
    with ScorexEncoding {

  private val modifiersCache = CacheBuilder.newBuilder()
    .maximumSize(config.history.modifiersCacheSize)
    .build[String, BlockSection]

  private val indexCache = CacheBuilder.newBuilder()
    .maximumSize(config.history.indexesCacheSize)
    .build[ByteArrayWrapper, Array[Byte]]

  def modifierBytesById(id: ModifierId): Option[Array[Byte]] =
    objectsStore.get(idToBytes(id)).map(_.tail)

  def modifierById(id: ModifierId): Option[BlockSection] =
    Option(modifiersCache.getIfPresent(id)) orElse
      objectsStore.get(idToBytes(id)).flatMap { bytes =>
        HistoryModifierSerializer.parseBytesTry(bytes) match {
          case Success(pm) =>
            log.trace(s"Cache miss for existing modifier $id")
            modifiersCache.put(id, pm)
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

  def get(id: ModifierId): Option[Array[Byte]] = objectsStore.get(idToBytes(id))

  def contains(id: ModifierId): Boolean = objectsStore.get(idToBytes(id)).isDefined

  def insert(indexesToInsert: Seq[(ByteArrayWrapper, Array[Byte])],
             objectsToInsert: Seq[BlockSection]): Try[Unit] = {
    objectsStore.insert(
      objectsToInsert.map(m => idToBytes(m.id) -> HistoryModifierSerializer.toBytes(m))
    ).flatMap { _ =>
      objectsToInsert.foreach(o => modifiersCache.put(o.id, o))
      if (indexesToInsert.nonEmpty) {
        indexStore.insert(indexesToInsert.map { case (k, v) => k.data -> v }).map { _ =>
          indexesToInsert.foreach(kv => indexCache.put(kv._1, kv._2))
          ()
        }
      } else Success(())
    }
  }

  /**
    * Remove elements from stored indices and modifiers
    *
    * @param indicesToRemove - indices keys to remove
    * @param idsToRemove - identifiers of modifiers to remove
    * @return
    */
  def remove(indicesToRemove: Seq[ByteArrayWrapper],
             idsToRemove: Seq[ModifierId]): Try[Unit] = {

      objectsStore.remove(idsToRemove.map(idToBytes)).map { _ =>
        idsToRemove.foreach { id =>
          modifiersCache.invalidate(id)
        }
        indexStore.remove(indicesToRemove.map(_.data)).map { _ =>
          idsToRemove.foreach { id =>
            indexCache.invalidate(id)
          }
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
