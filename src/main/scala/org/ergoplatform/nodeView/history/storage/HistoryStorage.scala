package org.ergoplatform.nodeView.history.storage

import com.google.common.cache.CacheBuilder
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.ErgoApp
import org.ergoplatform.db.LDBKVStore
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.settings.{Algos, CacheSettings}
import scorex.core.utils.ScorexEncoding
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
    .maximumSize(config.modifiersCacheSize)
    .build[String, ErgoPersistentModifier]

  private val indexCache = CacheBuilder.newBuilder()
    .maximumSize(config.indexesCacheSize)
    .build[ByteArrayWrapper, Array[Byte]]

  def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = {
    Option(modifiersCache.getIfPresent(id)) match {
      case Some(e) =>
        log.trace(s"Got modifier $id from cache")
        Some(e)
      case None =>
        objectsStore.get(idToBytes(id)).flatMap { bBytes =>
          HistoryModifierSerializer.parseBytesTry(bBytes) match {
            case Success(pm) =>
              log.trace(s"Cache miss for existing modifier $id")
              modifiersCache.put(id, pm)
              Some(pm)
            case Failure(_) =>
              log.warn(s"Failed to parse modifier ${encoder.encode(id)} from db (bytes are: ${Algos.encode(bBytes)})")
              None
          }
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

  def get(id: ModifierId): Option[Array[Byte]] = objectsStore.get(idToBytes(id))

  def contains(id: ModifierId): Boolean = objectsStore.get(idToBytes(id)).isDefined

  def insert(indexesToInsert: Seq[(ByteArrayWrapper, Array[Byte])],
             objectsToInsert: Seq[ErgoPersistentModifier]): Unit = Try {
    objectsToInsert.foreach(o => modifiersCache.put(o.id, o))
    objectsStore.insert(objectsToInsert.map(m => idToBytes(m.id) -> HistoryModifierSerializer.toBytes(m)))
    if (indexesToInsert.nonEmpty) {
      indexesToInsert.foreach(kv => indexCache.put(kv._1, kv._2))
      indexStore.insert(indexesToInsert.map { case (k, v) => k.data -> v })
    }
  }.recoverWith { case e =>
    log.error("Unable to perform insert", e)
    ErgoApp.forceStopApplication()
  }

  def remove(idsToRemove: Seq[ModifierId]): Unit = {
    idsToRemove.foreach { id =>
      modifiersCache.invalidate(id)
    }
    objectsStore.remove(idsToRemove.map(idToBytes))
  }

  override def close(): Unit = {
    log.warn("Closing history storage...")
    indexStore.close()
    objectsStore.close()
  }

}
