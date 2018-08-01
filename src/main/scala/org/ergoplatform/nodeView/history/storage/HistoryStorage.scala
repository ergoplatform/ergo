package org.ergoplatform.nodeView.history.storage

import com.google.common.cache.CacheBuilder
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.settings.{Algos, CacheSettings}
import scorex.core.ModifierId
import scorex.core.utils.{ScorexEncoding, ScorexLogging}

import scala.concurrent.Future
import scala.util.Failure

class HistoryStorage(indexStore: Store, objectsStore: ObjectsStore, config: CacheSettings) extends ScorexLogging
  with AutoCloseable with ScorexEncoding {

  private val modifiersCache = CacheBuilder.newBuilder()
    .maximumSize(config.modifiersCacheSize)
    .build[String, ErgoPersistentModifier]

  private val indexCache = CacheBuilder.newBuilder()
    .maximumSize(config.indexesCacheSize)
    .build[ByteArrayWrapper, ByteArrayWrapper]


  def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = {
    Option(modifiersCache.getIfPresent(id)) match {
      case Some(e) =>
        log.trace(s"Got modifier $id from cache")
        Some(e)
      case None =>
        objectsStore.get(id).flatMap { bBytes =>
          HistoryModifierSerializer.parseBytes(bBytes).recoverWith { case e =>
            log.warn(s"Failed to parse modifier ${encoder.encode(id)} from db (bytes are: ${Algos.encode(bBytes)})")
            Failure(e)
          }.toOption match {
            case Some(pm) =>
              log.trace(s"Cache miss for existing modifier $id")
              modifiersCache.put(id, pm)
              Some(pm)
            case None => None
          }
        }
    }
  }

  def getIndex(id: ByteArrayWrapper): Option[ByteArrayWrapper] = Option(indexCache.getIfPresent(id)).orElse {
    indexStore.get(id).map { value =>
      indexCache.put(id, value)
      value
    }
  }

  def get(id: ModifierId): Option[Array[Byte]] = objectsStore.get(id)

  def contains(id: ModifierId): Boolean = objectsStore.contains(id)

  def insert(id: ByteArrayWrapper,
             indexesToInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)],
             objectsToInsert: Seq[ErgoPersistentModifier]): Unit = {
    objectsToInsert.foreach { o =>
      modifiersCache.put(o.id, o)
      objectsStore.put(o)
    }
    if (indexesToInsert.nonEmpty) {
      indexesToInsert.foreach(kv => indexCache.put(kv._1, kv._2))
      indexStore.update(
        id,
        Seq.empty,
        indexesToInsert)
    }
  }

  def remove(idsToRemove: Seq[ModifierId]): Unit = idsToRemove.foreach { id =>
    modifiersCache.invalidate(id)
    objectsStore.delete(id)
  }

  override def close(): Unit = {
    log.warn("Closing history storage...")
    indexStore.close()
  }
}
