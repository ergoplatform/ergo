package org.ergoplatform.nodeView.history.storage

import com.google.common.cache.CacheBuilder
import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import org.ergoplatform.settings.Algos
import scorex.core.ModifierId
import scorex.core.utils.{ScorexEncoding, ScorexLogging}

import scala.util.Failure
import scalacache._
import scalacache.guava._
import scalacache.modes.try_.mode

class HistoryStorage(indexStore: Store, objectsStore: ObjectsStore) extends ScorexLogging with AutoCloseable
  with ScorexEncoding {

  // TODO move size to config?
  val underlyingGuavaCache = CacheBuilder.newBuilder().maximumSize(100L).build[String, Entry[ErgoPersistentModifier]]
  implicit val modifiersCache: Cache[ErgoPersistentModifier] = GuavaCache(underlyingGuavaCache)

  def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = {
    objectsStore.get(id).flatMap { bBytes =>
      cachingF(Algos.encode(id))(ttl = None) {
        HistoryModifierSerializer.parseBytes(bBytes).recoverWith { case e =>
          log.warn(s"Failed to parse modifier ${encoder.encode(id)} from db (bytes are: ${bBytes.mkString("-")}): ", e)
          Failure(e)
        }
      }.toOption
    }
  }


  def getIndex(id: ByteArrayWrapper): Option[ByteArrayWrapper] = indexStore.get(id)

  def get(id: ModifierId): Option[Array[Byte]] = objectsStore.get(id)

  def contains(id: ModifierId): Boolean = objectsStore.contains(id)

  def insert(id: ByteArrayWrapper,
             indexesToInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)],
             objectsToInsert: Seq[ErgoPersistentModifier]): Unit = {
    objectsToInsert.foreach(o => objectsStore.put(o))
    indexStore.update(
      id,
      Seq.empty,
      indexesToInsert)
  }

  def remove(idsToRemove: Seq[ModifierId]): Unit = idsToRemove.foreach(id => objectsStore.delete(id))

  override def close(): Unit = {
    log.warn("Closing history storage...")
    indexStore.close()
  }
}
