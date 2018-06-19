package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import scorex.core.ModifierId
import scorex.core.utils.{ScorexEncoding, ScorexLogging}

import scala.util.{Failure, Success}

class HistoryStorage(indexStore: Store, objectsStore: ObjectsStore) extends ScorexLogging with AutoCloseable
  with ScorexEncoding {

  def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = objectsStore.get(id)
    .flatMap { bBytes =>
      HistoryModifierSerializer.parseBytes(bBytes) match {
        case Success(b) =>
          Some(b)
        case Failure(e) =>
          log.warn(s"Failed to parse modifier ${encoder.encode(id)} from db (bytes are: ${bBytes.mkString("-")}): ", e)
          None
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
