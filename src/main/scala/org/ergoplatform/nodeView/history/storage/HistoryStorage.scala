package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.core.ModifierId
import scorex.core.utils.ScorexLogging

class HistoryStorage(indexStore: Store, objectsStore: ObjectsStore) extends ScorexLogging with AutoCloseable {

  def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = objectsStore.get(id)

  def getIndex(id: ByteArrayWrapper): Option[ByteArrayWrapper] = indexStore.get(id)

  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

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
    log.info("Closing history storage...")
    indexStore.close()
  }
}
