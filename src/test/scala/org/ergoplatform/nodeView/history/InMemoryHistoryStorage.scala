package org.ergoplatform.nodeView.history

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import scorex.util.ModifierId

import scala.collection.mutable

final class InMemoryHistoryStorage extends HistoryStorage {

  val indexes: mutable.Map[ByteArrayWrapper, Array[Byte]] = mutable.Map.empty
  val objects: mutable.Map[ModifierId, ErgoPersistentModifier] = mutable.Map.empty

  def modifierById(id: ModifierId): Option[ErgoPersistentModifier] =
    objects.get(id)

  def getIndex(id: ByteArrayWrapper): Option[Array[Byte]] =
    indexes.get(id)

  def get(id: ModifierId): Option[Array[Byte]] = None

  def contains(id: ModifierId): Boolean = objects.contains(id)

  def insert(indexesToInsert: Seq[(ByteArrayWrapper, Array[Byte])],
             objectsToInsert: Seq[ErgoPersistentModifier]): Unit = {
    indexes ++= indexesToInsert
    objects ++= objectsToInsert.map(x => x.id -> x)
  }

  def remove(idsToRemove: Seq[ModifierId]): Unit = objects --= idsToRemove

  override def close(): Unit = ()

}
