package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import scorex.util.ModifierId

abstract class HistoryStorage extends AutoCloseable {

  def modifierById(id: ModifierId): Option[ErgoPersistentModifier]

  def getIndex(id: ByteArrayWrapper): Option[Array[Byte]]

  def get(id: ModifierId): Option[Array[Byte]]

  def contains(id: ModifierId): Boolean

  def update(indexesToInsert: Seq[(ByteArrayWrapper, Array[Byte])],
             objectsToInsert: Seq[ErgoPersistentModifier]): Unit

  def remove(idsToRemove: Seq[ModifierId]): Unit

  def updateIndex(key: ByteArrayWrapper, value: Array[Byte]): Unit =
    update(Seq(key -> value), Seq.empty)

}
