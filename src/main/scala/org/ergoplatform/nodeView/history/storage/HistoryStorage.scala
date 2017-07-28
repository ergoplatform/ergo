package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.utils.ScorexLogging

import scala.util.{Failure, Success}

class HistoryStorage(val db: LSMStore) extends ScorexLogging with AutoCloseable {


  def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = db.get(ByteArrayWrapper(id)).flatMap { bBytes =>
    HistoryModifierSerializer.parseBytes(bBytes.data) match {
      case Success(b) =>
        Some(b)
      case Failure(e) =>
        log.warn("Failed to parse block from db", e)
        None
    }
  }

  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

  def insert(id: ModifierId, toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(
      ByteArrayWrapper(id),
      Seq(),
      toInsert)
  }

  def remove(id: ModifierId, idsToRemove: Seq[ByteArrayWrapper]): Unit = {
    db.update(
      ByteArrayWrapper(id),
      ByteArrayWrapper(id) +: idsToRemove,
      Seq())
  }


  override def close(): Unit = {
    log.info("Closing history storage...")
    db.close()
  }

}
