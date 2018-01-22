package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.{ByteArrayWrapper, Store}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.HistoryModifierSerializer
import scorex.core.ModifierId
import scorex.core.utils.ScorexLogging

import scala.util.{Failure, Success}

class HistoryStorage(db: Store) extends ScorexLogging with AutoCloseable {

  def modifierById(id: ModifierId): Option[ErgoPersistentModifier] = db.get(ByteArrayWrapper(id)).flatMap { bBytes =>
    HistoryModifierSerializer.parseBytes(bBytes.data) match {
      case Success(b) =>
        Some(b)
      case Failure(e) =>
        log.warn(s"Failed to parse block from db (bytes are: ${bBytes.data.mkString("-")}): ", e)
        None
    }
  }

  def get(id: ByteArrayWrapper): Option[ByteArrayWrapper] = db.get(id)

  def contains(id: ModifierId): Boolean = db.get(ByteArrayWrapper(id)).isDefined

  def insert(id: ModifierId, toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = update(id, Seq(), toInsert)

  def update(id: ByteArrayWrapper,
             idsToRemove: Seq[ByteArrayWrapper],
             toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(
      id,
      idsToRemove,
      toInsert)
  }

  def update(id: ModifierId,
             idsToRemove: Seq[ByteArrayWrapper],
             toInsert: Seq[(ByteArrayWrapper, ByteArrayWrapper)]): Unit = {
    db.update(
      ByteArrayWrapper(id),
      idsToRemove,
      toInsert)
  }

  override def close(): Unit = {
    log.info("Closing history storage...")
    db.close()
  }
}
