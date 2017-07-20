package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.history.{Header, HistoryModifier, HistoryModifierSerializer}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success}

class HistoryStorage(val db: LSMStore) extends ScorexLogging with AutoCloseable {

  val BestFullBlockKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(32)(-1))

  val BestHeaderKey: ByteArrayWrapper = ByteArrayWrapper(Array.fill(32)(Header.ModifierTypeId))

  def bestHeaderIdWithTransactions: Option[ModifierId] = db.get(BestFullBlockKey).map(_.data)

  def bestHeaderId: Option[ModifierId] = db.get(BestHeaderKey).map(_.data)

  def modifierById(id: ModifierId): Option[HistoryModifier] = db.get(ByteArrayWrapper(id)).flatMap { bBytes =>
    HistoryModifierSerializer.parseBytes(bBytes.data) match {
      case Success(b) =>
        Some(b)
      case Failure(e) =>
        log.warn("Failed to parse block from db", e)
        None
    }
  }

  def contains(id: ModifierId): Boolean = modifierById(id).isDefined

  def insert(id: ModifierId, toInsert: Seq[(ByteArrayWrapper,ByteArrayWrapper)]): Unit = {
    db.update(
      ByteArrayWrapper(id),
      Seq(),
      toInsert)
  }

  def drop(id: ModifierId, idsToRemove: Seq[ByteArrayWrapper]): Unit = {
    db.update(
      ByteArrayWrapper(Blake2b256(id ++ "drop".getBytes)),
      ByteArrayWrapper(id) +: idsToRemove,
      Seq())
  }


  override def close(): Unit = {
    log.info("Closing history storage...")
    db.close()
  }

}
