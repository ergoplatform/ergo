package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.{Header, HistoryModifier, HistoryModifierSerializer}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success}

trait HistoryStorage extends ScorexLogging with HeadersProcesser {

  def modifierById(id: ModifierId): Option[HistoryModifier] = storage.get(ByteArrayWrapper(id)).flatMap { bBytes =>
    HistoryModifierSerializer.parseBytes(bBytes.data) match {
      case Success(b) =>
        Some(b)
      case Failure(e) =>
        log.warn("Failed to parse block from db", e)
        None
    }
  }

  def insert(b: HistoryModifier): Unit = {
    //TODO calculate
    val requiredDifficulty: BigInt = 1
    val indexRows = indexes(b, requiredDifficulty)
    storage.update(
      ByteArrayWrapper(b.id),
      Seq(),
      indexRows :+ (ByteArrayWrapper(b.id) -> ByteArrayWrapper(HistoryModifierSerializer.toBytes(b))))
  }

  def drop(id: ModifierId): Unit = {
    storage.update(
      ByteArrayWrapper(Blake2b256(id ++ "drop".getBytes)),
      Seq(ByteArrayWrapper(id), headerScoreKey(id), headerDiffKey(id)),
      Seq())
  }

  private def indexes(mod: HistoryModifier, requiredDifficulty: BigInt): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    mod match {
      case h: Header => headerIndexes(h, requiredDifficulty)
      case _ =>
        Seq()
    }
  }


}
