package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.history.{HistoryModifier, HistoryModifierSerializer}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Blake2b256

import scala.util.{Failure, Success}

class HistoryStorage(protected val storage: LSMStore,
                     processors: Seq[ModifiersProcessor]) extends ScorexLogging {
  //TODO ensure we have headers processor???
  private val headersProcessor = processors.find(a => a.isInstanceOf[HeadersProcessor]).get.asInstanceOf[HeadersProcessor]
  def bestHeaderId: ModifierId = headersProcessor.bestHeaderId

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
    val env: ModifierProcessorEnvironment = ModifierProcessorEnvironment(requiredDifficulty)
    val indexRows = indexes(b, env)
    storage.update(
      ByteArrayWrapper(b.id),
      Seq(),
      indexRows :+ (ByteArrayWrapper(b.id) -> ByteArrayWrapper(HistoryModifierSerializer.toBytes(b))))
  }

  def drop(id: ModifierId): Unit = {
    val idsToRemove = processors.flatMap(_.idsToDrop(id))
    storage.update(
      ByteArrayWrapper(Blake2b256(id ++ "drop".getBytes)),
      ByteArrayWrapper(id) +: idsToRemove,
      Seq())
  }

  private def indexes(mod: HistoryModifier,
                      env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = {
    processors.flatMap(_.indexes(mod, env))
  }


}
