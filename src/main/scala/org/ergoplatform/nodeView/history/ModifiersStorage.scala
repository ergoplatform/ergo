package org.ergoplatform.nodeView.history

import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.modifiers.history.{HistoryModifier, HistoryModifierSerializer}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.utils.ScorexLogging

import scala.util.{Failure, Success}

class ModifiersStorage(storage: LSMStore, genesisId: ModifierId) extends ScorexLogging {

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
    storage.update(
      ByteArrayWrapper(b.id),
      Seq(),
      Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(HistoryModifierSerializer.toBytes(b))))
  }

}
