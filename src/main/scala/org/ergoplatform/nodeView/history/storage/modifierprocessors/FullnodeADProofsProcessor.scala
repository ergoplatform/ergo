package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.nodeView.history.storage.HistoryStorage

import scala.util.Try

/**
  * ADProof processor for fullnode regime
  */
trait FullnodeADProofsProcessor extends ADProofsProcessor {
  protected val historyStorage: HistoryStorage

  override def toInsert(m: ADProofs, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = ???

  override def toDrop(modifier: ADProofs): Seq[ByteArrayWrapper] = ???

  override def validate(m: ADProofs): Try[Unit] = Try {
    require(historyStorage.contains(m.headerId), s"Header for modifier $m is no defined")
    require(!historyStorage.contains(m.id), s"Modifier $m is already in history")
  }
}
