package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.ADProofs
import scorex.crypto.encode.Base58

import scala.util.Try

trait ADProofsProcessor {
  protected val historyStorage: HistoryStorage

  def toInsert(m: ADProofs, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)]

  def toDrop(modifier: ADProofs): Seq[ByteArrayWrapper]

  def validate(m: ADProofs): Try[Unit] = Try {
    require(historyStorage.contains(m.headerId), s"Header for modifier $m is no defined")
    require(!historyStorage.contains(m.id), s"Modifier $m is already in history")
  }
}

