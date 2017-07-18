package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.BlockTransactions

import scala.util.Try

trait BlockTransactionsProcessor {
  def toInsert(m: BlockTransactions, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)]

  def toDrop(modifier: BlockTransactions): Seq[ByteArrayWrapper]

  def validate(m: BlockTransactions):Try[Unit]
}

