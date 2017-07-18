package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.nodeView.history.storage.HistoryStorage

import scala.util.Try

/**
  * BlockTransactions processor for fullnode regime
  */
trait FullnodeBlockTransactionsProcessor extends BlockTransactionsProcessor {
  protected val historyStorage: HistoryStorage

  override def toInsert(m: BlockTransactions, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = ???

  override def toDrop(modifier: BlockTransactions): Seq[ByteArrayWrapper] = ???

  override def validate(m: BlockTransactions): Try[Unit] = Try {
    require(historyStorage.contains(m.headerId), s"Header for modifier $m is no defined")
    require(!historyStorage.contains(m.id), s"Modifier $m is already in history")
  }
}
