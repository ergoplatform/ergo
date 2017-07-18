package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.BlockTransactions

/**
  * BlockTransactions processor for regimes, that do not keep BlockTransactions
  */
trait EmptyBlockTransactionsProcessor {
  def toInsert(m: BlockTransactions, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = Seq()

  def toDrop(modifier: BlockTransactions): Seq[ByteArrayWrapper] = Seq()

}

