package org.ergoplatform.nodeView.history.storage

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.BlockTransactions

trait BlockTransactionsProcessor {
  def toInsert(m: BlockTransactions, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)]

  def toDrop(modifier: BlockTransactions): Seq[ByteArrayWrapper]

}

