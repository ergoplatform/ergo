package org.ergoplatform.nodeView.history.storage.modifierprocessors

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.BlockTransactions

import scala.util.{Failure, Try}

/**
  * BlockTransactions processor for regimes, that do not keep BlockTransactions
  */
trait EmptyBlockTransactionsProcessor extends BlockTransactionsProcessor {
  def toInsert(m: BlockTransactions, env: ModifierProcessorEnvironment): Seq[(ByteArrayWrapper, ByteArrayWrapper)] = Seq()

  def toDrop(modifier: BlockTransactions): Seq[ByteArrayWrapper] = Seq()

  override def validate(m: BlockTransactions): Try[Unit] =
    Failure(new Error("Regime that do not process BlockTransactions"))

}

