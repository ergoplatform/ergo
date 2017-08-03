package org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions

import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.BlockTransactions
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

/**
  * BlockTransactions processor for regimes, that do not keep BlockTransactions
  */
trait EmptyBlockTransactionsProcessor extends BlockTransactionsProcessor {

  def toDrop(modifier: BlockTransactions): Seq[ByteArrayWrapper] = Seq()

  override def validate(m: BlockTransactions): Try[Unit] =
    Failure(new Error("Regime that do not process BlockTransactions"))

  override def process(m: BlockTransactions): ProgressInfo[ErgoPersistentModifier] = ProgressInfo(None, Seq(), Seq())
}

