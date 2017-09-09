package org.ergoplatform.nodeView.history.storage.modifierprocessors.blocktransactions

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.BlockTransactions
import scorex.core.consensus.History.ProgressInfo

import scala.util.{Failure, Try}

/**
  * BlockTransactions processor for regimes, that do not keep BlockTransactions
  */
trait EmptyBlockTransactionsProcessor extends BlockTransactionsProcessor {

  override protected def validate(m: BlockTransactions): Try[Unit] =
    Failure(new Error("Regime that do not process BlockTransactions"))

  override protected def process(m: BlockTransactions): ProgressInfo[ErgoPersistentModifier] =
    ProgressInfo(None, Seq(), Seq(), Seq())
}

