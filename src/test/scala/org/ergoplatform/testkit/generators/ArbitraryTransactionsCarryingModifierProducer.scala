package org.ergoplatform.testkit.generators

import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPool
/**
  * Produces a modifier with transactions, not necessary syntatically or semantically valid
   */
trait ArbitraryTransactionsCarryingModifierProducer{

def modifierWithTransactions(memoryPoolOpt: Option[ErgoMemPool], customTransactionsOpt: Option[Seq[ErgoTransaction]]): BlockTransactions
}
