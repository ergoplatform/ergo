package org.ergoplatform.testkit.generators

import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.state.ErgoState

trait SemanticallyValidTransactionsCarryingModifier[ST <: ErgoState[ST]] {

  def semanticallyValidModifier(state: ST): BlockTransactions
  def genValidTransactionPair(state: ST): Seq[ErgoTransaction]
  def semanticallyValidModifierWithCustomTransactions(state: ST, transactions: Seq[ErgoTransaction]): BlockTransactions
}
