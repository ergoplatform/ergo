package scorex.testkit.generators

import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.core.transaction.state.MinimalState

trait SemanticallyValidTransactionsCarryingModifier[ST <: MinimalState[ST]] {

  def semanticallyValidModifier(state: ST): BlockTransactions
  def genValidTransactionPair(state: ST): Seq[ErgoTransaction]
  def semanticallyValidModifierWithCustomTransactions(state: ST, transactions: Seq[ErgoTransaction]): BlockTransactions
}
