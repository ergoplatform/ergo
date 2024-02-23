package org.ergoplatform.modifiers

import org.ergoplatform.modifiers.mempool.ErgoTransaction

/**
  * Block section which contains transactions
  */
trait TransactionsCarryingBlockSection extends BlockSection {

  def transactions: Seq[ErgoTransaction]

}
