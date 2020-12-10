package org.ergoplatform.wallet.protocol.context

import org.ergoplatform.{ErgoBox, ErgoLikeTransactionTemplate, UnsignedInput}

/**
  * Part of the execution context in regards with spending transaction
  *
  * @param boxesToSpend - inputs of the transaction
  * @param dataBoxes - data (read-only) inputs of the transaction
  * @param spendingTransaction - spending transaction
  */

// TODO: it seems spendingTransaction is needed only to get output candidates in ErgoLikeContext.
// After ErgoLikeContext refactoring in sigma, this class can be simplified.
case class TransactionContext(boxesToSpend: IndexedSeq[ErgoBox],
                              dataBoxes: IndexedSeq[ErgoBox],
                              spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput])
