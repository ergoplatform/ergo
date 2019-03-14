package org.ergoplatform.nodeView

import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.settings.Constants
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransactionTemplate, UnsignedInput}
import scorex.crypto.authds.ADDigest
import sigmastate.interpreter.ContextExtension
import sigmastate.{AvlTreeData, AvlTreeFlags}

case class TransactionContext(boxesToSpend: IndexedSeq[ErgoBox],
                              dataBoxes: IndexedSeq[ErgoBox],
                              spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
                              selfIndex: Short) {
  lazy val self = boxesToSpend(selfIndex)
}

class ErgoContext(val stateContext: ErgoStateContext,
                  transactionContext: TransactionContext,
                  override val extension: ContextExtension = ContextExtension(Map()))
  extends ErgoLikeContext(stateContext.currentHeight,
    ErgoContext.stateTreeFromDigest(stateContext.previousStateDigest),
    stateContext.lastBlockMinerPk,
    stateContext.sigmaLastHeaders,
    stateContext.sigmaPreHeader,
    transactionContext.dataBoxes,
    transactionContext.boxesToSpend,
    transactionContext.spendingTransaction,
    transactionContext.self,
    extension
  ) {

  override def withExtension(newExtension: ContextExtension): ErgoContext =
    new ErgoContext(stateContext, transactionContext, newExtension)

  override def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoContext =
    new ErgoContext(stateContext, transactionContext.copy(spendingTransaction = newSpendingTransaction), extension)
}

object ErgoContext {
  def stateTreeFromDigest(digest: ADDigest): AvlTreeData = {
    // todo check flags for correctness
    val flags = AvlTreeFlags(insertAllowed = true, updateAllowed = true, removeAllowed = true)
    AvlTreeData(digest, flags, Constants.HashLength)
  }
}
