package org.ergoplatform.nodeView

import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.settings.Constants
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransactionTemplate, UnsignedInput}
import scorex.crypto.authds.ADDigest
import sigmastate.AvlTreeData
import sigmastate.interpreter.ContextExtension

case class TransactionContext(boxesToSpend: IndexedSeq[ErgoBox],
                              spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
                              selfIndex: Short) {
  lazy val self = boxesToSpend(selfIndex)
}

class ErgoContext(val stateContext: ErgoStateContext,
                  transactionContext: TransactionContext,
                  override val metadata: Metadata,
                  override val extension: ContextExtension = ContextExtension(Map()))
  extends ErgoLikeContext(stateContext.currentHeight,
    ErgoContext.stateTreeFromDigest(stateContext.previousStateDigest),
    stateContext.lastBlockMinerPk,
    transactionContext.boxesToSpend,
    transactionContext.spendingTransaction,
    transactionContext.self, extension) {

  override def withExtension(newExtension: ContextExtension): ErgoContext =
    new ErgoContext(stateContext, transactionContext, metadata, newExtension)

  override def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoContext =
    new ErgoContext(stateContext, transactionContext, metadata, extension)
}

object ErgoContext {
  def stateTreeFromDigest(digest: ADDigest): AvlTreeData = AvlTreeData(digest, Constants.HashLength)
}
