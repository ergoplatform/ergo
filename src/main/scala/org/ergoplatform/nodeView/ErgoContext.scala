package org.ergoplatform.nodeView

import org.ergoplatform.ErgoLikeContext.Metadata
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.settings.Constants
import org.ergoplatform.{ErgoBox, ErgoLikeContext, ErgoLikeTransactionTemplate, UnsignedInput}
import scorex.crypto.authds.ADDigest
import sigmastate.AvlTreeData
import sigmastate.interpreter.ContextExtension

class ErgoContext(val stateContext: ErgoStateContext,
                  override val boxesToSpend: IndexedSeq[ErgoBox],
                  override val spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
                  override val self: ErgoBox,
                  override val metadata: Metadata,
                  override val extension: ContextExtension = ContextExtension(Map()))
  extends ErgoLikeContext(stateContext.currentHeight, ErgoContext.stateTreeFromDigest(stateContext.stateDigest),
                          boxesToSpend, spendingTransaction, self, metadata, extension) {

  override def withExtension(newExtension: ContextExtension): ErgoContext =
    new ErgoContext(stateContext, boxesToSpend, spendingTransaction, self, metadata, newExtension)

  override def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoContext =
    new ErgoContext(stateContext, boxesToSpend, newSpendingTransaction, self, metadata, extension)
}

object ErgoContext {
  def stateTreeFromDigest(digest: ADDigest): AvlTreeData = AvlTreeData(digest, Constants.HashLength)
}