package org.ergoplatform.nodeView

import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.wallet.protocol.context.TransactionContext
import org.ergoplatform.{ErgoLikeContext, ErgoLikeTransactionTemplate, UnsignedInput}
import sigmastate.interpreter.ContextExtension
import org.ergoplatform.validation.{SigmaValidationSettings, ValidationRules}


class ErgoContext(val stateContext: ErgoStateContext,
                  transactionContext: TransactionContext,
                  override val extension: ContextExtension = ContextExtension(Map()),
                  override val validationSettings: SigmaValidationSettings = ValidationRules.currentSettings)
  extends ErgoLikeContext(stateContext.currentHeight,
    ErgoInterpreter.avlTreeFromDigest(stateContext.previousStateDigest),
    stateContext.lastBlockMinerPk,
    stateContext.sigmaLastHeaders,
    stateContext.sigmaPreHeader,
    transactionContext.dataBoxes,
    transactionContext.boxesToSpend,
    transactionContext.spendingTransaction,
    transactionContext.self,
    extension,
    validationSettings
  ) {

  override def withExtension(newExtension: ContextExtension): ErgoContext =
    new ErgoContext(stateContext, transactionContext, newExtension)

  override def withTransaction(newSpendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput]): ErgoContext =
    new ErgoContext(stateContext, transactionContext.copy(spendingTransaction = newSpendingTransaction), extension)
}
