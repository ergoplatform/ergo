package org.ergoplatform.nodeView

import org.ergoplatform.ErgoLikeContext
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.sdk.wallet.protocol.context.TransactionContext
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.wallet.protocol.context.InputContext

/**
  * Context to be used during transaction verification
  */
class ErgoContext(val stateContext: ErgoStateContext,
                  transactionContext: TransactionContext,
                  inputContext: InputContext,
                  override val costLimit: Long,
                  override val initCost: Long,
                  override val softFieldsAllowed: Boolean)
  extends ErgoLikeContext(ErgoInterpreter.avlTreeFromDigest(stateContext.previousStateDigest),
    stateContext.sigmaLastHeaders,
    stateContext.sigmaPreHeader,
    transactionContext.dataBoxes,
    transactionContext.boxesToSpend,
    transactionContext.spendingTransaction,
    inputContext.selfIndex.toInt,
    inputContext.extension,
    stateContext.validationSettings.sigmaSettings,
    costLimit,
    initCost,
    activatedScriptVersion = (stateContext.blockVersion - 1).toByte, // block version N of ErgoProtocol corresponds to version N-1 of ErgoTree (aka script version)
    softFieldsAllowed
  )
