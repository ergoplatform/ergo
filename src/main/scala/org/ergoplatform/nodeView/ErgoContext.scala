package org.ergoplatform.nodeView

import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.wallet.protocol.context.InputContext
import org.ergoplatform.ErgoLikeContext
import org.ergoplatform.sdk.wallet.protocol.context.TransactionContext
import scorex.crypto.authds.ADDigest

/**
  * Context to be used during transaction verification
  */
class ErgoContext(val stateContext: ErgoStateContext,
                  transactionContext: TransactionContext,
                  inputContext: InputContext,
                  override val costLimit: Long,
                  override val initCost: Long)
  extends ErgoLikeContext(ErgoInterpreter.avlTreeFromDigest(ADDigest @@ stateContext.previousStateDigest.toArray),
    stateContext.sigmaLastHeaders,
    stateContext.sigmaPreHeader,
    transactionContext.dataBoxes,
    transactionContext.boxesToSpend,
    transactionContext.spendingTransaction,
    inputContext.selfIndex,
    inputContext.extension,
    stateContext.validationSettings.sigmaSettings,
    costLimit,
    initCost,
    activatedScriptVersion = (stateContext.blockVersion - 1).toByte // block version N of ErgoProtocol corresponds to version N-1 of ErgoTree (aka script version)
  )
