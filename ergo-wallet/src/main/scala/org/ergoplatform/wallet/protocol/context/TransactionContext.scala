package org.ergoplatform.wallet.protocol.context

import org.ergoplatform.{ErgoBox, ErgoLikeTransactionTemplate, UnsignedInput}
import sigmastate.interpreter.ContextExtension

final case class TransactionContext(boxesToSpend: IndexedSeq[ErgoBox],
                                    dataBoxes: IndexedSeq[ErgoBox],
                                    spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput])

final case class InputContext(selfIndex: Short, extension: ContextExtension)
