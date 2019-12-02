package org.ergoplatform.wallet.protocol.context

import org.ergoplatform.{ErgoBox, ErgoLikeTransactionTemplate, UnsignedInput}

final case class TransactionContext(boxesToSpend: IndexedSeq[ErgoBox],
                                    dataBoxes: IndexedSeq[ErgoBox],
                                    spendingTransaction: ErgoLikeTransactionTemplate[_ <: UnsignedInput],
                                    selfIndex: Short) {

  val self: ErgoBox = boxesToSpend(selfIndex)
}
