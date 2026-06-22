package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.util.ModifierId

case class InputBlockTransactionsRequest(inputBlockId: ModifierId, txIds: Seq[ErgoTransaction.WeakId])
