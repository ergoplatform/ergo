package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.util.ModifierId

case class InputBlockTransactionIdsData (inputBlockId: ModifierId,
                                         transactionIds: Seq[ErgoTransaction.WeakId]) {

}
