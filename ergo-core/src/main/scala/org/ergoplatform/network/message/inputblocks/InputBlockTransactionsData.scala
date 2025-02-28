package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.util.ModifierId

// todo: send transactions or transactions id ?
case class InputBlockTransactionsData(inputBlockId: ModifierId, transactions: Seq[ErgoTransaction]){

}
