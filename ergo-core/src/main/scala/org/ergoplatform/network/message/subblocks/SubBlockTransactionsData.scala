package org.ergoplatform.network.message.subblocks

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.util.ModifierId

// todo: send transactions or transactions id ?
case class SubBlockTransactionsData(subblockID: ModifierId, transactions: Seq[ErgoTransaction]){

}
