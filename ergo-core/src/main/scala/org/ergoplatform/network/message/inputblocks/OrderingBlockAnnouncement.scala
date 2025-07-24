package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import scorex.util.ModifierId

/**
  * Ordering block announcement data
  * @param header - ordering block header
  * @param nonBroadcastedTransactions - transactions which were not broadcasted by miner (like emission and fee but could be arb)
  * @param broadcastedTransactionIds - ids of ordering block transactions which were broadcasted previously
  * @param extensionFields - all the extension block section values
  */
case class OrderingBlockAnnouncement(header: Header,
                                     nonBroadcastedTransactions: Seq[ErgoTransaction],
                                     broadcastedTransactionIds: Seq[ModifierId],
                                     extensionFields: Seq[(Array[Byte], Array[Byte])])
