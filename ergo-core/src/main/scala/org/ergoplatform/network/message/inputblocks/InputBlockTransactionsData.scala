package org.ergoplatform.network.message.inputblocks

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, ErgoTransactionSerializer}
import org.ergoplatform.serialization.ErgoSerializer
import org.ergoplatform.settings.Constants
import scorex.util.{ModifierId, bytesToId, idToBytes}
import scorex.util.serialization.{Reader, Writer}
import scorex.util.Extensions._
import spire.syntax.all.cfor

/**
  * Data carrier for input block transactions in P2P messaging.
  */
case class InputBlockTransactionsData(inputBlockId: ModifierId,
                                      transactions: Seq[ErgoTransaction],
                                      sizeOpt: Option[Int] = None)
