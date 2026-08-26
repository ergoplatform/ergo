package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction

case class LocallyGeneratedOrderingBlock(efb: ErgoFullBlock, orderingBlockTransactions: Seq[ErgoTransaction])
