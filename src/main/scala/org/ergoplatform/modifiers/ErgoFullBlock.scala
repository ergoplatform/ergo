package org.ergoplatform.modifiers

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}

case class ErgoFullBlock(header: Header, blockTransactions: BlockTransactions, aDProofs: ADProofs)
