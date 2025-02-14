package org.ergoplatform.nodeView

import org.ergoplatform.network.message.inputblocks.InputBlockTransactionsData
import org.ergoplatform.subblocks.InputBlockInfo

case class LocallyGeneratedInputBlock(sbi: InputBlockInfo, sbt: InputBlockTransactionsData)
