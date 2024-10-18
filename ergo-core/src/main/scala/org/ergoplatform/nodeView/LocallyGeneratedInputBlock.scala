package org.ergoplatform.nodeView

import org.ergoplatform.network.message.subblocks.SubBlockTransactionsData
import org.ergoplatform.subblocks.SubBlockInfo

case class LocallyGeneratedInputBlock(sbi: SubBlockInfo, sbt: SubBlockTransactionsData)
