package org.ergoplatform.nodeView

import org.ergoplatform.network.message.inputblocks.InputBlockTransactionsData
import org.ergoplatform.subblocks.InputBlockAnnouncement

case class LocallyGeneratedInputBlock(sbi: InputBlockAnnouncement, sbt: InputBlockTransactionsData)
