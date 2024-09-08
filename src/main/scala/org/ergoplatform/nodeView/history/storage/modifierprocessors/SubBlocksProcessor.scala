package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.subblocks.SubBlockInfo
import scorex.util.ModifierId

trait SubBlocksProcessor {

  val subBlockRecords = Map[ModifierId, SubBlockInfo]()
  val subBlockTransactions = Map[ModifierId, Seq[ErgoTransaction]]()

  // sub-blocks related logic
  def applySubBlockHeader(sbi: SubBlockInfo): Unit = {
    // todo: implement
  }

  def applySubBlockTransactions(sbId: ModifierId, transactions: Seq[ErgoTransaction]): Unit = {
    // todo: implement
  }

  def getSubBlockTransactions(sbId: ModifierId): Option[Seq[ErgoTransaction]] = {
    subBlockTransactions.get(sbId)
  }

}
