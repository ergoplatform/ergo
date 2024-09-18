package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.subblocks.SubBlockInfo
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.collection.mutable

trait SubBlocksProcessor extends ScorexLogging {

  var _bestSubblock: Option[SubBlockInfo] = None
  val subBlockRecords = mutable.Map[ModifierId, SubBlockInfo]()
  val subBlockTransactions = mutable.Map[ModifierId, Seq[ErgoTransaction]]()

  // sub-blocks related logic
  def applySubBlockHeader(sbi: SubBlockInfo): Unit = {
    subBlockRecords.put(sbi.subBlock.id, sbi)

    // todo: currently only one chain of subblocks considered,
    // in fact there could be multiple trees here (one subblocks tree per header)
    _bestSubblock match {
      case None => _bestSubblock = Some(sbi)
      case Some(maybeParent) if (sbi.prevSubBlockId.map(bytesToId).contains(maybeParent.subBlock.id)) =>
        _bestSubblock = Some(sbi)
      case _ =>
        // todo: record it
        log.debug(s"Applying non-best subblock id: ${sbi.subBlock.id}")
    }
  }

  def applySubBlockTransactions(sbId: ModifierId, transactions: Seq[ErgoTransaction]): Unit = {
    subBlockTransactions.put(sbId, transactions)
  }

  def getSubBlockTransactions(sbId: ModifierId): Option[Seq[ErgoTransaction]] = {
    subBlockTransactions.get(sbId)
  }

  def bestSubblock(): Option[SubBlockInfo] = {
    _bestSubblock
  }

}
