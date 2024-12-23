package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.subblocks.InputBlockInfo
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.collection.mutable

/**
  * Storing and processing input-blocks related data
  * Desiderata:
  *   * store input blocks for short time only
  */
trait InputBlocksProcessor extends ScorexLogging {

  /**
    * Pointer to a best input-block known
    */
  var _bestInputBlock: Option[InputBlockInfo] = None

  // input block id -> input block index
  val inputBlockRecords = mutable.Map[ModifierId, InputBlockInfo]()

  // input block id -> input block transactions index
  val inputBlockTransactions = mutable.Map[ModifierId, Seq[ErgoTransaction]]()

  // reset sub-blocks structures, should be called on receiving ordering block (or slightly later?)
  def resetState() = {
    _bestInputBlock = None

    // todo: subBlockRecords & subBlockTransactions should be cleared a bit later, as other peers may still ask for them
    inputBlockRecords.clear()
    inputBlockTransactions.clear()
  }

  // sub-blocks related logic
  def applyInputBlock(sbi: InputBlockInfo): Unit = {
    // new ordering block arrived ( should be processed outside ? )
    if (sbi.header.height > _bestInputBlock.map(_.header.height).getOrElse(-1)) {
      resetState()
    }

    inputBlockRecords.put(sbi.header.id, sbi)

    // todo: currently only one chain of subblocks considered,
    // todo: in fact there could be multiple trees here (one subblocks tree per header)
    _bestInputBlock match {
      case None => _bestInputBlock = Some(sbi)
      case Some(maybeParent) if (sbi.prevInputBlockId.map(bytesToId).contains(maybeParent.header.id)) =>
        _bestInputBlock = Some(sbi)
      case _ =>
        // todo: record it
        log.debug(s"Applying non-best inpu block #: ${sbi.header.id}")
    }
  }

  def applySubBlockTransactions(sbId: ModifierId, transactions: Seq[ErgoTransaction]): Unit = {
    inputBlockTransactions.put(sbId, transactions)
  }

  def getSubBlockTransactions(sbId: ModifierId): Option[Seq[ErgoTransaction]] = {
    inputBlockTransactions.get(sbId)
  }

  def bestSubblock(): Option[InputBlockInfo] = {
    _bestInputBlock
  }

}
