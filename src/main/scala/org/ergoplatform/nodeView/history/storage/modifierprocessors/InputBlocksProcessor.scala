package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.ErgoLikeContext.Height
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

  private def bestInputBlockHeight: Option[Height] = _bestInputBlock.map(_.header.height)

  private def prune() = {
    val BlocksThreshold = 2 // we remove input-blocks data after 2 ordering blocks

    val bestHeight = bestInputBlockHeight.getOrElse(0)
    val idsToRemove = inputBlockRecords.flatMap{case (id, ibi) =>
      val res = (bestHeight - ibi.header.height) > BlocksThreshold
      if(res){
        Some(id)
      } else {
        None
      }
    }
    idsToRemove.foreach{ id =>
      inputBlockRecords.remove(id)
      inputBlockTransactions.remove(id)
    }
  }

  // reset sub-blocks structures, should be called on receiving ordering block (or slightly later?)
  private def resetState() = {
    _bestInputBlock = None
    prune()
  }

  // sub-blocks related logic
  def applyInputBlock(ib: InputBlockInfo): Unit = {
    // new ordering block arrived ( should be processed outside ? )
    if (ib.header.height > _bestInputBlock.map(_.header.height).getOrElse(-1)) {
      resetState()
    }

    inputBlockRecords.put(ib.header.id, ib)

    // todo: currently only one chain of subblocks considered,
    // todo: in fact there could be multiple trees here (one subblocks tree per header)
    // todo: split best input header / block
    _bestInputBlock match {
      case None =>
        log.debug(s"Applying best input block #: ${ib.header.id}, no parent")
        _bestInputBlock = Some(ib)
      case Some(maybeParent) if (ib.prevInputBlockId.map(bytesToId).contains(maybeParent.header.id)) =>
        log.debug(s"Applying best input block #: ${ib.header.id}, parent is $maybeParent")
        _bestInputBlock = Some(ib)
      case _ =>
        // todo: record it
        log.debug(s"Applying non-best input block #: ${ib.header.id}")
    }
  }

  def applyInputBlockTransactions(sbId: ModifierId, transactions: Seq[ErgoTransaction]): Unit = {
    inputBlockTransactions.put(sbId, transactions)
  }

  def getInputBlockTransactions(sbId: ModifierId): Option[Seq[ErgoTransaction]] = {
    inputBlockTransactions.get(sbId)
  }

  def bestInputBlock(): Option[InputBlockInfo] = {
    _bestInputBlock
  }

}
