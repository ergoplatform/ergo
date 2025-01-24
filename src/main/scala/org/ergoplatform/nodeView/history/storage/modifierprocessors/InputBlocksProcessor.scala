package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.subblocks.InputBlockInfo
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.collection.mutable

/**
  * Storing and processing input-blocks related data
  * Desiderata:
  * * store input blocks for short time only
  */
trait InputBlocksProcessor extends ScorexLogging {

  /**
    * @return interface to read objects from history database
    */
  def historyReader: ErgoHistoryReader

  /**
    * Pointer to a best input-block known
    */
  var _bestInputBlock: Option[InputBlockInfo] = None

  // input block id -> input block index
  val inputBlockRecords = mutable.Map[ModifierId, InputBlockInfo]()

  // input block id -> input block transaction ids index
  val inputBlockTransactions = mutable.Map[ModifierId, Seq[ModifierId]]()

  // txid -> transaction
  val transactionsCache = mutable.Map[ModifierId, ErgoTransaction]()

  // transactions generated AFTER an ordering block
  // block header (ordering block) -> transaction ids
  // so transaction ids do belong to transactions in input blocks since the block (header)
  val orderingBlockTransactions = mutable.Map[ModifierId, Seq[ModifierId]]()

  /**
    * @return best ordering and input blocks
    */
  def bestBlocks: (Option[Header], Option[InputBlockInfo]) = {
    val bestOrdering = historyReader.bestFullBlockOpt.map(_.header)
    val bestInputForOrdering = if (_bestInputBlock.exists(sbi => bestOrdering.map(_.id).contains(sbi.header.parentId))) {
      _bestInputBlock
    } else {
      None
    }
    bestOrdering -> bestInputForOrdering
  }

  private def bestInputBlockHeight: Option[Height] = _bestInputBlock.map(_.header.height)

  private def prune() = {
    val BlocksThreshold = 2 // we remove input-blocks data after 2 ordering blocks

    val bestHeight = bestInputBlockHeight.getOrElse(0)
    val idsToRemove = inputBlockRecords.flatMap { case (id, ibi) =>
      val res = (bestHeight - ibi.header.height) > BlocksThreshold
      if (res) {
        Some(id)
      } else {
        None
      }
    }
    idsToRemove.foreach { id =>
      log.info(s"Pruning input block # $id") // todo: .debug
      inputBlockRecords.remove(id)
      inputBlockTransactions.remove(id)
    }
  }

  // reset sub-blocks structures, should be called on receiving ordering block (or slightly later?)
  private def resetState() = {
    _bestInputBlock = None
    prune()
  }

  /**
    * Update input block related structures with a new input block got from a local miner or p2p network
    * @return true if provided input block is a new best input block
    */
  def applyInputBlock(ib: InputBlockInfo): Boolean = {
    // new ordering block arrived ( should be processed outside ? )
    if (ib.header.height > _bestInputBlock.map(_.header.height).getOrElse(-1)) {
      resetState()
    }

    inputBlockRecords.put(ib.header.id, ib)

    val ibParent = ib.prevInputBlockId.map(bytesToId)

    // todo: currently only one chain of subblocks considered,
    // todo: in fact there could be multiple trees here (one subblocks tree per header)
    // todo: split best input header / block
    _bestInputBlock match {
      case None =>
        log.info(s"Applying best input block #: ${ib.header.id}, no parent")
        _bestInputBlock = Some(ib)
        true
      case Some(maybeParent) if (ibParent.contains(maybeParent.id)) =>
        log.info(s"Applying best input block #: ${ib.id} @ height ${ib.header.height}, header is ${ib.header.id}, parent is ${maybeParent.id}")
        _bestInputBlock = Some(ib)
        true
      case _ =>
        // todo: switch from one input block chain to another
        log.info(s"Applying non-best input block #: ${ib.header.id}, parent #: $ibParent")
        false
    }
  }

  def applyInputBlockTransactions(sbId: ModifierId, transactions: Seq[ErgoTransaction]): Unit = {
    log.info(s"Applying input block transactions for ${sbId} , transactions: ${transactions.size}")
    val transactionIds = transactions.map(_.id)
    inputBlockTransactions.put(sbId, transactionIds)
    if (sbId == _bestInputBlock.map(_.id).getOrElse("")) {
      val orderingBlockId = _bestInputBlock.get.header.id
      val curr = orderingBlockTransactions.getOrElse(orderingBlockId, Seq.empty)
      orderingBlockTransactions.put(orderingBlockId, curr ++ transactionIds)
    }
    transactions.foreach { tx =>
      transactionsCache.put(tx.id, tx)
    }
  }

  // Getters to serve client requests below

  def getInputBlock(sbId: ModifierId): Option[InputBlockInfo] = {
    inputBlockRecords.get(sbId)
  }

  def getInputBlockTransactions(sbId: ModifierId): Option[Seq[ErgoTransaction]] = {
    // todo: cache input block transactions to avoid recalculating it on every p2p request
    // todo: optimize the code below
    inputBlockTransactions.get(sbId).map { ids =>
      ids.flatMap(transactionsCache.get)
    }
  }

  /**
    * @param id ordering block (header) id
    * @return transactions included in best input blocks chain since ordering block with identifier `id`
    */
  def getOrderingBlockTransactions(id: ModifierId): Option[Seq[ErgoTransaction]] = {
    // todo: cache input block transactions to avoid recalculating it on every input block regeneration?
    // todo: optimize the code below
    orderingBlockTransactions.get(id).map { ids =>
      ids.flatMap(transactionsCache.get)
    }
  }

  def bestInputBlock(): Option[InputBlockInfo] = {
    _bestInputBlock.flatMap { bib =>
      // todo: check header id? best input block can be child of non-best ordering header
      if (bib.header.height == historyReader.headersHeight + 1) {
        Some(bib)
      } else {
        None
      }
    }
  }

}
