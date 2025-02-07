package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.ErgoLikeContext.Height
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.subblocks.InputBlockInfo
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.annotation.tailrec
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

  // input block id -> input block
  val inputBlockRecords = mutable.Map[ModifierId, InputBlockInfo]()

  // input block id -> parent input block id (or None if parent is ordering block, and height from ordering block
  val inputBlockParents = mutable.Map[ModifierId, (Option[ModifierId], Int)]()

  // input block id -> input block transaction ids
  val inputBlockTransactions = mutable.Map[ModifierId, Seq[ModifierId]]()

  // txid -> transaction
  val transactionsCache = mutable.Map[ModifierId, ErgoTransaction]()

  // transactions generated AFTER an ordering block
  // block header (ordering block) -> transaction ids
  // so transaction ids do belong to transactions in input blocks since the block (header)
  val orderingBlockTransactions = mutable.Map[ModifierId, Seq[ModifierId]]()

  val waitingForInputBlocks = mutable.Set[ModifierId]()

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

  private def prune(): Unit = {
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
      log.info(s"Pruning input block # $id") // todo: switch to .debug
      inputBlockRecords.remove(id).foreach { ibi =>
        ibi.prevInputBlockId.foreach { parentId =>
          waitingForInputBlocks.remove(bytesToId(parentId))
        }
      }
      inputBlockTransactions.remove(id)
      inputBlockParents.remove(id)
    }

  }

  // reset sub-blocks structures, should be called on receiving ordering block (or slightly later?)
  private def resetState(doPruning: Boolean) = {
    _bestInputBlock = None
    if (doPruning) {
      prune()
    }
  }

  /**
    * Update input block related structures with a new input block got from a local miner or p2p network
    * @return true if provided input block is a new best input block,
    *         and also optionally id of another input block to download
    */
  def applyInputBlock(ib: InputBlockInfo): (Boolean, Option[ModifierId])= {

    // new ordering block arrived ( should be processed outside ? )
    if (ib.header.height > _bestInputBlock.map(_.header.height).getOrElse(-1)) {
      resetState(false)
    }

    inputBlockRecords.put(ib.header.id, ib)

    val ibParentOpt = ib.prevInputBlockId.map(bytesToId)

    ibParentOpt.flatMap(parentId => inputBlockParents.get(parentId)) match {
      case Some((_, parentDepth)) =>
        val selfDepth = parentDepth + 1
        inputBlockParents.put(ib.id, ibParentOpt -> selfDepth)

        if (waitingForInputBlocks.contains(ib.id)) {
          // todo: fix children's depth, check if the chain is connected ?
          return (false, None)
        }
      case None if ibParentOpt.isDefined => // parent exists but not known yet, download it
        waitingForInputBlocks.add(ibParentOpt.get)
        return (false, ibParentOpt)

      case _ =>
    }


    // todo: currently only one chain of subblocks considered,
    // todo: in fact there could be multiple trees here (one subblocks tree per header)
    // todo: split best input header / block
    _bestInputBlock match {
      case None =>
        if (ib.header.parentId == historyReader.bestHeaderOpt.map(_.id).getOrElse("")) {
          log.info(s"Applying best input block #: ${ib.header.id}, no parent")
          _bestInputBlock = Some(ib)
          (true, None)
        } else {
          (false, None)
        }
      case Some(maybeParent) if (ibParentOpt.contains(maybeParent.id)) =>
        log.info(s"Applying best input block #: ${ib.id} @ height ${ib.header.height}, header is ${ib.header.id}, parent is ${maybeParent.id}")
        _bestInputBlock = Some(ib)
        (true, None)
      case _ =>
        ibParentOpt match {
          case Some(ibParent) =>
            // child of forked input block
            log.info(s"Applying forked input block #: ${ib.header.id}, with parent $ibParent")
            // todo: forks switching etc
            (false, None)
          case None =>
            // first input block since ordering block but another best block exists
            log.info(s"Applying forked input block #: ${ib.header.id}, with no parent")
            (false, None)
        }
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

  // todo: call on best header change
  def updateStateWithOrderingBlock(h: Header): Unit = {
    if (h.height >= _bestInputBlock.map(_.header.height).getOrElse(0)) {
      resetState(true)
    }
  }

  /**
    * @return best known inputs-block chain for the current best-known ordering block
    */
  def bestInputBlocksChain(): Seq[ModifierId] = {
    bestInputBlock() match {
      case Some(tip) =>
        @tailrec
        def stepBack(acc: Seq[ModifierId], inputId: ModifierId): Seq[ModifierId] = {
          inputBlockParents.get(inputId) match {
            case Some((Some(parentId), _)) => stepBack(acc :+ parentId, parentId)
            case _ => acc
          }
        }
        stepBack(Seq.empty, tip.id)
      case None => Seq.empty
    }
  }

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
