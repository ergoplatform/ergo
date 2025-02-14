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
  // todo: improve removing, some txs included in forked input blocks may stuck in the cache
  val transactionsCache = mutable.Map[ModifierId, ErgoTransaction]()

  // ordering block id -> best known input block chain tips
  val bestTips = mutable.Map[ModifierId, mutable.Set[ModifierId]]()

  // ordering block id -> best known input block chain height
  val bestHeights = mutable.Map[ModifierId, Int]()

  // transactions generated AFTER an ordering block
  // block header (ordering block) -> transaction ids
  // so transaction ids do belong to transactions in input blocks since the block (header)
  val orderingBlockTransactions = mutable.Map[ModifierId, Seq[ModifierId]]()

  // waiting list for input blocks for which we got children but the parent not delivered yet
  val deliveryWaitlist = mutable.Set[ModifierId]()

  val disconnectedWaitlist = mutable.Set[InputBlockInfo]()

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

    val orderingBlockIdsToRemove = bestHeights.keys.filter { orderingId =>
      bestHeight > historyReader.heightOf(orderingId).getOrElse(0)
    }.toSeq

    orderingBlockIdsToRemove.foreach { id =>
      bestHeights.remove(id)
      bestTips.remove(id)
      orderingBlockTransactions.remove(id).map { ids =>
        ids.foreach { txId =>
          transactionsCache.remove(txId)
        }
      }
    }

    val inputBlockIdsToRemove = inputBlockRecords.flatMap { case (id, ibi) =>
      val res = (bestHeight - ibi.header.height) > BlocksThreshold
      if (res) {
        Some(id)
      } else {
        None
      }
    }

    inputBlockIdsToRemove.foreach { id =>
      log.info(s"Pruning input block # $id") // todo: switch to .debug
      inputBlockRecords.remove(id).foreach { ibi =>
        ibi.prevInputBlockId.foreach { parentId =>
          deliveryWaitlist.remove(bytesToId(parentId))
        }
        disconnectedWaitlist.remove(ibi)
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
  // todo: use PoEM to store only 2-3 best chains and select best one quickly
  def applyInputBlock(ib: InputBlockInfo): Option[ModifierId] = {
    if (ib.header.height > _bestInputBlock.map(_.header.height).getOrElse(-1)) {
      resetState(false)
    }

    inputBlockRecords.put(ib.header.id, ib)

    val orderingId = ib.header.parentId
    def currentBestTips = bestTips.getOrElse(orderingId, mutable.Set.empty)
    def tipHeight = bestHeights.getOrElse(orderingId, 0)
    val ibParentOpt = ib.prevInputBlockId.map(bytesToId)

    def updateBestTipsAndHeight(depth: Int): Unit = {
      if (depth > tipHeight) {
        bestHeights.put(orderingId, depth)
      }
      if (depth >= tipHeight || (currentBestTips.size < 3 && tipHeight >= 4 && depth >= tipHeight - 2)) {
        bestTips.put(orderingId, currentBestTips += ib.id)
      }
    }

    def addChildren(parentId: ModifierId, parentDepth: Int): Unit = {
      val children = disconnectedWaitlist.filter(childIb =>
        childIb.prevInputBlockId.exists(pid => bytesToId(pid) == parentId)
      )
      val childDepth = parentDepth + 1
      updateBestTipsAndHeight(childDepth)
      children.foreach { childIb =>
        inputBlockParents.put(childIb.id, Some(parentId) -> childDepth)
        disconnectedWaitlist.remove(childIb)
        addChildren(childIb.id, childDepth)
      }
    }

    ibParentOpt.flatMap(parentId => inputBlockParents.get(parentId)) match {
      case Some((_, parentDepth)) =>
        val selfDepth = parentDepth + 1
        inputBlockParents.put(ib.id, ibParentOpt -> selfDepth)
        updateBestTipsAndHeight(selfDepth)
        if (deliveryWaitlist.contains(ib.id)) {
          addChildren(ib.id, selfDepth)
        }
        None

      case None if ibParentOpt.isDefined =>
        deliveryWaitlist.add(ibParentOpt.get)
        disconnectedWaitlist.add(ib)
        ibParentOpt

      case None =>
        inputBlockParents.put(ib.id, None -> 1)
        updateBestTipsAndHeight(1)
        None
    }
  }

  /**
    * @return - sequence of new best input blocks
    */
  def applyInputBlockTransactions(sbId: ModifierId,
                                  transactions: Seq[ErgoTransaction]): Seq[ModifierId] = {
    log.info(s"Applying input block transactions for ${sbId} , transactions: ${transactions.size}")
    val transactionIds = transactions.map(_.id)
    inputBlockTransactions.put(sbId, transactionIds)
    // todo: currently only one chain of subblocks considered,
    // todo: in fact there could be multiple trees here (one subblocks tree per header)
    // todo: split best input header / block

    if (!inputBlockRecords.contains(sbId)) {
      log.warn(s"Input block transactions delivered for not known input block $sbId")
      return Seq.empty
    }

    // put transactions into cache shared among all the input blocks,
    // to avoid data duplication in input block related functions
    transactions.foreach { tx =>
      transactionsCache.put(tx.id, tx)
    }

    def processBestInputBlockCandidate(blockId: ModifierId): Seq[ModifierId] = {
      val ib = inputBlockRecords.apply(blockId)
      val ibParentOpt = ib.prevInputBlockId.map(bytesToId)

      val res: Seq[ModifierId] = _bestInputBlock match {
        case None =>
          if (ib.header.parentId == historyReader.bestHeaderOpt.map(_.id).getOrElse("")) {
            log.info(s"Applying best input block #: ${ib.header.id}, no parent")
            _bestInputBlock = Some(ib)
            Seq(blockId)
          } else {
            Seq.empty
          }
        case Some(maybeParent) if (ibParentOpt.contains(maybeParent.id)) =>
          log.info(s"Applying best input block #: ${ib.id} @ height ${ib.header.height}, header is ${ib.header.id}, parent is ${maybeParent.id}")
          _bestInputBlock = Some(ib)
          Seq(blockId)
        case _ =>
          ibParentOpt match {
            case Some(ibParent) =>
              // child of forked input block
              log.info(s"Applying forked input block #: ${ib.header.id}, with parent $ibParent")
              // todo: forks switching etc
              Seq.empty
            case None =>
              // first input block since ordering block but another best block exists
              log.info(s"Applying forked input block #: ${ib.header.id}, with no parent")
              Seq.empty
          }
      }

      if (res.headOption.getOrElse("0") == _bestInputBlock.map(_.id).getOrElse("1")) {
        val orderingBlockId = _bestInputBlock.get.header.id
        val curr = orderingBlockTransactions.getOrElse(orderingBlockId, Seq.empty)
        orderingBlockTransactions.put(orderingBlockId, curr ++ transactionIds)
      }
      res
    }

    processBestInputBlockCandidate(sbId)
  }

  // todo: call on best header change
  def updateStateWithOrderingBlock(h: Header): Unit = {
    if (h.height >= _bestInputBlock.map(_.header.height).getOrElse(0)) {
      resetState(true)
    }
  }

  // Getters to serve client requests below

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

  /**
    * Checks if input block with id `child` has ancestor with id `parent` in its chain
    *
    * @param child id of potential descendant input block
    * @param parent id of potential ancestor input block
    * @return true if `parent` is ancestor of `child`, false otherwise
    */
  def isAncestor(child: ModifierId, parent: ModifierId): Boolean = {
    @tailrec
    def loop(current: ModifierId): Boolean = {
      if (current == parent) {
        true
      } else {
        inputBlockParents.get(current) match {
          case Some((Some(parentId), _)) => loop(parentId)
          case _ => false
        }
      }
    }
    
    if (child == parent) false else loop(child)
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
    * @return tips (leaf input blocks) for the ordering block with identifier `id`
    */
  def getOrderingBlockTips(id: ModifierId): Option[Set[ModifierId]] = {
    bestTips.get(id).map(_.toSet)
  }
  
  /**
    * @param id ordering block (header) id
    * @return height of the best input block tip for the ordering block with identifier `id`
    */
  def getOrderingBlockTipHeight(id: ModifierId): Option[Int] = {
    bestHeights.get(id)
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

}
