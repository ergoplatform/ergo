package org.ergoplatform.nodeView.history.modifierprocessors

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.state.ErgoState
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

  private val bestInputBlocks = mutable.Map[ModifierId, Option[InputBlockInfo]]()
  /**
    * Pointer to a best input-block with transactions known
    */
  // todo: just read _bestInputBlock from bestInputBlocks ?
  private var _bestInputBlock: Option[InputBlockInfo] = None

  /**
    * Input block id -> input block index
    */
  private val inputBlockRecords = mutable.Map[ModifierId, InputBlockInfo]()

  /**
    * Index for input block id -> parent input block id (or None if parent is ordering block), and height from ordering block
    * First input-block after ordering block has height = 1.
    */
  private val inputBlockParents = mutable.Map[ModifierId, (Option[ModifierId], Int)]()

  /**
    * input block id -> input block transaction ids index
    */
  private val inputBlockTransactions = mutable.Map[ModifierId, Seq[ModifierId]]()

  /**
    * txid -> transaction index
    */
  // todo: improve removing, some txs included in forked input blocks may stuck in the cache
  private val transactionsCache = mutable.Map[ModifierId, ErgoTransaction]()

  /**
    * Best known chain tips (in terms of pow), input blocks in those chain do not necessarily have transactions (yet)
    * ordering block id -> best known input block chain tip ids
    */
  private val bestTips = mutable.Map[ModifierId, mutable.Set[ModifierId]]()

  /**
    * Best known input block chain tip heights known, input blocks not necessarily have transactions (yet)
    * ordering block id -> best known input block chain height
    */
  private val bestHeights = mutable.Map[ModifierId, Int]()

  /**
    * transactions generated AFTER an ordering block, till best known input block with transactions
    * block header (ordering block) -> transaction ids
    * so transaction ids do belong to transactions in input blocks since the block (header)
    */
  private val orderingInputBlocksTransactions = mutable.Map[ModifierId, Seq[ModifierId]]()

  private val orderingBlockTransactions = mutable.Map[ModifierId, Seq[ErgoTransaction]]()

  /**
    * waiting list for input blocks for which we got children for but the parent not delivered yet
    */
  private[modifierprocessors] val deliveryWaitlist = mutable.Set[ModifierId]()

  /**
    * Temporary cache of children which do not have parents downloaded yet
    */
  private[modifierprocessors] val disconnectedWaitlist = mutable.Set[InputBlockInfo]()

  private val invalid = mutable.Set[ModifierId]()

  private def bestOrderingBlock(): Option[Header] = historyReader.bestFullBlockOpt.map(_.header)

  /**
    * @return best ordering and input blocks
    */
  def bestBlocks: (Option[Header], Option[InputBlockInfo]) = {
    val bestOrdering = bestOrderingBlock()
    val bestInputForOrdering = if (_bestInputBlock.exists(sbi => bestOrdering.map(_.id).contains(sbi.header.parentId))) {
      _bestInputBlock
    } else {
      None
    }
    bestOrdering -> bestInputForOrdering
  }

  private def prune(): Unit = {
    val BlocksThreshold = 2 // we remove input-blocks data after 2 ordering blocks

    val bestHeight = _bestInputBlock.map(_.header.height).getOrElse(0)

    val orderingBlockIdsToRemove = bestHeights.keys.filter { orderingId =>
      bestHeight > historyReader.heightOf(orderingId).getOrElse(0)
    }.toSeq

    orderingBlockIdsToRemove.foreach { id =>
      bestHeights.remove(id)
      bestTips.remove(id)
      bestInputBlocks.remove(id)
      orderingInputBlocksTransactions.remove(id).map { ids =>
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
  private def resetState(doPruning: Boolean): Unit = {
    _bestInputBlock = None
    if (doPruning) {
      prune()
    }
  }

  /**
    * Update input block related structures with a new input block got from a local miner or p2p network
    *
    * @return id of another input block to download
    */
  // todo: use PoEM to store only 2-3 best chains and select best one quickly
  def applyInputBlock(ib: InputBlockInfo): Option[ModifierId] = {
    lazy val orderingId = ib.header.parentId

    // updates best known input block chain tips and best tip's height
    def updateBestTipsAndHeight(childId: ModifierId, parentIdOpt: Option[ModifierId], depth: Int): Unit = {
      def currentBestTips = bestTips.getOrElse(orderingId, mutable.Set.empty)
      def tipHeight = bestHeights.getOrElse(orderingId, 0)

      parentIdOpt.foreach { parentId =>
        bestTips.put(orderingId, currentBestTips -= parentId)
      }
      if (depth >= tipHeight || (currentBestTips.size < 3 && tipHeight >= 4 && depth >= tipHeight - 2)) {
        if (depth > tipHeight) {
          bestHeights.put(orderingId, depth)
        }
        bestTips.put(orderingId, currentBestTips += childId)
      }
    }

    // look through disconnected children to find ones which can be connected now
    def addChildren(parentId: ModifierId, parentDepth: Int): Unit = {
      val children = disconnectedWaitlist.filter(childIb =>
        childIb.prevInputBlockId.exists(pid => bytesToId(pid) == parentId)
      )
      val childDepth = parentDepth + 1
      children.foreach { childIb =>
        updateBestTipsAndHeight(childIb.id, Some(parentId), childDepth)
        inputBlockParents.put(childIb.id, Some(parentId) -> childDepth)
        disconnectedWaitlist.remove(childIb)
        addChildren(childIb.id, childDepth)
      }
    }

    if (ib.header.height > _bestInputBlock.map(_.header.height).getOrElse(-1)) {
      log.debug("Resetting state")
      resetState(false)
    }

    inputBlockRecords.put(ib.id, ib)

    val ibParentOpt = ib.prevInputBlockId.map(bytesToId)

    ibParentOpt.flatMap(parentId => inputBlockParents.get(parentId)) match {
      case Some((_, parentDepth)) =>
        val selfDepth = parentDepth + 1
        inputBlockParents.put(ib.id, ibParentOpt -> selfDepth)
        updateBestTipsAndHeight(ib.id, ibParentOpt, selfDepth)
        if (deliveryWaitlist.contains(ib.id)) {
          addChildren(ib.id, selfDepth)
        }
        None

      case None if ibParentOpt.isDefined =>
        deliveryWaitlist.add(ibParentOpt.get)
        disconnectedWaitlist.add(ib)
        ibParentOpt

      case None =>
        val selfDepth = 1
        inputBlockParents.put(ib.id, None -> selfDepth)
        updateBestTipsAndHeight(ib.id, None, selfDepth)
        if (deliveryWaitlist.contains(ib.id)) {
          addChildren(ib.id, selfDepth)
        }
        None
    }
  }

  // helper method to find best input block (tip of a best PoW chain containing transactions)
  private def processBestInputBlockCandidate(blockId: ModifierId,
                                             transactionIds: Seq[ModifierId],
                                             state: ErgoState[_]): Boolean = {
    val ib = inputBlockRecords.apply(blockId)
    val ibParentOpt = ib.prevInputBlockId.map(bytesToId)

    val res: Boolean = _bestInputBlock match {
      case None =>
        if (ibParentOpt.isEmpty && ib.header.parentId == historyReader.bestHeaderOpt.map(_.id).getOrElse("")) {
          val txs = transactionIds.map(id => transactionsCache.apply(id))
          val txsValid = state.applyInputBlock(txs, ib.header)
          if (txsValid.isSuccess) {
            log.info(s"Applying best input block #: ${ib.header.id}, no parent")
            val orderingId = ib.header.parentId
            bestInputBlocks += orderingId -> Some(ib)
            _bestInputBlock = Some(ib)
            true
          } else {
            // todo: more processing ?
            invalid.add(blockId)
            false
          }
        } else {
          false
        }
      case Some(maybeParent) if (ibParentOpt.contains(maybeParent.id)) =>
        val txs = transactionIds.map(id => transactionsCache.apply(id))
        val txsValid = state.applyInputBlock(txs, ib.header)
        if (txsValid.isSuccess) {
          log.info(s"Applying best input block #: ${ib.id} @ height ${ib.header.height}, header is ${ib.header.id}, parent is ${maybeParent.id}")
          val orderingId = ib.header.parentId
          bestInputBlocks += orderingId -> Some(ib)
          _bestInputBlock = Some(ib)
          true
        } else {
          // todo: eliminate common code with the previous branch
          // todo: more processing ?
          invalid.add(blockId)
          false
        }
      case _ =>
        ibParentOpt match {
          case Some(ibParent) =>
            // child of forked input block
            log.info(s"Applying forked input block #: ${ib.header.id}, with parent $ibParent")
            // todo: forks switching etc
            false
          case None =>
            // first input block since ordering block but another best block exists
            log.info(s"Applying forked input block #: ${ib.header.id}, with no parent")
            false
        }
    }

    if (res) {
      val orderingBlockId = _bestInputBlock.get.header.parentId
      val curr = orderingInputBlocksTransactions.getOrElse(orderingBlockId, Seq.empty)
      orderingInputBlocksTransactions.put(orderingBlockId, curr ++ transactionIds)
    }
    res
  }

  /**
    * @return - sequence of new best input blocks
    */
  //  todo: return input block ids rolled back?
  def applyInputBlockTransactions(sbId: ModifierId,
                                  transactions: Seq[ErgoTransaction],
                                  state: ErgoState[_]): Seq[ModifierId] = {
    log.info(s"Applying input block transactions for $sbId , transactions: ${transactions.size}")
    val transactionIds = transactions.map(_.id)
    inputBlockTransactions.put(sbId, transactionIds)

    // put transactions into cache shared among all the input blocks,
    // to avoid data duplication in input block related functions
    transactions.foreach { tx =>
      transactionsCache.put(tx.id, tx)
    }

    var forkingInputBlock: Option[ModifierId] = None

    inputBlockRecords.get(sbId) match {
      case Some(ib) if ib.prevInputBlockId.map(bytesToId) == bestInputBlock().map(_.id) =>
      // continuation of best input blocks chain, do nothing aside of linear tip update
      case Some(ib) =>
        val depth = inputBlockParents.get(sbId).map(_._2).getOrElse(1)
        val bestInputDepth = _bestInputBlock.map(_.id).flatMap(inputBlockParents.get).map(_._2).getOrElse(1)
        if (depth > bestInputDepth) {
          val orderingId = ib.header.parentId

          // find common input block and do rollback
          val thisChain = inputBlocksChain(sbId).reverse
          if (thisChain.forall(id => inputBlockTransactions.contains(id))) {

            val currentBestChain = bestInputBlocksChain().reverse
            var commonIndex = -1
            (0 until currentBestChain.length).foreach { idx =>
              if (thisChain(idx) == currentBestChain(idx)) {
                commonIndex = idx
              }
            }
            ((currentBestChain.length - 1).to(commonIndex + 1, -1)).foreach { idx =>
              val ibId = currentBestChain(idx)
              val txs = inputBlockTransactions.get(ibId).get
              // removing input-block transactions
              orderingInputBlocksTransactions.put(orderingId, orderingInputBlocksTransactions.apply(orderingId).filter(id => !txs.contains(id)))
            }

            if (commonIndex > -1) {
              val bestInputId = Some(inputBlockRecords(currentBestChain(commonIndex)))
              bestInputBlocks += orderingId -> bestInputId
              _bestInputBlock = bestInputId
              forkingInputBlock = Some(thisChain(commonIndex + 1))
            } else {
              val bestInputId = None
              bestInputBlocks += orderingId -> bestInputId
              _bestInputBlock = bestInputId
              forkingInputBlock = Some(thisChain.head)
            }
          }
        }
      case None =>
        log.warn(s"Input block transactions delivered for not known input block $sbId")
        // todo: should transactions be saved in this case ?
        return Seq.empty
    }

    @tailrec
    def bestInputBlockStep(sbId: ModifierId,
                           transactionIds: Seq[ModifierId],
                           state: ErgoState[_],
                           acc: Seq[ModifierId] = Seq.empty): Seq[ModifierId] = {
      if (processBestInputBlockCandidate(sbId, transactionIds, state)) {
        val orderingId = inputBlockRecords.get(sbId).map(_.header.parentId).get // todo: .get

        val maybeChildToApply = (bestTips.getOrElse(orderingId, Set.empty).flatMap { tipId =>
          isAncestor(tipId, sbId).map(_ -> tipId)
        }.filter { case (childId, _) =>
          inputBlockTransactions.contains(childId)
        }) match {
          case s if s.isEmpty => None
          case s => Some(s.maxBy { case (_, tipId) => inputBlockParents.get(tipId).map(_._2).getOrElse(0) }._1)
        }

        val updAcc = acc :+ sbId

        maybeChildToApply match {
          case Some(nsbId) =>
            inputBlockTransactions.get(sbId) match {
              case Some(ntransactionIds) => bestInputBlockStep(nsbId, ntransactionIds, state, updAcc)
              case None => updAcc
            }
          case None => updAcc
        }
      } else {
        acc
      }
    }

    if (forkingInputBlock.isEmpty) {
      bestInputBlockStep(sbId, transactionIds, state)
    } else {
      val sbId = forkingInputBlock.get
      val transactionIds = inputBlockTransactions.get(sbId).get
      bestInputBlockStep(sbId, transactionIds, state)
    }
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

  def inputBlocksChain(tipId: ModifierId): Seq[ModifierId] = {
    @tailrec
    def stepBack(acc: Seq[ModifierId], inputId: ModifierId): Seq[ModifierId] = {
      inputBlockParents.get(inputId) match {
        case Some((Some(parentId), _)) => stepBack(acc :+ parentId, parentId)
        case _ => acc
      }
    }

    stepBack(Seq(tipId), tipId)
  }

  /**
    * @return best known inputs-block chain for the current best-known ordering block
    */
  def bestInputBlocksChain(): Seq[ModifierId] = {
    bestInputBlock() match {
      case Some(tip) => inputBlocksChain(tip.id)
      case None => Seq.empty
    }
  }

  /**
    * Returns parent's immediate child that is an ancestor of the given child block
    *
    * @param child  id of descendant input block
    * @param parent id of ancestor input block
    * @return Some(parentChild) if found in child's ancestry chain, None otherwise
    */
  def isAncestor(child: ModifierId, parent: ModifierId): Option[ModifierId] = {
    @tailrec
    def loop(current: ModifierId, lastSeen: ModifierId): Option[ModifierId] = {
      inputBlockParents.get(current) match {
        case Some((Some(parentId), _)) if parentId == parent => Some(lastSeen)
        case Some((Some(parentId), _)) => loop(parentId, current)
        case _ => None
      }
    }

    if (child == parent) None else loop(child, child)
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
  def getCollectedInputBlocksTransactions(id: ModifierId): Option[Seq[ErgoTransaction]] = {
    // todo: cache input block transactions to avoid recalculating it on every input block regeneration?
    // todo: optimize the code below
    orderingInputBlocksTransactions.get(id).map { ids =>
      ids.flatMap(transactionsCache.get)
    }
  }

  def getBestOrderingCollectedInputBlocksTransactions(): Seq[ErgoTransaction] = {
    bestOrderingBlock().map(h => h.id).flatMap(getCollectedInputBlocksTransactions).getOrElse(Seq.empty)
  }

  def saveOrderingBlockTransactions(orderingBlockId: ModifierId, transactions: Seq[ErgoTransaction]) = {
    orderingBlockTransactions.put(orderingBlockId, transactions)
  }

  def getOrderingBlockTransactions(orderingBlockId: ModifierId): Option[Seq[ErgoTransaction]] = {
    orderingBlockTransactions.get(orderingBlockId)
  }

  def getBestInputBlock(orderingBlockId: ModifierId): Option[ModifierId] = {
    bestInputBlocks.get(orderingBlockId).flatten.map(_.id)
  }

}
