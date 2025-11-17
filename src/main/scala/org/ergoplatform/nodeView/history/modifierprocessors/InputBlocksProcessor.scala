package org.ergoplatform.nodeView.history.modifierprocessors

import com.google.common.cache.CacheBuilder
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncement
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.subblocks.InputBlockInfo
import scorex.util.{ModifierId, ScorexLogging}

import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

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

  private val PruningThreshold = 2 // we remove input-blocks data after 2 ordering blocks

  // input blocks chain since ordering
  case class InputBlocksChain(chain: Seq[ModifierId], processedIndex: Int, costCollected: Long) {
    def tip: Option[ModifierId] = {
      if(processedIndex == -1) {
        None
      } else {
        Some((chain(processedIndex)))
      }
    }

    def depthOf(id: ModifierId): Int = {
      chain.indexOf(id)
    }

    def complete: Boolean = processedIndex == chain.length

    def fork(newInputBlock: InputBlockInfo): Seq[InputBlocksChain] = {
      newInputBlock.prevInputBlockId match {
        case Some(prevId) =>
          if (prevId == chain.lastOption.getOrElse("")) {
            val updChain = InputBlocksChain(chain :+ newInputBlock.id, processedIndex, costCollected)
            Seq(updChain)
          } else {
            val idx = chain.indexOf(prevId)
            // todo: fix costCollected in fork processing, it may decrease
            val newPi = Math.min(processedIndex, idx)
            val forkedChain = InputBlocksChain(chain.take(idx + 1) :+ newInputBlock.id, newPi, costCollected)
            Seq(this, forkedChain)
          }
        case _ =>
          log.error(s"Input block with no parent in fork(): ${newInputBlock.id}")
          Seq(this)
      }
    }

    lazy val collectedTransactions: Seq[ErgoTransaction] = {
      (0 to processedIndex).flatMap{i =>
        val id = chain(i)
        inputBlockTransactions.get(id) match {
          case Some(txIds) =>
            //todo: more efficient loading
            txIds.flatMap { tid =>
              Option(transactionsCache.getIfPresent(tid))
            }
          case None =>
            Seq.empty
        }
      }
    }

    def firstToComplete(): Option[ModifierId] = {
      if ((processedIndex + 1) < chain.length && chain.nonEmpty) {
        Some(chain(processedIndex + 1))
      } else {
        None
      }
    }

    private def registerCompletion(id: ModifierId, costDelta: Long): Try[InputBlocksChain] = {
      firstToComplete() match {
        case Some(expectedId) if expectedId == id => // todo: extra check which can be removed after release ?
          Success(InputBlocksChain(chain, processedIndex + 1, costCollected + costDelta))
        case _ =>
          val msg = s"Improper input-block completion: $id"
          log.error(msg)
          Failure(new Exception(msg))
      }
    }

    def applyTransactions(ib: InputBlockInfo, txs: Seq[ErgoTransaction], state: ErgoState[_]): Try[(InputBlocksChain)] = {
      val prevTransactions = this.collectedTransactions
      val txsValid = state.applyInputBlock(txs, prevTransactions, ib.header)
      txsValid match {
        case Success(cost) => registerCompletion(ib.id, cost)
        case Failure(e) => Failure(e)

      }
    }

  }

  object InputBlocksChain {
    def apply(ib: InputBlockInfo): InputBlocksChain = {
      new InputBlocksChain(Seq(ib.id), -1, 0)
    }
  }

  case class InputBlocksTree(forks: Seq[InputBlocksChain]) {

    // todo: cache?
    lazy val knownInputBlocks = forks.flatMap(_.chain).toSet

    lazy private val longestIndex = {
      var bl = -1
      var i = -1
      (0 until forks.length).foreach { c =>
        if (forks(c).chain.length > bl) {
          bl = forks(c).chain.length
          i = c
        }
      }
      i
    }

    def longestDepth: Option[Int] = {
      if (longestIndex != -1) {
        Some(forks(longestIndex).chain.length)
      } else None
    }

    lazy private val bestIndex = {
      var bl = -1
      var i = -1
      (0 until forks.length).foreach { c =>
        if (forks(c).processedIndex > bl) {
          bl = forks(c).processedIndex
          i = c
        }
      }
      i
    }

    def bestDepth: Int = {
      if (bestIndex != -1) {
        forks(bestIndex).processedIndex
      } else -1
    }

    def bestTip: Option[ModifierId] = {
      if (bestIndex != -1) {
        forks(bestIndex).chain.lastOption
      } else None
    }

    def bestChain: Seq[ModifierId] = {
      if (bestIndex != -1) {
        forks(bestIndex).chain
      } else Seq.empty
    }

    def bestChainTransactions: Seq[ErgoTransaction] = {
      if (bestIndex != -1) {
        forks(bestIndex).collectedTransactions
      } else Seq.empty
    }

    def insertInputBlock(ibi: InputBlockInfo): Option[InputBlocksTree] = {
      def applyDisconnected(acc: Seq[InputBlocksChain]): Seq[InputBlocksChain] = {
        disconnectedWaitlist.foldLeft(acc) { case (a, ib) =>
          val idx = acc.indexWhere(_.chain.lastOption == ib.prevInputBlockId)

          if (idx > -1) {
            val c = a(idx)
            val newChains = c.fork(ib)
            a.updated(idx, newChains.head) ++ newChains.tail
          } else {
            a
          }
        }
      }

      val prevId = ibi.prevInputBlockId
      if (prevId.isEmpty) {
        val newChain = InputBlocksChain(ibi)
        val chains = applyDisconnected(Seq(newChain))
        Some(InputBlocksTree(forks ++ chains))
      } else {
        if (prevId.exists(id => knownInputBlocks.contains(id))) {
          val newForks = forks.flatMap { c =>
            if (c.chain.contains(prevId.get)) {
              val forked = c.fork(ibi)
              applyDisconnected(forked)
            } else {
              Seq(c)
            }
          }
          Some(InputBlocksTree(newForks))
        } else {
          None
        }
      }
    }

    case class InputBlockTxsProcessingResult(processingLog: (Seq[ModifierId], Seq[ModifierId]))
    /**
      * @return A tuple containing:
      *         - Sequence of new best input blocks applied (forward progress)
      *         - Sequence of input blocks rolled back (when switching forks)
      */
    def processInputBlockTransactions(ib: InputBlockInfo,
                                      txs: Seq[ErgoTransaction],
                                      state: ErgoState[_]): (Seq[ModifierId], Seq[ModifierId]) = {
      @tailrec
      def applicationStep(ib: InputBlockInfo,
                          txs: Seq[ErgoTransaction],
                          acc: (InputBlocksChain, Seq[ModifierId])): (InputBlocksChain, Seq[ModifierId]) = {
        acc._1.applyTransactions(ib, txs, state) match {
          case Success(updChain) =>
            val res = (updChain -> (acc._2 ++ Seq(ib.id)))
            updChain.firstToComplete().filter(inputBlockTransactions.contains) match {
              case Some(nextId)  =>
                val nextIb = inputBlockRecords(nextId)
                val txs = inputBlockTransactions(nextId).map(transactionsCache.getIfPresent)
                applicationStep(nextIb, txs, res)
              case _ => res
            }
          case Failure(e) =>
            log.warn(s"Application of input-block transactions failed for ${ib.id} : ", e)
            acc
        }
      }

      val bestIndex = if(this.bestIndex == -1){
        this.longestIndex
      } else {
        this.bestIndex
      }
      if (bestIndex == -1) {
        return Seq.empty -> Seq.empty
      }

      def switchNeeded(id: ModifierId): Boolean = {
        val lf = forks(longestIndex)
        val d = lf.depthOf(id)
        d > bestDepth && {
          (lf.processedIndex + 1 to d).forall{i =>
            val id = lf.chain(i)
            inputBlockTransactions.contains(id)
          }
        }
      }

      if(longestIndex != bestIndex && switchNeeded(ib.id)) {
        //todo: rollback
        val f = forks(longestIndex)
        val ibId = f.chain(f.processedIndex + 1)
        val ib = inputBlockRecords(ibId)
        val txs = inputBlockTransactions(ibId).map(transactionsCache.getIfPresent)
        val r = applicationStep(ib, txs, (f -> Seq.empty)) // todo: rollback instead of Seq.empty
        if (r._2.nonEmpty) {
          val updTree = new InputBlocksTree(forks.updated(longestIndex, r._1))
          inputBlockTrees.put(ib.header.parentId, updTree) // todo: more beautiful modification of mutable state
          r._2 -> Seq.empty
        } else {
          log.warn("") // todo
          Seq.empty -> Seq.empty
        }
      } else if(forks(bestIndex).firstToComplete().contains(ib.id)) {
        val f = forks(bestIndex)
        val r = applicationStep(ib, txs, (f -> Seq.empty))
        if (r._2.nonEmpty) {
          val updTree = new InputBlocksTree(forks.updated(bestIndex, r._1))
          inputBlockTrees.put(ib.header.parentId, updTree) // todo: more beautiful modification of mutable state
          r._2 -> Seq.empty
        } else {
          log.warn("") // todo
          Seq.empty -> Seq.empty
        }
      } else {
        log.debug("") // todo
        Seq.empty -> Seq.empty
      }
    }
  }

  object InputBlocksTree {
    def empty: InputBlocksTree = InputBlocksTree(Seq.empty)
  }

  // dictionary which is storing ordering block -> best input block correspondence
  private val inputBlockTrees = mutable.Map[ModifierId, InputBlocksTree]()

  /**
    * Input block id -> input block index
    */
  private val inputBlockRecords = mutable.Map[ModifierId, InputBlockInfo]()


  /**
    * input block id -> input block transaction ids index
    */
  // todo: transactions can be put here without input block received, ie PoW and difficulty checked
  // todo: thus they wont be cleared on pruning and the data structure can be DoSed. Fix by putting such transactions
  // todo: into a special queue
  private val inputBlockTransactions = mutable.Map[ModifierId, Seq[ModifierId]]()

  /**
    * txid -> transaction index
    *
    * We use Google Guava's cache with expiration, remove from cache after few ordering blocks of confirmation,
    * but in case of a transaction got into an input-blocks fork not confirmed by ordering blocks it can be stuck in
    * the cache till expiration (8 hours now)
    */
  // todo: elements of the cache are accessed via getIfPresent without being checked for null result
  // todo: as they should be in the cache always, but in some extreme cases could be possible exceptions
  private val transactionsCache = CacheBuilder.newBuilder()
    .maximumSize(1000000)
    .expireAfterWrite(120, TimeUnit.MINUTES) // 2 hours
    .build[ModifierId, ErgoTransaction]()

  /**
    * Transactions commited in an ordering block
    * Ordering (full) block -> transactions committed by it
    */
  private val orderingBlockTransactions = mutable.Map[ModifierId, Seq[ErgoTransaction]]()

  /**
    * Temporary cache of children which do not have parents downloaded yet
    */
  private[modifierprocessors] val disconnectedWaitlist = mutable.Set[InputBlockInfo]()


  private def bestOrderingBlock(): Option[Header] = historyReader.bestFullBlockOpt.map(_.header)

  // extracts ordering block id from input block data provided
  private def extractOrderingId(ib: InputBlockInfo) = ib.header.parentId

  /**
    * @return best ordering and input blocks
    */
  def bestBlocks: (Option[Header], Option[InputBlockInfo]) = {
    val bestOrdering = bestOrderingBlock()
    val bestInputForOrdering =
      bestOrdering.map(_.id)
        .flatMap(inputBlockTrees.get)
        .flatMap(_.bestTip)
        .flatMap(inputBlockRecords.get)
    bestOrdering -> bestInputForOrdering
  }

  //todo: recheck that all the structures are cleared
  private def prune(): Unit = {
    val bestHeight = bestBlocks._1.map(_.height).getOrElse(0)

    val orderingBlockIdsToRemove = inputBlockTrees.keys.filter { orderingId =>
      bestHeight > historyReader.heightOf(orderingId).getOrElse(0)
    }.toSeq

    orderingBlockIdsToRemove.foreach { id =>
      inputBlockTrees.remove(id)
    }

    val inputBlockIdsToRemove = inputBlockRecords.flatMap { case (id, ibi) =>
      val res = (bestHeight - ibi.header.height) > PruningThreshold
      if (res) {
        Some(id)
      } else {
        None
      }
    }

    inputBlockIdsToRemove.foreach { id =>
      log.debug(s"Pruning input block # $id")
      inputBlockRecords.remove(id).foreach { ibi =>
        disconnectedWaitlist.remove(ibi)
      }
      inputBlockTransactions.remove(id)
    }

  }

  // reset sub-blocks structures, should be called on receiving ordering block (or slightly later?)
  private def resetState(): Unit = {
    prune()
  }

  /**
    * Update input block related structures with a new input block got from a local miner or p2p network
    * We dont have input block transactions yet (usually) when this method is called.
    *
    * @return id of parent input block to download, if it is not known to us
    */
  def applyInputBlock(ib: InputBlockInfo): Option[ModifierId] = {
    lazy val orderingId = extractOrderingId(ib)

    // =============== helper functions ===========================

    // updates best known input block chain tips and best tip's height
    /* def updateBestTipsAndHeight(childId: ModifierId, parentIdOpt: Option[ModifierId], depth: Int): Unit = {
      def currentBestTips = bestTips.getOrElse(orderingId, mutable.Set.empty)
      def tipHeight = bestHeights.getOrElse(orderingId, 0)

      parentIdOpt.foreach { parentId =>
        bestTips.put(orderingId, currentBestTips -= parentId)
      }
      if (depth >= tipHeight) { //} || (currentBestTips.size < 3 && tipHeight >= 4 && depth >= tipHeight - 2)) {
        if (depth > tipHeight) {
          bestHeights.put(orderingId, depth)
        }
        bestTips.put(orderingId, currentBestTips += childId)
      }
    } */

    // look through disconnected children to find ones which can be connected now
   /* def addChildren(parentId: ModifierId, parentDepth: Int): Unit = {
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
    } */

    // =============== main function ===========================

    // if input-block corresponds to an ordering block @ better height, reset best input block reference
    // todo: make sure PoW and difficulty checked, to avoid low-diff block being sent in order to break input blocks chain
    if (ib.header.height > bestBlocks._1.map(_.height).getOrElse(0) + 2) { // todo: beautify
      log.debug("Resetting state")
      resetState()
    }

    inputBlockRecords.put(ib.id, ib)

    // val ibParentOpt = ib.prevInputBlockId.map(bytesToId)

    def updateTree(tree: InputBlocksTree): Option[ModifierId] = {
      tree.insertInputBlock(ib) match {
        case Some(updTree) =>
          inputBlockTrees.put(orderingId, updTree)
          None
        case None =>
          println("adding to disconnected queue: " + ib.id)
          disconnectedWaitlist.add(ib)
          ib.prevInputBlockId
      }
    }

    inputBlockTrees.get(orderingId) match {
      case Some(tree) =>
        updateTree(tree)
      case None =>
        val tree = InputBlocksTree.empty
        inputBlockTrees.put(orderingId, tree)
        updateTree(tree)
    }

/*
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
        // parent input-block exists, but not known to us, remember it and request downloading it
        deliveryWaitlist.add(ibParentOpt.get)
        disconnectedWaitlist.add(ib)
        ibParentOpt

      case None =>
        // there is no parent input-block, thus this input block is the first generated after its ordering block
        val selfDepth = 1
        inputBlockParents.put(ib.id, None -> selfDepth)
        updateBestTipsAndHeight(ib.id, None, selfDepth)
        if (deliveryWaitlist.contains(ib.id)) {
          addChildren(ib.id, selfDepth)
        }
        None
    } */
  }

  // helper method to find best input block (tip of a best PoW chain containing transactions)
  /*
  private def processBestInputBlockCandidate(blockId: ModifierId,
                                             transactionIds: Seq[ModifierId],
                                             state: ErgoState[_]): Boolean = {
    val ib = inputBlockRecords.apply(blockId)
    val ibParentOpt = ib.prevInputBlockId.map(bytesToId)
    val orderingId = extractOrderingId(ib)


    println("ib : " + ib.id + " parentid: " + ibParentOpt + " _bestInputBlock: " + _bestInputBlock.map(_.id))
    val res: Boolean = _bestInputBlock match {
      case None =>
        if (ibParentOpt.isEmpty && orderingId == historyReader.bestHeaderOpt.map(_.id).getOrElse("")) {
          val txs = transactionIds.map(id => transactionsCache.getIfPresent(id))
          val txsValid = state.applyInputBlock(txs, Seq.empty, ib.header)
          if (txsValid.isSuccess) {
            log.info(s"Applying best input block #: ${ib.header.id}, no parent")
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
        val txs = transactionIds.map(id => transactionsCache.getIfPresent(id))

        // todo: checks
        val previousTxs = orderingInputBlocksTransactions.get(orderingId).map(_.map(transactionsCache.getIfPresent)).getOrElse(Seq.empty)

        val txsValid = state.applyInputBlock(txs, previousTxs, ib.header)
        if (txsValid.isSuccess) {
          log.info(s"Applying best input block #: ${ib.id} @ height ${ib.header.height}, header is ${ib.header.id}, parent is ${maybeParent.id}")
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
      val orderingBlockId = extractOrderingId(_bestInputBlock.get) // todo: .get
      val curr = orderingInputBlocksTransactions.getOrElse(orderingBlockId, Seq.empty)
      orderingInputBlocksTransactions.put(orderingBlockId, curr ++ transactionIds)
    }
    res
  }
   */

  /**
    * Applies input block transactions and updates the best input block chain.
    * 
    * This method is the core of input block processing, handling both linear chain extension
    * and fork switching scenarios. It manages the state transitions when new input blocks
    * with transactions are received.
    *
    * @param sbId The input block ID to process
    * @param transactions The transactions contained in the input block
    * @param state The current Ergo state for transaction validation
    * @return A tuple containing:
    *         - Sequence of new best input blocks applied (forward progress)
    *         - Sequence of input blocks rolled back (when switching forks)
    */
  // todo: use PoEM to store only 2-3 best chains and select best one quickly
  def applyInputBlockTransactions(sbId: ModifierId,
                                  transactions: Seq[ErgoTransaction],
                                  state: ErgoState[_]): (Seq[ModifierId], Seq[ModifierId]) = {

    log.info(s"Applying ${transactions.size} input block transactions for $sbId")
    val transactionIds = transactions.map(_.id)
    inputBlockTransactions.put(sbId, transactionIds)

    // put transactions into cache shared among all the input blocks,
    // to avoid data duplication in input block related functions
    transactions.foreach { tx =>
      transactionsCache.put(tx.id, tx)
    }

    inputBlockRecords.get(sbId) match {
      case Some(ib) =>
        val orderingId = extractOrderingId(ib)
        if(!bestBlocks._1.map(_.id).contains(orderingId)){
          return Seq.empty -> Seq.empty
        }
        inputBlockTrees.get(orderingId) match {
          case Some(tree) =>
            tree.processInputBlockTransactions(ib, transactions, state)
          case None =>
            Seq.empty -> Seq.empty
        }

      case None =>
        log.warn(s"Input block transactions delivered for unknown input block $sbId")
        // todo: should transactions be saved in this case ?
        Seq.empty -> Seq.empty
    }

    /*
        /**
          * Recursively processes the best input block chain by applying transactions and moving to the next child block.
          *
          * This is the core algorithm for input block chain progression. It implements a tail-recursive
          * traversal that:
          * 1. Attempts to process the current input block candidate with its transactions
          * 2. If successful, finds the best child block to process next
          * 3. Recursively continues with the child block
          * 4. Returns the accumulated sequence of processed block IDs
          *
          * The function ensures that only valid chains are extended and maintains the invariant that
          * the best chain contains only blocks with valid transactions that pass state validation.
          *
          * Key characteristics:
          * - Tail-recursive for stack safety with long chains
          * - Processes blocks in depth-first order along the best chain
          * - Stops when no valid child blocks are available
          * - Accumulates successfully processed block IDs
          *
          * @return Sequence of input block IDs that were successfully processed in order
          */
        @tailrec
        def bestInputBlockStep(sbId: ModifierId,
                               transactionIds: Seq[ModifierId],
                               state: ErgoState[_],
                               acc: Seq[ModifierId] = Seq.empty): Seq[ModifierId] = {

          // Attempt to process the current block candidate
          if (processBestInputBlockCandidate(sbId, transactionIds, state)) {
            val orderingId = inputBlockRecords.get(sbId).map(extractOrderingId).get // todo: .get

            // Find the best child block to process next
            // This selects from the best tips that are descendants of the current block
            // and have their transactions available
            val maybeChildToApply = (bestTips.getOrElse(orderingId, Set.empty).flatMap { tipId =>
              isAncestor(tipId, sbId).map(_ -> tipId)
            }.filter { case (childId, _) =>
              inputBlockTransactions.contains(childId)
            }) match {
              case s if s.isEmpty => None
              // Select the child with the highest depth (longest chain)
              case s => Some(s.maxBy { case (_, tipId) => inputBlockParents.get(tipId).map(_._2).getOrElse(0) }._1)
            }

            val updAcc = acc :+ sbId

            // Recursively process the next child block if available
            maybeChildToApply match {
              case Some(nsbId) =>
                inputBlockTransactions.get(sbId) match {
                  case Some(ntransactionIds) => bestInputBlockStep(nsbId, ntransactionIds, state, updAcc)
                  case None => updAcc
                }
              case None => updAcc
            }
          } else {
            // Current block processing failed, return accumulated results
            acc
          }
        }

        log.info(s"Applying ${transactions.size} input block transactions for $sbId")
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
              log.info(s"Switching input-block forks as $depth > $bestInputDepth") // todo: make debug before release
              val orderingId = extractOrderingId(ib)

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
                  val updTxs = orderingInputBlocksTransactions.get(orderingId).getOrElse(Seq.empty).filter(id => !txs.contains(id))
                  orderingInputBlocksTransactions.put(orderingId, updTxs)
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
              } else {
                log.warn("Broken input-blocks chain during fork switching attempt")
              }
            }
          case None =>
            log.warn(s"Input block transactions delivered for unknown input block $sbId")
            // todo: should transactions be saved in this case ?
            return Seq.empty -> Seq.empty
        }

        if (forkingInputBlock.isEmpty) {
          bestInputBlockStep(sbId, transactionIds, state) -> Seq.empty
        } else {
          val sbId = forkingInputBlock.get
          val transactionIds = inputBlockTransactions.get(sbId).get // todo: .get
          val applied = bestInputBlockStep(sbId, transactionIds, state)
          val rolledBack = Seq.empty
          applied -> rolledBack
        }
        */
  }

  def updateStateWithOrderingBlock(h: Header): Unit = {
    if (h.height >= bestOrderingBlock().map(_.height).getOrElse(-1)) {
      resetState()
    }
  }

  // Getters to serve client requests below

  /**
    * Returns the best input block for the current best ordering block.
    * 
    * @return the best input block information if available, None otherwise
    */
  def bestInputBlock(): Option[InputBlockInfo] = {
    bestBlocks._2
  }

  /**
    * Returns the input blocks tree structure for the current best ordering block.
    * 
    * @return the input blocks tree if available, None otherwise
    */
  def inputBlocksTree(): Option[InputBlocksTree] = {
    bestBlocks._1.flatMap(h => inputBlockTrees.get(h.id))
  }

  /**
    * Returns the best known input blocks chain for the current best-known ordering block.
    * The chain is returned in reverse order (from tip to genesis).
    */
  def bestInputBlocksChain(): Seq[ModifierId] = {
    bestOrderingBlock().map(_.id).flatMap(id => inputBlockTrees.get(id)).map(_.bestChain).getOrElse(Seq.empty).reverse
  }


  /**
    * Retrieves an input block by its modifier ID.
    */
  def getInputBlock(sbId: ModifierId): Option[InputBlockInfo] = {
    inputBlockRecords.get(sbId)
  }

  /**
    * Retrieves the transaction IDs contained in a specified input block.
    */
  def getInputBlockTransactionIds(sbId: ModifierId): Option[Seq[ModifierId]] = {
    inputBlockTransactions.get(sbId)
  }

  /**
    * Retrieves transactions for a specified input block.
    */
  def getInputBlockTransactions(sbId: ModifierId): Option[Seq[ErgoTransaction]] = {
    // todo: cache input block transactions to avoid recalculating it on every p2p request
    // todo: optimize the code below
    inputBlockTransactions.get(sbId).map { ids =>
      ids.map(transactionsCache.getIfPresent)
    }
  }

  // todo: pruning
  private val orderingBlockAnnouncements = mutable.Map[ModifierId, OrderingBlockAnnouncement]()

  def storeOrderingBlockAnnouncement(announcement: OrderingBlockAnnouncement): Unit = {
    val id = announcement.header.id
    orderingBlockAnnouncements.put(id, announcement)
  }

  def getOrderingBlockAnnouncement(id: ModifierId): Option[OrderingBlockAnnouncement] = {
    orderingBlockAnnouncements.get(id)
  }



  /**
    * @param sbId
    * @param toFilter - weak ids of transactions which SHOULD BE in resul
    * @return
    */
  def getInputBlockTransactions(sbId: ModifierId,
                                toFilter: Seq[ErgoTransaction.WeakId]): Option[Seq[ErgoTransaction]] = {
    // todo: cache input block transactions to avoid recalculating it on every p2p request
    // todo: optimize the code below
    inputBlockTransactions.get(sbId).map { ids =>
      ids.flatMap { id =>
        val tx = transactionsCache.getIfPresent(id)
        if (toFilter.exists(fId => tx.weakId.sameElements(fId))) {
          Some(tx)
        } else {
          None
        }
      }
    }
  }

  def getInputBlockTransactionWeakIds(sbId: ModifierId): Option[Seq[ErgoTransaction.WeakId]] = {
    // todo: cache input block transactions to avoid recalculating it on every p2p request
    // todo: optimize the code below
    inputBlockTransactions.get(sbId).map { ids =>
      ids.map(transactionsCache.getIfPresent).map(_.weakId)
    }
  }

  /**
    * @param id ordering block (header) id
    * @return tips (leaf input blocks) for the ordering block with identifier `id`
    */
  def getOrderingBlockTips(id: ModifierId): Option[Set[ModifierId]] = {
    val treeOpt = inputBlockTrees.get(id)
    val bd = treeOpt.map(_.bestDepth).getOrElse(-1)
    treeOpt.map(_.forks.filter(_.processedIndex == bd).flatMap(_.tip).toSet)
  }

  /**
    * @param id ordering block (header) id
    * @return height of the best input block tip for the ordering block with identifier `id`
    */
  def getOrderingBlockTipHeight(id: ModifierId): Int = {
    inputBlockTrees.get(id).map(_.bestDepth).getOrElse(-1)
  }

  def getLongestChainLength(id: ModifierId): Int = {
    inputBlockTrees.get(id).flatMap(_.longestDepth).getOrElse(-1)
  }


  /**
    * @param id ordering block (header) id
    * @return transactions included in best input blocks chain since ordering block with identifier `id`
    */
  def getCollectedInputBlocksTransactions(id: ModifierId): Option[Seq[ErgoTransaction]] = {
    bestOrderingBlock().map(_.id).flatMap(inputBlockTrees.get).map(_.bestChainTransactions)
  }

  /**
    * @return all the transaction in best input-blocks chain collected after current best ordering block
    */
  def getBestOrderingCollectedInputBlocksTransactions(): Seq[ErgoTransaction] = {
    bestOrderingBlock().map(h => h.id).flatMap(getCollectedInputBlocksTransactions).getOrElse(Seq.empty)
  }

  def saveOrderingBlockTransactions(orderingBlockId: ModifierId,
                                    transactions: Seq[ErgoTransaction]): Option[Seq[ErgoTransaction]] = {
    orderingBlockTransactions.put(orderingBlockId, transactions)
  }

  def getOrderingBlockTransactions(orderingBlockId: ModifierId): Option[Seq[ErgoTransaction]] = {
    orderingBlockTransactions.get(orderingBlockId)
  }

}
