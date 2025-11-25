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
      if (processedIndex == -1) {
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
            val updChain =
              InputBlocksChain(chain :+ newInputBlock.id, processedIndex, costCollected)
            Seq(updChain)
          } else {
            val idx = chain.indexOf(prevId)
            // todo: fix costCollected in fork processing, it may decrease
            val newPi = Math.min(processedIndex, idx)
            val forkedChain = InputBlocksChain(
              chain.take(idx + 1) :+ newInputBlock.id,
              newPi,
              costCollected
            )
            Seq(this, forkedChain)
          }
        case _ =>
          log.error(s"Input block with no parent in fork(): ${newInputBlock.id}")
          Seq(this)
      }
    }

    lazy val collectedTransactions: Seq[ErgoTransaction] = {
      (0 to processedIndex).flatMap { i =>
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

    def registerCompletion(id: ModifierId, costDelta: Long): Try[InputBlocksChain] = {
      firstToComplete() match {
        case Some(expectedId)
            if expectedId == id => // todo: extra check which can be removed after release ?
          Success(InputBlocksChain(chain, processedIndex + 1, costCollected + costDelta))
        case _ =>
          val msg = s"Improper input-block completion: $id"
          log.error(msg)
          Failure(new Exception(msg))
      }
    }

    def applyTransactions(
      ib: InputBlockInfo,
      txs: Seq[ErgoTransaction],
      state: ErgoState[_]
    ): Try[(InputBlocksChain)] = {
      val prevTransactions = this.collectedTransactions
      val txsValid         = state.applyInputBlock(txs, prevTransactions, ib.header)
      txsValid match {
        case Success(cost) => registerCompletion(ib.id, cost)
        case Failure(e)    => Failure(e)

      }
    }

  }

  object InputBlocksChain {

    def apply(ib: InputBlockInfo): InputBlocksChain = {
      new InputBlocksChain(Seq(ib.id), -1, 0)
    }
  }

  case class InputBlocksTree(forks: Seq[InputBlocksChain]) {

    // todo: cache it?
    lazy val knownInputBlocks = forks.flatMap(_.chain).toSet

    private lazy val longestIndex = {
      var bl = -1
      var i  = -1
      (0 until forks.length).foreach { c =>
        if (forks(c).chain.length > bl) {
          bl = forks(c).chain.length
          i  = c
        }
      }
      i
    }

    def longestDepth: Option[Int] = {
      if (longestIndex != -1) {
        Some(forks(longestIndex).chain.length)
      } else None
    }

    private lazy val bestIndex = {
      var bl = -1
      var i  = -1
      (0 until forks.length).foreach { c =>
        if (forks(c).processedIndex > bl) {
          bl = forks(c).processedIndex
          i  = c
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
        val f = forks(bestIndex)
        f.chain.take(f.processedIndex + 1)
      } else Seq.empty
    }

    def bestChainTransactions: Seq[ErgoTransaction] = {
      if (bestIndex != -1) {
        forks(bestIndex).collectedTransactions
      } else Seq.empty
    }

    def insertInputBlock(ibi: InputBlockInfo): Option[InputBlocksTree] = {
      def applyDisconnected(acc: Seq[InputBlocksChain]): Seq[InputBlocksChain] = {
        disconnectedWaitlist.foldLeft(acc) {
          case (a, ib) =>
            val idx = acc.indexWhere(_.chain.lastOption == ib.prevInputBlockId)

            if (idx > -1) {
              val c         = a(idx)
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
        val chains   = applyDisconnected(Seq(newChain))
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

    /**
      * @return A tuple containing:
      *         - Sequence of new best input blocks applied (forward progress)
      *         - Sequence of input blocks rolled back (when switching forks)
      */
    def processInputBlockTransactions(
      ib: InputBlockInfo,
      txs: Seq[ErgoTransaction],
      state: ErgoState[_]
    ): (Seq[ModifierId], Seq[ModifierId]) = {
      @tailrec
      def applicationStep(
        ib: InputBlockInfo,
        txs: Seq[ErgoTransaction],
        acc: (InputBlocksChain, Seq[ModifierId])
      ): (InputBlocksChain, Seq[ModifierId]) = {
        acc._1.applyTransactions(ib, txs, state) match {
          case Success(updChain) =>
            val res = (updChain -> (acc._2 ++ Seq(ib.id)))
            updChain.firstToComplete().filter(inputBlockTransactions.contains) match {
              case Some(nextId) =>
                val nextIb = inputBlockRecords(nextId)
                val txs =
                  inputBlockTransactions(nextId).map(transactionsCache.getIfPresent)
                applicationStep(nextIb, txs, res)
              case _ => res
            }
          case Failure(e) =>
            log.warn(s"Application of input-block transactions failed for ${ib.id} : ", e)
            acc
        }
      }

      val bestIndex = if (this.bestIndex == -1) {
        this.longestIndex
      } else {
        this.bestIndex
      }
      if (bestIndex == -1) {
        return Seq.empty -> Seq.empty
      }

      def switchNeeded(id: ModifierId): Boolean = {
        val lf = forks(longestIndex)
        val d  = lf.depthOf(id)
        d > bestDepth && {
          (lf.processedIndex + 1 to d).forall { i =>
            val id = lf.chain(i)
            inputBlockTransactions.contains(id)
          }
        }
      }

      if (longestIndex != bestIndex && switchNeeded(ib.id)) { // forking case

        val currentFork = forks(bestIndex)
        val newFork    = forks(longestIndex)

        val rollbackInputBlocks = {
          var commonIdx = -1
          (0 until currentFork.chain.length).foreach { idx =>
            if (currentFork.chain(idx).sameElements(newFork.chain(idx)) && idx <= newFork.processedIndex) { // todo: finish
              commonIdx = idx
            }
          }
          if(commonIdx == -1 || commonIdx == currentFork.processedIndex){
            Seq.empty
          } else {
            currentFork.chain.slice(commonIdx + 1, currentFork.processedIndex)
          }
        }

        val ibId = newFork.chain(newFork.processedIndex + 1)
        val ib   = inputBlockRecords(ibId)
        val txs  = inputBlockTransactions(ibId).map(transactionsCache.getIfPresent)
        val r    = applicationStep(ib, txs, (newFork -> rollbackInputBlocks))
        if (r._2.nonEmpty) {
          // todo: eliminate boilerplate, see the same code in another branch below
          var updTree  = new InputBlocksTree(forks.updated(longestIndex, r._1))
          val updForks = updTree.forks
          (0 until updForks.length).foreach { idx =>
            val f = updForks(idx)
            if (f.firstToComplete().contains(ib.id)) {
              f.registerCompletion(ib.id, costDelta = 0) match { // todo: real cost
                case Success(ibc) =>
                  updTree = new InputBlocksTree(forks.updated(idx, ibc))
                case Failure(e) =>
                  log.warn(s"registerCompletion failed for input block ${ib.id} : ", e)
              }
            }
          }
          inputBlockTrees.put(ib.header.parentId, updTree) // todo: more beautiful modification of mutable state
          r._2 -> Seq.empty
        } else {
          log.warn("") // todo
          Seq.empty -> Seq.empty
        }
      } else if (forks(bestIndex).firstToComplete().contains(ib.id)) { // no forking
        val f = forks(bestIndex)
        val r = applicationStep(ib, txs, (f -> Seq.empty))
        if (r._2.nonEmpty) {
          // todo: eliminate boilerplate, see the same code in another branch below
          var updTree  = new InputBlocksTree(forks.updated(longestIndex, r._1))
          val updForks = updTree.forks
          (0 until updForks.length).foreach { idx =>
            val f = updForks(idx)
            if (f.firstToComplete().contains(ib.id)) {
              f.registerCompletion(ib.id, costDelta = 0) match { // todo: real cost
                case Success(ibc) =>
                  updTree = new InputBlocksTree(forks.updated(idx, ibc))
                case Failure(e) =>
                  log.warn(s"registerCompletion failed for input block ${ib.id} : ", e)
              }
            }
          }
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
  private val transactionsCache = CacheBuilder
    .newBuilder()
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
      bestOrdering
        .map(_.id)
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

    val inputBlockIdsToRemove = inputBlockRecords.flatMap {
      case (id, ibi) =>
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
    try {
      lazy val orderingId = extractOrderingId(ib)

      // if input-block corresponds to an ordering block @ better height, reset best input block reference
      // todo: make sure PoW and difficulty checked, to avoid low-diff block being sent in order to break input blocks chain
      if (ib.header.height > bestBlocks._1
        .map(_.height)
        .getOrElse(0) + 2) { // todo: beautify
        log.debug("Resetting state")
        resetState()
      }

      inputBlockRecords.put(ib.id, ib)

      /**
        * @return an optional if of input block to download
        */
      def updateTree(tree: InputBlocksTree): Option[ModifierId] = {
        tree.insertInputBlock(ib) match {
          case Some(updTree) =>
            inputBlockTrees.put(orderingId, updTree)
            None
          case None =>
            log.info("Put input block to disconnected queue: " + ib.id)
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
    } catch {
      case t: Throwable =>
        log.error(s"Can't apply input block ${ib.id}", t)
        None
    }
  }

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
  def applyInputBlockTransactions(
    sbId: ModifierId,
    transactions: Seq[ErgoTransaction],
    state: ErgoState[_]
  ): (Seq[ModifierId], Seq[ModifierId]) = {

    try {
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
          if (!bestBlocks._1.map(_.id).contains(orderingId)) {
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
    } catch {
      case t: Throwable =>
        log.error(s"Error in $sbId transactions application ", t)
        Seq.empty -> Seq.empty
    }

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
    bestOrderingBlock()
      .map(_.id)
      .flatMap(id => inputBlockTrees.get(id))
      .map(_.bestChain)
      .getOrElse(Seq.empty)
      .reverse
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
    val bd      = treeOpt.map(_.bestDepth).getOrElse(-1)
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
    bestOrderingBlock()
      .map(_.id)
      .flatMap(inputBlockTrees.get)
      .map(_.bestChainTransactions)
  }

  /**
    * @return all the transaction in best input-blocks chain collected after current best ordering block
    */
  def getBestOrderingCollectedInputBlocksTransactions(): Seq[ErgoTransaction] = {
    bestOrderingBlock()
      .map(h => h.id)
      .flatMap(getCollectedInputBlocksTransactions)
      .getOrElse(Seq.empty)
  }

  def saveOrderingBlockTransactions(orderingBlockId: ModifierId,
                                    transactions: Seq[ErgoTransaction]): Option[Seq[ErgoTransaction]] = {
    orderingBlockTransactions.put(orderingBlockId, transactions)
  }

  def getOrderingBlockTransactions(
    orderingBlockId: ModifierId
  ): Option[Seq[ErgoTransaction]] = {
    orderingBlockTransactions.get(orderingBlockId)
  }

}
