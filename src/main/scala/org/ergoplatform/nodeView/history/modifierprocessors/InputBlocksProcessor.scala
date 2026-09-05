package org.ergoplatform.nodeView.history.modifierprocessors

import com.google.common.cache.CacheBuilder
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.message.inputblocks.OrderingBlockAnnouncement
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.settings.Algos
import org.ergoplatform.subblocks.InputBlockAnnouncement
import scorex.crypto.authds.LeafData
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, ScorexLogging}
import spire.syntax.all.cfor

import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}


/**
  * Trait responsible for storing and processing input-blocks related data in the Ergo blockchain protocol.
  *
  * Input blocks are a key component of Ergo's two-tier blockchain architecture, where full blocks (ordering blocks)
  * contain headers and proofs-of-work, while input blocks contain transactions that reference these full blocks.
  * This processor manages the relationship between ordering blocks and input blocks, handles transaction processing,
  * manages chain forks, and performs state transitions.
  *
  * Key responsibilities:
  * - Store input blocks temporarily (pruned after a threshold to conserve memory)
  * - Manage multiple competing input block chains (forks) for the same ordering block
  * - Process transactions within input blocks and validate them against the current state
  * - Handle fork switching when a longer chain is discovered
  * - Maintain transaction caches and indexes for efficient retrieval
  * - Coordinate with the history reader to stay synchronized with the best chain
  *
  * The processor implements a sophisticated caching and pruning strategy to balance memory usage
  * with the need to handle multiple chain forks and maintain transaction availability.
  */
trait InputBlocksProcessor extends ScorexLogging {

  /**
    * @return interface to read objects from history database
    */
  def historyReader: ErgoHistoryReader

  private val PruningThreshold = 2 // we remove input-blocks data after 2 ordering blocks

  /**
    * Represents a chain of input blocks forming a sequence from an ordering block.
    *
    * This class tracks both the logical chain of input block IDs and the processing state
    * of each block in the chain. It supports fork detection and creation when new input
    * blocks reference earlier blocks in the chain.
    *
    * @param chain The sequence of input block IDs forming the chain
    * @param processedBlocks The sequence of processing costs for each successfully processed block
    */
  case class InputBlocksChain(chain: Seq[ModifierId], processedBlocks: Seq[Long]) {

    /** Current index of the last processed block in the chain (-1 if none processed) */
    val processedIndex: Int = processedBlocks.length - 1

    /**
      * Gets the ID of the tip (most recent processed) input block in the chain.
      *
      * @return Some(modifier ID) if there are processed blocks, None otherwise
      */
    def tip: Option[ModifierId] = {
      if (processedIndex == -1) {
        None
      } else {
        Some((chain(processedIndex)))
      }
    }

    /**
      * Calculates the depth (position) of a given input block in the chain.
      *
      * @param id The modifier ID to find the depth for
      * @return The zero-based index of the block in the chain, or -1 if not found
      */
    def depthOf(id: ModifierId): Int = {
      chain.indexOf(id)
    }

    /**
      * Checks if the entire input block chain has been processed.
      *
      * @return true if all blocks in the chain have been processed, false otherwise
      */
    def complete: Boolean = processedIndex == chain.length

    /**
      * Creates a new fork in the input block chain when a new block references an earlier block.
      *
      * This method handles the creation of competing input block chains when a new input block
      * references a parent that is not the tip of the current chain, indicating a fork in the
      * input block sequence.
      *
      * Algorithm:
      * 1. If the new input block references the current chain tip, extend the chain linearly
      * 2. If the new input block references an earlier block in the chain:
      *    - Find the position of the referenced parent in the current chain
      *    - Create a new forked chain starting from the referenced parent and including the new block
      *    - Return both the original chain and the new forked chain
      * 3. If the parent is unknown, return the original chain unchanged
      *
      * @param newInputBlock The new input block to add to the chain
      * @return A sequence containing the original chain and any newly created forked chains
      */
    def fork(newInputBlock: InputBlockAnnouncement): Seq[InputBlocksChain] = {
      newInputBlock.prevInputBlockId match {
        case Some(prevId) =>
          if (prevId == chain.lastOption.getOrElse("")) {
            // Linear extension: new block references the current chain tip
            val updChain =
              InputBlocksChain(chain :+ newInputBlock.id, processedBlocks)
            Seq(updChain)
          } else {
            // Fork scenario: new block references an earlier block in the chain
            val idx = chain.indexOf(prevId)
            if (idx >= 0) {
              // Create a new forked chain from the referenced parent onwards
              val forkedChain = InputBlocksChain(
                chain.take(idx + 1) :+ newInputBlock.id,  // Chain from genesis to parent + new block
                processedBlocks.take(idx + 1)             // Processed blocks up to parent
              )
              log.info(s"Fork detected: creating new fork from ${prevId} at index $idx with input block ${newInputBlock.id} " +
                s"Original chain length: ${chain.length}, forked chain length: ${forkedChain.chain.length}")
              Seq(this, forkedChain)  // Return both original and forked chains
            } else {
              log.warn(s"Input block ${newInputBlock.id} references unknown parent $prevId, cannot fork")
              Seq(this)
            }
          }
        case _ =>
          log.error(s"Input block with no parent in fork(): ${newInputBlock.id}")
          Seq(this)
      }
    }

    /**
      * Collects all transactions from the processed portion of the input block chain.
      *
      * This method aggregates transactions from all blocks that have been successfully
      * processed in the chain, up to the current processedIndex.
      *
      * @return A sequence of all transactions from processed input blocks in the chain
      */
    lazy val collectedTransactions: Seq[ErgoTransaction] = {
      val result = mutable.ArrayBuffer[ErgoTransaction]()
      cfor(0)(_ <= processedIndex, _ + 1) { i =>
        val id = chain(i)
        inputBlockTransactions.get(id) match {
          case Some(txIds) =>
            cfor(0)(_ < txIds.length, _ + 1) { j =>
              val tid = txIds(j)
              val tx = transactionsCache.getIfPresent(tid)
              if (tx != null) {
                result += tx
              } else {
                log.warn(s"Transaction $tid not found in cache (expired or evicted)")
              }
            }
          case None => // skip
        }
      }
      result
    }

    /**
      * Gets the ID of the next input block that needs to be processed in the chain.
      *
      * @return Some(modifier ID) of the next block to process, or None if all are processed
      */
    def firstToComplete(): Option[ModifierId] = {
      if ((processedIndex + 1) < chain.length && chain.nonEmpty) {
        Some(chain(processedIndex + 1))
      } else {
        None
      }
    }

    /**
      * Registers the successful completion of an input block processing.
      *
      * Updates the chain state to reflect that the given input block has been processed
      * with the specified computational cost.
      *
      * @param id The ID of the input block that was completed
      * @param costDelta The computational cost of processing this block
      * @return Success with the updated InputBlocksChain if the completion is valid,
      *         Failure with an exception if the completion is unexpected
      */
    def registerCompletion(id: ModifierId, costDelta: Long): Try[InputBlocksChain] = {
      firstToComplete() match {
        case Some(expectedId) if expectedId == id =>
          Success(InputBlocksChain(chain, processedBlocks :+ costDelta))
        case _ =>
          val msg = s"Improper input-block completion: $id, expected ${firstToComplete().getOrElse("None")}"
          log.error(msg)
          Failure(new Exception(msg))
      }
    }

    /**
      * Applies transactions from an input block to the current state and registers completion.
      *
      * This method validates the transactions against the current Ergo state and, if successful,
      * updates the chain's processing state to include this block.
      *
      * @param ib The input block information to process
      * @param txs The transactions contained in the input block
      * @param state The current Ergo state to validate transactions against
      * @return Success with the updated InputBlocksChain if transactions are valid,
      *         Failure with an exception if validation fails
      */
    def applyTransactions(
      ib: InputBlockAnnouncement,
      txs: Seq[ErgoTransaction],
      state: ErgoState[_]
    ): Try[(InputBlocksChain)] = {
      val prevTransactions = this.collectedTransactions
      val txsValid         = state.applyInputBlock(txs, prevTransactions, ib.header)
      txsValid match {
        case Success(cost) =>
          log.debug(s"Successfully applied transactions for input block ${ib.id}, cost: $cost")
          registerCompletion(ib.id, cost)
        case Failure(e) =>
          log.warn(s"Failed to apply transactions for input block ${ib.id}: ${e.getMessage}")
          Failure(e)
      }
    }

  }

  object InputBlocksChain {

    def apply(ib: InputBlockAnnouncement): InputBlocksChain = {
      new InputBlocksChain(Seq(ib.id), Seq.empty)
    }

  }

  /**
    * Represents a tree structure of competing input block chains for a single ordering block.
    *
    * This class manages multiple possible input block chains (forks) that compete to become
    * the canonical chain for a given ordering block. It tracks the longest chain and the
    * best (most processed) chain, enabling fork resolution and chain selection.
    *
    * @param forks The sequence of competing input block chains
    */
  case class InputBlocksTree(forks: Seq[InputBlocksChain]) {

    // Log fork information
    if (forks.length > 1) {
      log.info(s"InputBlocksTree has ${forks.length} competing forks. Best depth: ${bestDepth}, Longest depth: ${longestDepth.getOrElse(0)}")
    }

    /**
      * Set of all known input block IDs across all competing forks.
      * Used for quick lookup to determine if an input block is already known.
      */
    // todo: cache it?
    lazy val knownInputBlocks = forks.flatMap(_.chain).toSet

    /** Index of the fork with the longest chain (by number of blocks) */
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

    /**
      * Gets the length of the longest fork in terms of number of input blocks.
      *
      * @return Some(length) of the longest fork, or None if no forks exist
      */
    def longestDepth: Option[Int] = {
      if (longestIndex != -1) {
        Some(forks(longestIndex).chain.length)
      } else None
    }

    /** Index of the fork with the highest processing depth (most processed blocks) */
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

    /**
      * Gets the processing depth of the best fork (number of processed blocks).
      *
      * @return The number of processed blocks in the best fork, or -1 if no forks exist
      */
    def bestDepth: Int = {
      if (bestIndex != -1) {
        forks(bestIndex).processedIndex
      } else -1
    }

    /**
      * Gets the ID of the tip (last processed block) of the best fork.
      *
      * @return Some(modifier ID) of the best fork's tip, or None if no forks exist
      */
    def bestTip: Option[ModifierId] = {
      if (bestIndex != -1) {
        forks(bestIndex).chain.lastOption
      } else None
    }

    /**
      * Gets the complete chain of processed input blocks from the best fork.
      *
      * @return A sequence of modifier IDs representing the best chain of processed blocks
      */
    def bestChain: Seq[ModifierId] = {
      if (bestIndex != -1) {
        val f = forks(bestIndex)
        f.chain.take(f.processedIndex + 1)
      } else Seq.empty
    }

    /**
      * Gets all transactions from the processed portion of the best fork.
      *
      * @return A sequence of all transactions from processed blocks in the best fork
      */
    def bestChainTransactions: Seq[ErgoTransaction] = {
      if (bestIndex != -1) {
        forks(bestIndex).collectedTransactions
      } else Seq.empty
    }

    /**
      * Inserts a new input block into the tree, potentially creating new forks.
      *
      * This method handles the insertion of a new input block into the appropriate
      * fork in the tree. If the input block creates a new fork, it will be added
      * to the tree structure.
      *
      * Algorithm:
      * 1. Process any disconnected blocks that can now be connected
      * 2. If the input block has no parent, create a new chain
      * 3. If the parent is known, find the appropriate chain and insert the block
      * 4. If the parent is unknown, add the block to the disconnected waitlist
      *
      * @param ibi The input block information to insert
      * @return Some(updated InputBlocksTree) if the block was inserted successfully,
      *         None if the parent block is unknown and the block was added to the disconnected waitlist
      */
    def insertInputBlock(ibi: InputBlockAnnouncement): Option[InputBlocksTree] = {
      /**
       * Processes disconnected input blocks that may now be connectable to the current chains.
       *
       * This helper function attempts to connect any previously disconnected input blocks
       * to the current set of chains. It checks if any disconnected blocks have parents
       * that are now present in the accumulated chains.
       *
       * @param acc The sequence of input block chains to try connecting to
       * @return Updated sequence of chains with any newly connected blocks
       */
      def applyDisconnected(acc: Seq[InputBlocksChain]): Seq[InputBlocksChain] = {
        disconnectedWaitlist.foldLeft(acc) {
          case (a, ib) =>
            // Find the index of the chain whose tip matches the parent of the disconnected block
            val idx = acc.indexWhere(_.chain.lastOption == ib.prevInputBlockId)

            if (idx > -1) {
              // Found a chain to attach to, create fork if needed
              val c         = a(idx)
              val newChains = c.fork(ib)  // May create a fork if ib references an earlier block in the chain
              a.updated(idx, newChains.head) ++ newChains.tail  // Update the chain with new forks
            } else {
              // No matching parent found, leave the chain unchanged
              a
            }
        }
      }

      val prevId = ibi.prevInputBlockId
      if (prevId.isEmpty) {
        // No parent specified - create a new chain starting with this input block
        val newChain = InputBlocksChain(ibi)
        val chains   = applyDisconnected(Seq(newChain))  // Process any disconnected blocks that can attach to the new chain
        log.debug(s"Created new input block chain for ${ibi.id}")
        Some(InputBlocksTree(forks ++ chains))
      } else {
        // Parent is specified - check if we know the parent block
        if (prevId.exists(id => knownInputBlocks.contains(id))) {
          // Parent is known, find the appropriate chain to insert into
          var processed = false  // Flag to ensure we only process one chain (avoid duplicates)
          val newForks = forks.flatMap { c =>
            if (!processed && c.chain.contains(prevId.get)) {
              // Found the chain that contains the parent block
              processed = true
              val forked = c.fork(ibi)  // Create fork if needed, or extend the chain
              applyDisconnected(forked)  // Process any disconnected blocks that can attach to the new fork(s)
            } else {
              Seq(c)  // Return the unchanged chain
            }
          }
          log.debug(s"Inserted input block ${ibi.id} into existing chain, now ${newForks.length} forks")
          Some(InputBlocksTree(newForks))
        } else {
          // Parent is unknown - add to disconnected waitlist for later processing
          log.debug(s"Input block ${ibi.id} has unknown parent ${prevId.get}, adding to disconnected waitlist")
          None
        }
      }
    }

    /**
     * Processes input block transactions, handling both linear progression and fork switching.
     *
     * This is the core algorithm for processing input block transactions, managing both
     * linear chain extension and fork switching scenarios. The method determines whether
     * to continue on the current best chain or switch to a longer competing chain.
     *
     * Algorithm:
     * 1. Determine if a fork switch is needed by comparing the longest chain with the best chain
     * 2. If a fork switch is needed:
     *    - Identify the common ancestor between current and new best chains
     *    - Rollback processed blocks from the old chain
      *    - Apply transactions from the new best chain
      * 3. If no fork switch is needed but the block belongs to the best chain:
      *    - Process the block on the current best chain
      * 4. Return the sequence of applied blocks and rolled back blocks
      *
      * Note: Sequential spending within the same input block is supported.
      * applyTransactions pre-populates createdOutputs with all outputs from the batch,
      * so transactions can spend outputs created by other transactions in the same block.
      *
      * @param ib The input block info to apply transactions to
     * @param txs The transactions to apply to the input block
     * @param state The current Ergo state for transaction validation
     * @return A tuple containing:
     *         - Sequence of new best input blocks applied (forward progress)
     *         - Sequence of input blocks rolled back (when switching forks)
     */
    def processInputBlockTransactions(
      ib: InputBlockAnnouncement,
      txs: Seq[ErgoTransaction],
      state: ErgoState[_]
    ): (Seq[ModifierId], Seq[ModifierId]) = {

      /**
       * Recursively applies transactions to an input block chain, continuing to process
       * subsequent blocks in the chain if they have available transactions.
       *
       * This tail-recursive helper function processes a chain of input blocks sequentially,
       * applying transactions to each block in order until no more blocks are available
       * or a failure occurs.
       *
       * @param ib The input block info to apply transactions to
       * @param txs The transactions to apply to the input block
       * @param acc A tuple containing:
       *           - The current input block chain being processed
       *           - A sequence of modifier IDs that have been processed so far
       * @return A tuple containing:
       *         - The updated input block chain after applying transactions
       *         - A sequence of modifier IDs representing all blocks that were processed
       *           in this application step (including the current block and any subsequent
       *           blocks that were also processed)
       */
      @tailrec
      def applicationStep(ib: InputBlockAnnouncement,
                          txs: Seq[ErgoTransaction],
                          acc: (InputBlocksChain, Seq[ModifierId])): (InputBlocksChain, Seq[ModifierId]) = {
        acc._1.applyTransactions(ib, txs, state) match {
          case Success(updChain) =>
            val res = (updChain -> (acc._2 ++ Seq(ib.id)))
            // Check if the next block in the chain has available transactions to process
            updChain.firstToComplete().filter(inputBlockTransactions.contains) match {
              case Some(nextId) =>
                // Continue processing the next block in the chain
                val nextIb = inputBlockRecords(nextId)
                val txIds = inputBlockTransactions(nextId)
                val txs = mutable.ArrayBuffer[ErgoTransaction]()
                cfor(0)(_ < txIds.length, _ + 1) { j =>
                  val tid = txIds(j)
                  val tx = transactionsCache.getIfPresent(tid)
                  if (tx != null) {
                    txs += tx
                  } else {
                    log.warn(s"Transaction $tid not found in cache during chain continuation (expired or evicted)")
                  }
                }
                log.debug(s"Continuing input block chain with $nextId")
                applicationStep(nextIb, txs, res)
              case _ =>
                // No more blocks to process in this chain
                log.debug(s"No more input blocks to process in chain after ${ib.id}")
                res
            }
          case Failure(e) =>
            log.warn(s"Application of input-block transactions failed for ${ib.id} : ", e)
            acc
        }
      }

      // Determine the best fork index (prefer processed blocks over longest chain)
      val bestIndex = if (this.bestIndex == -1) {
        this.longestIndex
      } else {
        this.bestIndex
      }
      if (bestIndex == -1) {
        log.debug("No best fork found, returning empty progress")
        return Seq.empty -> Seq.empty
      }

      /**
       * Determines if a fork switch is needed based on chain lengths and available transactions.
       *
       * A fork switch is needed when:
       * 1. The longest chain is different from the best chain
       * 2. The depth of the current block in the longest chain is greater than the best chain depth
       * 3. All blocks from the current processing point to the target depth have available transactions
       */
      def switchNeeded(id: ModifierId): Boolean = {
        val lf = forks(longestIndex)  // Get the longest fork
        val d  = lf.depthOf(id)      // Get the depth of the current block in the longest fork
        val needed = d > bestDepth && {  // Switch if longest fork is deeper than best fork
          // Verify that all blocks from current processing point to target depth have transactions
          (lf.processedIndex + 1 to d).forall { i =>
            val id = lf.chain(i)
            inputBlockTransactions.contains(id)  // Check if transactions are available
          }
        }
        if (needed) {
          log.info(s"Fork switch needed: longest fork depth $d > best fork depth ${bestDepth}")
        }
        needed
      }

      if (longestIndex != bestIndex && switchNeeded(ib.id)) { // forking case
        log.info(s"Performing fork switch from fork ${bestIndex} to fork ${longestIndex}")

        val currentFork = forks(bestIndex)  // Current best fork (to be abandoned)
        val newFork    = forks(longestIndex)  // New best fork (to be switched to)

        // Calculate which blocks need to be rolled back
        val rollbackInputBlocks = {
          val commonPrefixLength = currentFork.chain
            .zip(newFork.chain)
            .take(newFork.processedIndex + 1)
            .takeWhile { case (currentId, newId) => currentId == newId }
            .length
          val rolledBack =
            currentFork.chain.slice(commonPrefixLength, currentFork.processedIndex + 1)
          if (rolledBack.nonEmpty) {
            log.info(s"Fork switch: rolling back ${rolledBack.length} input blocks from fork ${bestIndex}")
          }
          rolledBack
        }

        // Process the next block in the new best chain
        val ibId = newFork.chain(newFork.processedIndex + 1)  // Next unprocessed block in new chain
        val ib   = inputBlockRecords(ibId)
        val txIds = inputBlockTransactions(ibId)
        val txs = mutable.ArrayBuffer[ErgoTransaction]()
        cfor(0)(_ < txIds.length, _ + 1) { j =>
          val tid = txIds(j)
          val tx = transactionsCache.getIfPresent(tid)
          if (tx != null) {
            txs += tx
          } else {
            log.warn(s"Transaction $tid not found in cache during fork switch (expired or evicted)")
          }
        }
        val r = applicationStep(ib, txs, (newFork -> Seq.empty))  // Process the block

        if (r._2.nonEmpty) {
          // Update the tree with the processed chain
          var updTree  = new InputBlocksTree(forks.updated(longestIndex, r._1))
          val updForks = updTree.forks

          // Register completion for any other forks that were waiting for this block
          (0 until updForks.length).foreach { idx =>
            val f = updForks(idx)
            if (f.firstToComplete().contains(ib.id)) {
              // todo: pass real cost of input block instead of costDelta = 0
              f.registerCompletion(ib.id, costDelta = 0) match {
                case Success(ibc) =>
                  updTree = new InputBlocksTree(forks.updated(idx, ibc))
                case Failure(e) =>
                  log.warn(s"registerCompletion failed for input block ${ib.id} : ", e)
              }
            }
          }
          inputBlockTrees.put(ib.header.parentId, updTree) // Update global tree storage
          log.info(s"Fork switch completed: ${r._2.length} blocks rolled back, new best fork has ${r._1.processedIndex + 1} processed blocks")
          r._2 -> rollbackInputBlocks  // Return forward progress and rollback blocks
        } else {
          log.warn("Progress is empty in processInputBlockTransactions during fork switch")
          Seq.empty -> Seq.empty
        }
      } else if (forks(bestIndex).firstToComplete().contains(ib.id)) { // no forking - linear processing
        log.debug(s"Processing input block ${ib.id} on best fork ${bestIndex}")
        val f = forks(bestIndex)
        val r = applicationStep(ib, txs, (f -> Seq.empty))  // Process the block on the current best chain

        if (r._2.nonEmpty) {
          // Update the tree with the processed chain
          var updTree  = new InputBlocksTree(forks.updated(bestIndex, r._1))
          val updForks = updTree.forks

          // Register completion for any other forks that were waiting for this block
          (0 until updForks.length).foreach { idx =>
            val f = updForks(idx)
            if (f.firstToComplete().contains(ib.id)) {
              // todo: pass real cost of input block instead of costDelta = 0
              f.registerCompletion(ib.id, costDelta = 0) match {
                case Success(ibc) =>
                  updTree = new InputBlocksTree(forks.updated(idx, ibc))
                case Failure(e) =>
                  log.warn(s"registerCompletion failed for input block ${ib.id} : ", e)
              }
            }
          }
          inputBlockTrees.put(ib.header.parentId, updTree) // Update global tree storage
          log.debug(s"Input block ${ib.id} processed successfully, ${r._2.length} blocks added to chain")
          r._2 -> Seq.empty  // Return forward progress, no rollback since no fork switch
        } else {
          log.warn("Progress is empty in processInputBlockTransactions during linear processing")
          Seq.empty -> Seq.empty
        }
      } else {
        log.debug(s"No forking and no non-forking for input block ${ib.id}, best depth: ${bestDepth}, longest depth: ${longestDepth.getOrElse(0)}")
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
  private val inputBlockRecords = mutable.Map[ModifierId, InputBlockAnnouncement]()

  /**
    * input block id -> input block transaction ids index
    */
  // todo: transactions can be put here without input block received, ie PoW and difficulty checked
  // todo: and they wont be cleared on pruning and the so structure can be DoSed. Fix by putting such transactions
  // todo: into a special queue
  private val inputBlockTransactions = mutable.Map[ModifierId, Seq[ModifierId]]()

  /**
    * txid -> transaction index
    *
    * We use Google Guava's cache with expiration, remove from cache after few ordering blocks of confirmation,
    * but in case of a transaction got into an input-blocks fork not confirmed by ordering blocks it can be stuck in
    * the cache till expiration (8 hours now)
    *
    * All cache accesses check for null results and log warnings if transactions are missing.
    */
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
  private[modifierprocessors] val disconnectedWaitlist = mutable.Set[InputBlockAnnouncement]()

  private def bestOrderingBlock(): Option[Header] = historyReader.bestFullBlockOpt.map(_.header)

  // extracts ordering block id from input block data provided
  private def extractOrderingId(ib: InputBlockAnnouncement) = ib.header.parentId

  private def inputBlockTransactionsDigest(transactions: Seq[ErgoTransaction]): Digest32 = {
    Algos.merkleTreeRoot(transactions.map(tx => LeafData @@ tx.serializedId))
  }

  private def transactionBodiesMatchAnnouncement(ib: InputBlockAnnouncement,
                                                 transactions: Seq[ErgoTransaction]): Boolean = {
    ib.inputBlockFields.inputBlockFieldsProof.indices.isEmpty ||
      inputBlockTransactionsDigest(transactions).sameElements(ib.inputBlockFields.transactionsDigest)
  }

  private def inputBlockDigestMatches(sbId: ModifierId,
                                      transactions: Seq[ErgoTransaction]): Boolean = {
    inputBlockRecords.get(sbId) match {
      case Some(ib) => transactionBodiesMatchAnnouncement(ib, transactions)
      case None => true
    }
  }

  /**
    * Gets the current best ordering block and best input block pair.
    *
    * This method returns the combination of the best known ordering block (full block)
    * and the corresponding best input block (transaction block) in the current view
    * of the blockchain state.
    *
    * @return A tuple containing:
    *         - Option[Header] for the best ordering block (if any exists)
    *         - Option[InputBlockAnnouncement] for the best input block (if any exists)
    */
  def bestBlocks: (Option[Header], Option[InputBlockAnnouncement]) = {
    val bestOrdering = bestOrderingBlock()
    val bestInputForOrdering =
      bestOrdering
        .map(_.id)
        .flatMap(inputBlockTrees.get)
        .flatMap(_.bestTip)
        .flatMap(inputBlockRecords.get)
    bestOrdering -> bestInputForOrdering
  }

  /**
   * Removes outdated input block data to free memory and maintain optimal performance.
   *
   * This pruning algorithm removes input block data that is considered too far behind
   * the current best chain height. It operates in two phases:
   * 1. Removes input block trees associated with ordering blocks that are behind the best chain
   * 2. Removes individual input blocks that are beyond the pruning threshold from the best height
   *
   * The pruning threshold is defined as 2 ordering blocks, meaning input blocks that are
   * more than 2 ordering blocks behind the current best chain will be removed.
   */
  private def prune(): Unit = {
    val bestHeight = bestBlocks._1.map(_.height).getOrElse(0)

    // Phase 1: Remove input block trees for ordering blocks that are behind the best chain
    val orderingBlockIdsToRemove = inputBlockTrees.keys.filter { orderingId =>
      // Remove if the ordering block height is behind the current best height
      bestHeight > historyReader.heightOf(orderingId).getOrElse(0)
    }.toSeq

    orderingBlockIdsToRemove.foreach { id =>
      inputBlockTrees.remove(id)
    }

    // Phase 2: Remove individual input blocks that are too far behind the best chain
    val inputBlockIdsToRemove = inputBlockRecords.flatMap {
      case (id, ibi) =>
        // Calculate if the input block is beyond the pruning threshold
        val res = (bestHeight - ibi.header.height) > PruningThreshold
        if (res) {
          Some(id)  // Mark for removal
        } else {
          None      // Keep the input block
        }
    }

    inputBlockIdsToRemove.foreach { id =>
      log.debug(s"Pruning input block # $id")
      // Remove from records and also clean up from disconnected waitlist if present
      inputBlockRecords.remove(id).foreach { ibi =>
        disconnectedWaitlist.remove(ibi)
      }
      // Also remove associated transaction data
      inputBlockTransactions.remove(id)
    }

    val OrderingBlockAnnouncementPruningThreshold = PruningThreshold * 3

    // Remove ordering block announcements that are stale or fully applied
    val announcementsToRemove = orderingBlockAnnouncements.collect {
      case (id, announcement) if
        (bestHeight - announcement.header.height) > OrderingBlockAnnouncementPruningThreshold ||
        historyReader.contains(announcement.header.transactionsId)
      => id
    }.toSeq

    announcementsToRemove.foreach { id =>
      orderingBlockAnnouncements.remove(id)
      log.debug(s"Pruned ordering block announcement: ${Algos.encode(id)}")
    }

    if (announcementsToRemove.nonEmpty) {
      log.debug(s"Pruned ${announcementsToRemove.size} ordering block announcements, best height: $bestHeight")
    }

  }

  // reset sub-blocks structures, should be called on receiving ordering block (or slightly later?)
  private def resetState(): Unit = {
    val oldTreeCount = inputBlockTrees.size
    val oldRecordCount = inputBlockRecords.size
    val oldTxCount = inputBlockTransactions.size
    val oldAnnouncementCount = orderingBlockAnnouncements.size

    prune()

    log.info(s"State reset: pruned ${oldTreeCount - inputBlockTrees.size} trees, " +
      s"${oldRecordCount - inputBlockRecords.size} records, " +
      s"${oldTxCount - inputBlockTransactions.size} transactions, " +
      s"${oldAnnouncementCount - orderingBlockAnnouncements.size} announcements")
  }

  /**
    * Updates input block related structures with a new input block received from a local miner or P2P network.
    *
    * This method integrates a new input block into the internal data structures, handling chain linking
    * and fork management. At this stage, input block transactions are typically not yet available,
    * so this method focuses on establishing the structural relationships between blocks.
    *
    * The method handles several scenarios:
    * - Creating new chains for input blocks that don't have parents
    * - Linking input blocks to existing chains
    * - Managing disconnected input blocks that reference unknown parents
    * - Performing state resets when significant height jumps are detected
    *
    * @param ib The input block information to be integrated
    * @return Option containing the ID of a parent input block to download if the current block
    *         references an unknown parent, or None if the block was successfully integrated
    */
  def applyInputBlock(ib: InputBlockAnnouncement): Option[ModifierId] = {
    val HeightThreshold = 2

    try {
      // Skip already known input blocks
      if (inputBlockRecords.contains(ib.id)) {
        log.debug(s"Input block ${ib.id} already known, skipping")
        return None
      }

      lazy val orderingId = extractOrderingId(ib)

      // if input-block corresponds to an ordering block @ better height, reset best input block reference
      // todo: make sure PoW and difficulty checked, to avoid low-diff block being sent in order to break input blocks chain
      if (ib.header.height > bestBlocks._1
        .map(_.height)
        .getOrElse(0) + HeightThreshold) {
        log.info(s"Resetting state due to height jump: input block height ${ib.header.height}, " +
          s"best ordering height ${bestBlocks._1.map(_.height).getOrElse(0)}")
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
            log.debug(s"Successfully added input block ${ib.id} to tree for ordering block $orderingId")
            None
          case None =>
            log.info(s"Put input block to disconnected queue: ${ib.id}")
            disconnectedWaitlist.add(ib)
            ib.prevInputBlockId
        }
      }

      inputBlockTrees.get(orderingId) match {
        case Some(tree) =>
          log.debug(s"Adding input block ${ib.id} to existing tree for ordering block $orderingId")
          updateTree(tree)
        case None =>
          log.debug(s"Creating new tree for input block ${ib.id} and ordering block $orderingId")
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
    * with transactions are received. The method performs transaction validation against the
    * current state, updates internal caches, and coordinates with the InputBlocksTree to
    * manage competing chain forks.
    *
    * Key responsibilities:
    * - Validates transactions against the current Ergo state
    * - Updates transaction caches and indexes
    * - Processes transactions through the InputBlocksTree structure
    * - Handles fork switching when a longer chain becomes available
    * - Maintains the relationship between ordering blocks and input blocks
    *
    * @param sbId The input block ID for which transactions are being applied
    * @param transactions The sequence of transactions contained in the input block
    * @param state The current Ergo state used for transaction validation
    * @return A tuple containing:
    *         - Sequence of new best input block IDs that were successfully applied (forward progress)
    *         - Sequence of input block IDs that were rolled back (when switching from one fork to another)
    */
  // todo: use PoEM to store only 2-3 best chains and select best one quickly
  def applyInputBlockTransactions(
    sbId: ModifierId,
    transactions: Seq[ErgoTransaction],
    state: ErgoState[_]
  ): (Seq[ModifierId], Seq[ModifierId]) = {

    try {
      log.info(s"Applying ${transactions.size} input block transactions for $sbId")
      if (!inputBlockDigestMatches(sbId, transactions)) {
        log.warn(s"Input block transactions digest does not match announcement for $sbId")
        return Seq.empty -> Seq.empty
      }

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
            log.debug(s"Skipping input block transactions for $sbId: ordering block $orderingId is not best")
            return Seq.empty -> Seq.empty
          }
          
          inputBlockTrees.get(orderingId) match {
            case Some(tree) =>
              log.debug(s"Processing input block transactions for $sbId in tree with ${tree.forks.length} forks")
              val (forward, rollback) = tree.processInputBlockTransactions(ib, transactions, state)
              log.info(s"Input block transaction processing completed: ${forward.length} forward, ${rollback.length} rollback")
              (forward, rollback)
            case None =>
              log.warn(s"No tree found for ordering block $orderingId when processing input block $sbId")
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

  /**
    * Updates the internal state when a new ordering block is received.
    *
    * This method handles the state transition when a new ordering block (full block) is processed,
    * triggering a state reset if the new block represents a height advancement. This ensures
    * that input block data is properly maintained relative to the current best ordering block.
    *
    * @param h The header of the new ordering block to update state with
    */
  def updateStateWithOrderingBlock(h: Header): Unit = {
    if (h.height >= bestOrderingBlock().map(_.height).getOrElse(-1)) {
      log.info(s"Updating state with new ordering block ${h.encodedId}, height: ${h.height}")
      resetState()
    }
  }

  // Getters to serve client requests below

  /**
    * Returns the best input block for the current best ordering block.
    *
    * @return the best input block information if available, None otherwise
    */
  def bestInputBlock(): Option[InputBlockAnnouncement] = {
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
    *
    * This method returns the sequence of input block IDs that form the best (most processed)
    * chain for the current best ordering block, ordered from tip to genesis.
    *
    * @return A sequence of modifier IDs representing the best input block chain, in reverse order (from tip to genesis)
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
    *
    * @param sbId The modifier ID of the input block to retrieve
    * @return Some(InputBlockAnnouncement) if the input block exists, None otherwise
    */
  def getInputBlock(sbId: ModifierId): Option[InputBlockAnnouncement] = {
    inputBlockRecords.get(sbId)
  }

  /**
    * Retrieves the transaction IDs contained in a specified input block.
    *
    * @param sbId The modifier ID of the input block to query
    * @return Some(sequence of transaction IDs) if the input block exists, None otherwise
    */
  def getInputBlockTransactionIds(sbId: ModifierId): Option[Seq[ModifierId]] = {
    inputBlockTransactions.get(sbId)
  }

  /**
    * Retrieves transactions for a specified input block.
    *
    * This method fetches the actual transaction objects associated with an input block
    * from the internal transaction cache.
    *
    * @param sbId The modifier ID of the input block to query
    * @return Some(sequence of ErgoTransaction objects) if the input block exists, None otherwise
    */
  def getInputBlockTransactions(sbId: ModifierId): Option[Seq[ErgoTransaction]] = {
    // todo: cache input block transactions to avoid recalculating it on every p2p request
    inputBlockTransactions.get(sbId).map { ids =>
      val result = new mutable.ArrayBuffer[ErgoTransaction](ids.length)
      cfor(0)(_ < ids.length, _ + 1) { i =>
        val tx = transactionsCache.getIfPresent(ids(i))
        if (tx != null) {
          result += tx
        } else {
          log.warn(s"Transaction ${ids(i)} not found in cache for input block $sbId (expired or evicted)")
        }
      }
      result
    }
  }

  private val orderingBlockAnnouncements = mutable.Map[ModifierId, OrderingBlockAnnouncement]()

  /**
    * Stores an ordering block announcement for later retrieval.
    *
    * @param announcement The ordering block announcement to store
    */
  def storeOrderingBlockAnnouncement(announcement: OrderingBlockAnnouncement): Unit = {
    val id = announcement.header.id
    orderingBlockAnnouncements.put(id, announcement)
  }

  /**
    * Retrieves an ordering block announcement by its ID.
    *
    * @param id The modifier ID of the ordering block announcement to retrieve
    * @return Some(OrderingBlockAnnouncement) if it exists, None otherwise
    */
  def getOrderingBlockAnnouncement(id: ModifierId): Option[OrderingBlockAnnouncement] = {
    orderingBlockAnnouncements.get(id)
  }

  /**
    * Retrieves specific transactions from an input block based on weak transaction IDs.
    *
    * This method filters the transactions in an input block to return only those that
    * match the provided weak transaction IDs.
    *
    * @param sbId The modifier ID of the input block to query
    * @param toFilter A sequence of weak transaction IDs to filter for
    * @return Some(sequence of matching ErgoTransaction objects) if the input block exists, None otherwise
    */
  def getInputBlockTransactions(sbId: ModifierId,
                                toFilter: Seq[ErgoTransaction.WeakId]): Option[Seq[ErgoTransaction]] = {
    // todo: cache input block transactions to avoid recalculating it on every p2p request
    inputBlockTransactions.get(sbId).map { ids =>
      val result = new mutable.ArrayBuffer[ErgoTransaction](ids.length)
      cfor(0)(_ < ids.length, _ + 1) { i =>
        val tx = transactionsCache.getIfPresent(ids(i))
        if (tx != null) {
          if (toFilter.exists(fId => tx.weakId.sameElements(fId))) {
            result += tx
          }
        } else {
          log.warn(s"Transaction ${ids(i)} not found in cache for filtered request (expired or evicted)")
        }
      }
      result
    }
  }

  /**
    * Retrieves the weak transaction IDs from a specified input block.
    *
    * Weak transaction IDs are compact representations of transaction IDs used for
    * efficient filtering and comparison operations.
    *
    * @param sbId The modifier ID of the input block to query
    * @return Some(sequence of weak transaction IDs) if the input block exists, None otherwise
    */
  def getInputBlockTransactionWeakIds(sbId: ModifierId): Option[Seq[ErgoTransaction.WeakId]] = {
    // todo: cache input block weak ids to avoid recalculating it on every p2p request
    inputBlockTransactions.get(sbId).map { ids =>
      val result = new mutable.ArrayBuffer[ErgoTransaction.WeakId](ids.length)
      cfor(0)(_ < ids.length, _ + 1) { i =>
        val tx = transactionsCache.getIfPresent(ids(i))
        if (tx != null) {
          result += tx.weakId
        } else {
          log.warn(s"Transaction ${ids(i)} not found in cache for weak ID lookup (expired or evicted)")
        }
      }
      result
    }
  }

  /**
    * Gets the tip input blocks for an ordering block at the best processing depth.
    *
    * This method returns the leaf nodes (tips) of all competing input block chains
    * that have reached the best processing depth for a given ordering block.
    *
    * @param id The modifier ID of the ordering block to query
    * @return Some(set of input block IDs that represent the tips) if the ordering block exists, None otherwise
    */
  def getOrderingBlockTips(id: ModifierId): Option[Set[ModifierId]] = {
    val treeOpt = inputBlockTrees.get(id)
    val bd      = treeOpt.map(_.bestDepth).getOrElse(-1)
    treeOpt.map(_.forks.filter(_.processedIndex == bd).flatMap(_.tip).toSet)
  }

  /**
    * Gets the processing depth of the best input block chain for an ordering block.
    *
    * @param id The modifier ID of the ordering block to query
    * @return The processing depth (number of processed blocks) of the best input block chain,
    *         or -1 if the ordering block is not found
    */
  def getOrderingBlockTipHeight(id: ModifierId): Int = {
    inputBlockTrees.get(id).map(_.bestDepth).getOrElse(-1)
  }

  /**
    * Gets the length of the longest input block chain for an ordering block.
    *
    * @param id The modifier ID of the ordering block to query
    * @return The length of the longest input block chain, or -1 if the ordering block is not found
    */
  def getLongestChainLength(id: ModifierId): Int = {
    inputBlockTrees.get(id).flatMap(_.longestDepth).getOrElse(-1)
  }

  /**
    * Gets transactions from the best input block chain for a specific ordering block.
    *
    * @param id The modifier ID of the ordering block to query
    * @return Some(sequence of transactions from the best input block chain) if the ordering block exists, None otherwise
    */
  def getCollectedInputBlocksTransactions(id: ModifierId): Option[Seq[ErgoTransaction]] = {
    inputBlockTrees
      .get(id)
      .map(_.bestChainTransactions)
  }

  /**
    * Gets all transactions from the best input block chain since the current best ordering block.
    *
    * This method retrieves all transactions that have been collected in the best input block chain
    * since the current best ordering block was established.
    *
    * @return A sequence of all transactions in the best input block chain since the current best ordering block
    */
  def getBestOrderingCollectedInputBlocksTransactions(): Seq[ErgoTransaction] = {
    bestOrderingBlock()
      .map(h => h.id)
      .flatMap(getCollectedInputBlocksTransactions)
      .getOrElse(Seq.empty)
  }

  /**
    * Saves transactions associated with an ordering block.
    *
    * @param orderingBlockId The modifier ID of the ordering block
    * @param transactions The sequence of transactions to associate with the ordering block
    * @return Some(previous sequence of transactions) if any existed, None otherwise
    */
  def saveOrderingBlockTransactions(orderingBlockId: ModifierId,
                                    transactions: Seq[ErgoTransaction]): Option[Seq[ErgoTransaction]] = {
    orderingBlockTransactions.put(orderingBlockId, transactions)
  }

  /**
    * Gets transactions associated with an ordering block.
    *
    * @param orderingBlockId The modifier ID of the ordering block to query
    * @return Some(sequence of transactions) if the ordering block exists, None otherwise
    */
  def getOrderingBlockTransactions(
    orderingBlockId: ModifierId
  ): Option[Seq[ErgoTransaction]] = {
    orderingBlockTransactions.get(orderingBlockId)
  }

}
