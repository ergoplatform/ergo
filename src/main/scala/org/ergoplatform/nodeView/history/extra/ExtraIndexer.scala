package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Stash, Timers}
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, ErgoApp, GlobalConstants, Pay2SAddress}
import org.ergoplatform.core.{VersionTag, idToVersion, versionToId}
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.ErgoNodeViewModifier
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{DigestFullBlockApplied, FullBlockApplied, RemoteBlockApplied, Rollback}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.history.extra.ExtraIndexer._
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages._
import org.ergoplatform.nodeView.history.extra.IndexedContractTemplateSerializer.hashTreeTemplate
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.uniqueId
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, ErgoStateReader}
import org.ergoplatform.settings.{Algos, CacheSettings, ChainSettings}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigma.ast.ErgoTree
import sigma.Extensions._
import sigma.interpreter.ProverResult

import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import spire.syntax.all.cfor

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.collection.concurrent
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/**
  * Base trait for extra indexer actor and its test.
  */
trait ExtraIndexerBase extends Actor with Stash with Timers with ScorexLogging {

  private case class RetryIndex(generation: Long)
  private case object RetryIndexTimerKey
  private case class RollbackToHeader(header: Header, resume: Boolean)
  private var retryGeneration: Long = 0L
  private var persistencePending: Boolean = false
  private var digestRefreshGeneration: Long = 0L
  private var digestRefreshDirty: Boolean = false

  private implicit val ec: ExecutionContextExecutor = context.dispatcher

  /**
    * Max buffer size (determined by config)
    */
  protected def saveLimit: Int

  /**
    * Number of transaction/box numeric indexes object segments contain
    */
  protected implicit val segmentThreshold: Int

  /**
    * Address encoder instance
    */
  protected implicit val addressEncoder: ErgoAddressEncoder

  /**
    * Database handle
    */
  protected var _history: ErgoHistory = _
  protected def chainHeight: Int = _history.fullBlockHeight

  protected def history: ErgoHistoryReader = _history.getReader

  protected def historyStorage: HistoryStorage = _history.historyStorage

  protected def fullChainHeaderAtHeight(height: Int): Option[Header] = {
    // No semantic-validity check: Unknown marks are transient (the best-chain marker can be
    // persisted before validation finishes) and Invalid modifiers are already filtered out by
    // typedModifierById. The marker does not say that the state has applied the block: history
    // sets it at append, for every block of a batch, before the state has looked at any of them,
    // and rolling back a block the state went on to reject is not guaranteed to restore the index.
    // Use this to compare the index with the best chain and to find rollback targets; a header
    // to index comes from appliedFullChainHeaderAtHeight.
    _history.headerIdsAtHeight(height)
      .find(id => FullBlockProcessor.isInBestFullChain(historyStorage, id))
      .flatMap(id => _history.typedModifierById[Header](id))
  }

  /**
    * Header up to which full bodies may be indexed. UTXO uses the state version at startup and
    * FullBlockApplied afterwards; Digest uses only its persisted contiguous-body certificate.
    */
  protected var lastAppliedHeaderOpt: Option[Header] = None

  protected var digestMode: Boolean = false

  /**
    * Applied tip the indexer may index up to. Overridden in tests which drive the indexer
    * over a history without a state.
    */
  protected def appliedTipOpt: Option[Header] = lastAppliedHeaderOpt

  private def appliedTipOnBestFullChain: Option[Header] =
    appliedTipOpt.filter(tip => FullBlockProcessor.isInBestFullChain(historyStorage, tip.id))

  /**
    * Height up to which the best full chain is applied: the height of the applied tip while
    * that tip is on the best full chain, 0 otherwise (nothing applied yet, or history has
    * switched to a branch of which the state has not reported a block yet). The indexer does
    * not go above it; the next `FullBlockApplied` sets it.
    */
  protected def appliedHeight: Int = appliedTipOnBestFullChain.fold(0)(_.height)

  /**
    * Best-full-chain header at the given height, if the state has applied its block, that is
    * if it is the applied tip or an ancestor of it. The applied tip is matched by id: during
    * a chain switch history marks the new branch as best at heights the state still holds
    * for the old one. No parent links are walked: the best-chain markers form a single chain,
    * so while the applied tip carries the marker every marked header at or below its height
    * is the tip or its ancestor. The markers are read under the history lock, which their
    * writers (`append`, `reportModifierIsInvalid`) hold, so a switch is never seen half-written
    * for the store; the index cache is filled by unlocked readers too.
    */
  private def appliedFullChainHeaderAtHeight(height: Int): Option[Header] = _history.synchronized {
    appliedTipOnBestFullChain.filter(_.height >= height).flatMap(_ => fullChainHeaderAtHeight(height))
  }

  protected def blockTransactionsForHeader(header: Header): Option[BlockTransactions] = {
    history.typedModifierById[BlockTransactions](header.transactionsId)
  }

  protected def retryDelay: FiniteDuration = 1.second

  private def scheduleRetry(): Unit = {
    if (!timers.isTimerActive(RetryIndexTimerKey)) {
      retryGeneration += 1
      timers.startSingleTimer(RetryIndexTimerKey, RetryIndex(retryGeneration), retryDelay)
    }
  }

  private def cancelRetry(): Unit = {
    retryGeneration += 1
    timers.cancel(RetryIndexTimerKey)
  }

  protected def resetTransientState(): Unit = {
    cancelRetry()
    blockCache.clear()
    readingUpTo = 0
  }

  /**
   * Used in tests to indicate the indexer has caught up to the chain
   */
  protected def caughtUpHook(height: Int = 0): Unit = {}

  protected def continueCatchUpAfterIndex(state: IndexerState): Boolean = true

  protected def stopIndexer(): Unit = context.stop(self)

  /** The production actor asks the holder for a paired view; test indexers do not use Digest mode. */
  protected def requestDigestAuthorityRefresh(generation: Long): Boolean = false

  /** Keep existing rows unreadable until a verified extra-index rebuild removes this marker. */
  protected def quarantineDigestIndex(reason: String): Unit = {
    historyStorage.insertExtraTry(
      Array(DigestFrontierQuarantineKey -> Array(1.toByte)),
      Array.empty[ExtraIndex]
    ) match {
      case Success(_) => log.error(s"Quarantined extra index: $reason")
      case Failure(error) =>
        log.error("Could not quarantine unsafe Digest extra index; shutting down node", error)
        ErgoApp.shutdownSystem()(context.system)
    }
    stopIndexer()
  }

  protected def removeRollbackIndexes(ids: Array[ModifierId]): Try[Unit] =
    historyStorage.removeExtraTry(ids)

  /**
   * Used in tests to get block for rollback, maybe orphan
   */
  protected def getLastTxForHeight(height: Int): ErgoTransaction = {
    fullChainHeaderAtHeight(height).flatMap(blockTransactionsForHeader).get.txs.last
  }

  // fast access buffers
  protected val general: ArrayBuffer[ExtraIndex] = ArrayBuffer.empty[ExtraIndex]
  protected val boxes: mutable.HashMap[ModifierId, IndexedErgoBox] = mutable.HashMap.empty[ModifierId, IndexedErgoBox]
  protected val trees: mutable.HashMap[ModifierId, IndexedErgoAddress] = mutable.HashMap.empty[ModifierId, IndexedErgoAddress]
  protected val templates: mutable.HashMap[ModifierId, IndexedContractTemplate] = mutable.HashMap.empty[ModifierId, IndexedContractTemplate]
  protected val tokens: mutable.HashMap[ModifierId, IndexedToken] = mutable.HashMap.empty[ModifierId, IndexedToken]
  protected val segments: mutable.HashMap[ModifierId, Segment[_]] = mutable.HashMap.empty[ModifierId, Segment[_]]

  /**
    * Whether the storage-rent eligibility index is maintained (`storageRentCollection`
    * setting). When off, no rent rows are written at all; the schema-version check in
    * `ErgoHistory.readOrGenerate` then also requires only the base schema, and turning
    * the flag on later forces a rescan which builds the rows from scratch.
    */
  protected val rentIndexEnabled: Boolean

  /**
    * Storage-rent eligibility index buffers: upserts and deletions keyed by the entry id
    * (the index key). A box created and spent within the same buffer window is removed from
    * the upserts only; a box created in an already-flushed batch gets a deletion marker.
    * Unused when [[rentIndexEnabled]] is off.
    */
  protected val rentBoxes: mutable.HashMap[ModifierId, StorageRentBox] = mutable.HashMap.empty[ModifierId, StorageRentBox]
  protected val rentBoxDeletes: mutable.HashSet[ModifierId] = mutable.HashSet.empty[ModifierId]

  /**
    * Input tokens in a transaction, cleared after every transaction
    */
  private val inputTokens: mutable.HashMap[Seq[Byte], Long] = mutable.HashMap.empty[Seq[Byte], Long]

  /**
    * Holds upcoming blocks to be indexed, and when empty, it is filled back from multiple threads
    */
  private val blockCache: concurrent.Map[Int, BlockTransactions] = new ConcurrentHashMap[Int, BlockTransactions]().asScala

  private[extra] final def putBlockTransactionsInCache(
    height: Int,
    transactions: BlockTransactions
  ): Unit =
    blockCache.put(height, transactions)

  private var readingUpTo: Int = 0

  /**
    * Get transactions for specified height, preferably from cache, or from database.
    * If indexer is getting close to emptying cache, asynchronously reads 1000 blocks into it
    *
    * @param height - blockheight to get transations from
    * @return transactions at height
    */
  private def getBlockTransactionsAt(height: Int, header: Header): Option[BlockTransactions] = {
    val cached = blockCache.remove(height)
    val txsOpt = cached.filter(_.headerId == header.id).orElse(blockTransactionsForHeader(header))

    txsOpt.map { txs =>
      if (height % 1000 == 0) blockCache.keySet.filter(_ < height).map(blockCache.remove)
      if (readingUpTo - height < 300 && chainHeight - height > 1000) {
        readingUpTo = math.min(height + 1001, chainHeight)

        if(height < history.fullBlockHeight - 1000) {
          val blockNums = height + 1 to readingUpTo by 250
          blockNums.zip(blockNums.tail).map { range => // ranges of 250 blocks for each thread to read
            Future {
              (range._1 until range._2).foreach { blockNum =>
                fullChainHeaderAtHeight(blockNum)
                  .flatMap(blockTransactionsForHeader)
                  .map(putBlockTransactionsInCache(blockNum, _))
              }
            }
          }
        } else {
          val blockNums = height + 1 to readingUpTo
          Future {
            blockNums.foreach { blockNum =>
              fullChainHeaderAtHeight(blockNum)
                .flatMap(blockTransactionsForHeader)
                .map(putBlockTransactionsInCache(blockNum, _))
            }
          }
        }
      }
      txs
    }
  }

  /**
    * Spend an IndexedErgoBox from buffer or database. Also record tokens for later use in balance tracking logic.
    *
    * @param id     - id of the wanted box
    * @param txId   - id of the spending transaction
    * @param height - height of the block the spending transaction is included in
    * @return whether the box was found in buffer or database -> should always return true
    */
  private def findAndSpendBox(id: ModifierId, txId: ModifierId, height: Int, spendingProof: ProverResult): Boolean = {
    def recordTokens(iEb: IndexedErgoBox): Unit =
      iEb.box.additionalTokens.toArray.map(x => x._1.toArray.toSeq -> x._2).foreach { case (k, v) =>
        inputTokens.put(k, inputTokens.getOrElse(k, 0L) + v)
      }
    // A spend recorded by another transaction is never overwritten: the block doing the
    // overwrite can still be rejected by the state, and rolling it back would otherwise
    // erase the original spend record.
    def alreadySpentElsewhere(iEb: IndexedErgoBox): Boolean = iEb.spendingTxIdOpt.exists(_ != txId)
    boxes.get(id) match {
      case Some(box) if !alreadySpentElsewhere(box) =>
        recordTokens(box.asSpent(txId, height, spendingProof))
        true
      case Some(box) =>
        log.warn(s"Box $id already spent by ${box.spendingTxIdOpt}, not spending it again in $txId")
        false
      case None =>
        history.typedExtraIndexById[IndexedErgoBox](id) match { // box not found in last saveLimit modifiers
          case Some(x) if !alreadySpentElsewhere(x) => // box found in DB, update
            val detached = detachedBox(x)
            recordTokens(detached.asSpent(txId, height, spendingProof))
            boxes.put(id, detached)
            true
          case Some(x) =>
            log.warn(s"Box $id already spent by ${x.spendingTxIdOpt}, not spending it again in $txId")
            false
          case None => // box not found at all (this shouldn't happen)
            log.warn(s"Unknown box used as input: $id")
            false
        }
    }
  }

  /**
    * Delete the storage-rent eligibility entry of a box being spent (see [[rentBoxes]]).
    */
  private def markRentBoxSpent(iEb: IndexedErgoBox): Unit = {
    if (rentIndexEnabled) {
      val srb = StorageRentBox(iEb)
      if (rentBoxes.remove(srb.id).isEmpty) {
        rentBoxDeletes += srb.id
      }
    }
  }

  /**
    * Add or subtract a box from an address in the buffer or in database.
    *
    * @param id             - hash of the (ergotree) address
    * @param spendOrReceive - IndexedErgoBox to receive (Right) or spend (Left)
    */
  private def findAndUpdateTree(id: ModifierId, spendOrReceive: Either[IndexedErgoBox, IndexedErgoBox])(state: IndexerState): Unit = {
    trees.get(id).map { tree =>
      spendOrReceive match {
        case Left(iEb) => tree.addTx(state.globalTxIndex).spendBox(iEb, Some(history)) // spend box
        case Right(iEb) => tree.addTx(state.globalTxIndex).addBox(iEb) // receive box
      }
      return
    }
    history.typedExtraIndexById[IndexedErgoAddress](id) match { // address not found in last saveLimit modifiers
      case Some(x) =>
        spendOrReceive match {
          case Left(iEb) => trees.put(id, x.addTx(state.globalTxIndex).spendBox(iEb, Some(history))) // spend box
          case Right(iEb) => trees.put(id, x.addTx(state.globalTxIndex).addBox(iEb)) // receive box
        }
      case None => // address not found at all
        spendOrReceive match {
          case Left(iEb) => log.error(s"Unknown address spent box ${bytesToId(iEb.box.id)}") // spend box should never happen by an unknown address
          case Right(iEb) => trees.put(id, IndexedErgoAddress(id).initBalance.addTx(state.globalTxIndex).addBox(iEb)) // receive box, new address
        }
    }
  }

  /**
    * Add or subtract a box from a token in the buffer or in database.
    *
    * @param id             - token id
    * @param spendOrReceive - IndexedErgoBox to receive (Right) or spend (Left)
    */
  private def findAndUpdateToken(id: ModifierId, spendOrReceive: Either[IndexedErgoBox, IndexedErgoBox]): Unit = {
    tokens.get(id).map { token =>
      spendOrReceive match {
        case Left(iEb) => token.spendBox(iEb, Some(history)) // spend box
        case Right(iEb) => token.addBox(iEb) // receive box
      }
      return
    }
    history.typedExtraIndexById[IndexedToken](uniqueId(id)) match { // token not found in last saveLimit modifiers
      case Some(x) =>
        spendOrReceive match {
          case Left(iEb) => tokens.put(id, x.spendBox(iEb, Some(history))) // spend box
          case Right(iEb) => tokens.put(id, x.addBox(iEb)) // receive box
        }
      case None => // token not found at all
        log.error(s"Unknown token $id") // spend box should never happen by an unknown token
    }
  }

  private def findAndUpdateTemplate(id: ModifierId, spendOrReceive: Either[IndexedErgoBox, IndexedErgoBox]): Unit = {
    templates.get(id).map { template =>
      spendOrReceive match {
        case Left(iEb) => template.spendBox(iEb, Some(history)) // spend box
        case Right(iEb) => template.addBox(iEb) // receive box
      }
      return
    }
    history.typedExtraIndexById[IndexedContractTemplate](id) match {
      case Some(x) =>
        spendOrReceive match {
          case Left(iEb) => templates.put(id, x.spendBox(iEb, Some(history))) // spend box
          case Right(iEb) => templates.put(id, x.addBox(iEb)) // receive box
        }
      case None => // template not found at all
        spendOrReceive match {
          case Left(iEb) => log.error(s"Unknown template spent box ${bytesToId(iEb.box.id)}") // spend box should never happen by an unknown template
          case Right(iEb) => templates.put(id, IndexedContractTemplate(id).addBox(iEb)) // receive box, new template
        }

    }
  }

  /**
    * @return number of indexes in all buffers
    */
  private def modCount: Int = general.length + boxes.size + trees.size + templates.size + tokens.size +
    rentBoxes.size + rentBoxDeletes.size

  // HistoryStorage caches boxes by reference. The indexer must not mutate that shared
  // persisted view while a newly applied block is still only in its local batch.
  private def detachedBox(box: IndexedErgoBox): IndexedErgoBox =
    new IndexedErgoBox(box.inclusionHeight, box.spendingTxIdOpt, box.spendingHeightOpt,
      box.spendingProofOpt, box.box, box.globalIndex)

  /**
    * Write buffered indexes to database and clear buffers.
    */
  private def saveProgress(state: IndexerState): Try[Unit] = Try {
    persistencePending = true
    val start: Long = System.currentTimeMillis

    // perform segmentation on big addresses and save their internal segment buffer
    trees.values.foreach { tree =>
      tree.buffer.values.foreach(seg => segments.put(seg.id, seg))
      tree.splitToSegments.foreach(seg => segments.put(seg.id, seg))
    }
    templates.values.foreach { template =>
      template.buffer.values.foreach(seg => segments.put(seg.id, seg))
      template.splitToSegments.foreach(seg => segments.put(seg.id, seg))
    }

    // perform segmentation on big tokens and save their internal segment buffer
    tokens.values.foreach { token =>
      token.buffer.values.foreach(seg => segments.put(seg.id, seg))
      token.splitToSegments.foreach(seg => segments.put(seg.id, seg))
    }

    val indexedHeaderEntry = state.indexedHeaderId.map { id =>
      IndexedHeaderIdKey -> fastIdToBytes(id)
    }.toArray
    val objects = ((((((general ++= boxes.values) ++= trees.values)
      ++= templates.values) ++= tokens.values) ++= segments.values) ++= rentBoxes.values).toArray
    historyStorage.insertExtraTry(
      Array(
        (IndexedHeightKey, ByteBuffer.allocate(4).putInt(state.indexedHeight).array),
        (GlobalTxIndexKey, ByteBuffer.allocate(8).putLong(state.globalTxIndex).array),
        (GlobalBoxIndexKey, ByteBuffer.allocate(8).putLong(state.globalBoxIndex).array),
        (RollbackToKey, ByteBuffer.allocate(4).putInt(state.rollbackTo).array)
      ) ++ indexedHeaderEntry,
      objects,
      // spent storage-rent rows are deleted in the same batch as the progress marker,
      // so a crash can not leave them behind an advanced checkpoint
      rentBoxDeletes.toArray
    ).recoverWith { case error =>
      historyStorage.invalidateExtraCache(objects.iterator.map(_.id).toSeq)
      Failure(error)
    }.get

    log.debug(s"Processed ${trees.size} ErgoTrees with ${boxes.size} boxes and inserted them to database in ${System.currentTimeMillis - start}ms")

    // clear buffers for next batch
    general.clear()
    boxes.clear()
    trees.clear()
    templates.clear()
    tokens.clear()
    segments.clear()
    rentBoxes.clear()
    rentBoxDeletes.clear()
    persistencePending = false
  }

  /**
    * Process a batch of BlockTransactions into memory and occasionally write them to database.
    *
    * @param state     - current indexer state
    * @param header       - exact full-chain header to index
    * @param targetHeight - applied height captured for this catch-up pass
    */
  protected def index(state: IndexerState,
                      header: Header,
                      targetHeight: Int): Option[IndexerState] = {
    val height = header.height
    val btOpt = getBlockTransactionsAt(height, header)

    if (btOpt.isEmpty) {
      log.error(s"Could not read block $height / $chainHeight from database, waiting for new block until retrying")
      return None
    }

    val txs: Seq[ErgoTransaction] = btOpt.get.txs

    var boxCount: Int = 0
    var newState: IndexerState = state

    // record transactions and boxes
    cfor(0)(_ < txs.length, _ + 1) { n =>

      val tx: ErgoTransaction = txs(n)
      val inputs: Array[Long] = Array.ofDim[Long](tx.inputs.length)
      val outputs: Array[Long] = Array.ofDim[Long](tx.outputs.length)

      inputTokens.clear()

      //process transaction inputs
      if (height > 1) { //only after 1st block (skip genesis box)
        cfor(0)(_ < tx.inputs.size, _ + 1) { i =>
          val boxId = bytesToId(tx.inputs(i).boxId)
          val spendingProof = tx.inputs(i).spendingProof
          if (findAndSpendBox(boxId, tx.id, height, spendingProof)) { // spend box and add tx
            val iEb = boxes(boxId)
            markRentBoxSpent(iEb)
            findAndUpdateTree(hashErgoTree(iEb.box.ergoTree), Left(iEb))(newState)
            findAndUpdateTemplate(hashTreeTemplate(iEb.box.ergoTree), Left(iEb))
              cfor(0)(_ < iEb.box.additionalTokens.length, _ + 1) { j =>
              findAndUpdateToken(iEb.box.additionalTokens(j)._1.toModifierId, Left(iEb))
            }
            inputs(i) = iEb.globalIndex
          } else {
            log.warn(s"Not found input box: $boxId")
          }
        }
      }

      //process transaction outputs
      cfor(0)(_ < tx.outputs.size, _ + 1) { i =>
        val iEb: IndexedErgoBox = new IndexedErgoBox(height, None, None, None, tx.outputs(i), newState.globalBoxIndex)
        boxes.put(iEb.id, iEb) // box by id
        general += NumericBoxIndex(newState.globalBoxIndex, iEb.id) // box id by global box number
        outputs(i) = iEb.globalIndex

        // unspent box by creation height (storage-rent eligibility index)
        if (rentIndexEnabled) {
          val srb = StorageRentBox(iEb)
          rentBoxes.put(srb.id, srb)
        }

        // box by address
        findAndUpdateTree(hashErgoTree(iEb.box.ergoTree), Right(boxes(iEb.id)))(newState)

        // box by template
        findAndUpdateTemplate(hashTreeTemplate(iEb.box.ergoTree), Right(boxes(iEb.id)))

        // check if box is creating new tokens, if yes record them
        cfor(0)(_ < iEb.box.additionalTokens.length, _ + 1) { j =>
          val idMatch = java.util.Arrays.equals(iEb.box.additionalTokens(j)._1.toArray, tx.inputs.head.boxId)
          if (idMatch && !inputTokens.contains(iEb.box.additionalTokens(j)._1.toArray.toSeq)) {
            val token = IndexedToken.fromBox(iEb, j)
            tokens.get(token.tokenId) match {
              case Some(t) => // same new token created in multiple boxes -> add amounts
                tokens.put(token.tokenId, t.addEmissionAmount(token.amount.get))
              case None => tokens.put(token.tokenId, token) // new token
            }
          }
          findAndUpdateToken(iEb.box.additionalTokens(j)._1.toModifierId, Right(iEb))
        }

        newState = newState.incrementBoxIndex
        boxCount += 1

      }

      //process transaction
      general += IndexedErgoTransaction.fromTx(tx, n, height, newState.globalTxIndex, inputs, outputs, header.id)
      general += NumericTxIndex(newState.globalTxIndex, tx.id)

      newState = newState.incrementTxIndex

    }

    log.info(s"Buffered block $height / $chainHeight [txs: ${txs.length}, boxes: $boxCount] (buffer: $modCount / $saveLimit)")

    Some(newState.copy(
      caughtUp = newState.indexedHeight == targetHeight,
      indexedHeaderId = Some(header.id)
    ))
  }

  /**
    * Remove all indexes after a given height and revert address balances.
    *
    * @param state  - current state of indexer
    * @param height - forking height (height of last common block)
    */
  private def removeAfter(state: IndexerState, targetHeader: Header): Try[IndexerState] = Try {

    var newState: IndexerState = state
    val height = targetHeader.height

    saveProgress(newState).get
    log.info(s"Rolling back indexes from ${state.indexedHeight} to $height")

      val lastTxToKeep: ErgoTransaction = blockTransactionsForHeader(targetHeader).get.txs.last
      val txTarget: Long = history.typedExtraIndexById[IndexedErgoTransaction](lastTxToKeep.id).get.globalIndex
      val boxTarget: Long = history.typedExtraIndexById[IndexedErgoBox](bytesToId(lastTxToKeep.outputs.last.id)).get.globalIndex
      val toRemove: ArrayBuffer[ModifierId] = ArrayBuffer.empty[ModifierId]

      // remove all tx indexes
      newState = newState.decrementTxIndex
      while (newState.globalTxIndex > txTarget) {
        val tx: IndexedErgoTransaction = NumericTxIndex.getTxByNumber(history, newState.globalTxIndex).get
        // undo only spendings recorded by this transaction: inputs the index never resolved
        // are stored as 0, and a rejected block can also reference boxes spent by other,
        // valid transactions - unspending those would resurrect boxes this block never spent
        tx.inputNums.flatMap(NumericBoxIndex.getBoxByNumber(history, _))
          .filter(_.spendingTxIdOpt.contains(tx.id))
          .map(detachedBox)
          .foreach { iEb => // undo spendings

          iEb.spendingHeightOpt = None
          iEb.spendingTxIdOpt = None
          iEb.spendingProofOpt = None

          val address = history.typedExtraIndexById[IndexedErgoAddress](hashErgoTree(iEb.box.ergoTree)).get.addBox(iEb, record = false)
          address.findAndModBox(iEb.globalIndex, history)

          val template = history.typedExtraIndexById[IndexedContractTemplate](hashTreeTemplate(iEb.box.ergoTree)).get
          template.findAndModBox(iEb.globalIndex, history)

          val rentRow: Array[ExtraIndex] =
            if (rentIndexEnabled) Array[ExtraIndex](StorageRentBox(iEb)) else Array.empty
          historyStorage.insertExtraTry(Array.empty, Array[ExtraIndex](iEb, address, template) ++ rentRow ++ address.buffer.values ++ template.buffer.values).get

          cfor(0)(_ < iEb.box.additionalTokens.length, _ + 1) { i =>
            history.typedExtraIndexById[IndexedToken](IndexedToken.fromBox(iEb, i).id).map { token =>
              token.findAndModBox(iEb.globalIndex, history)
              historyStorage.insertExtraTry(Array.empty, Array[ExtraIndex](token) ++ token.buffer.values).get
            }
          }
        }
        toRemove += tx.id // tx by id
        toRemove += bytesToId(NumericTxIndex.indexToBytes(newState.globalTxIndex)) // tx id by number
        newState = newState.decrementTxIndex
      }
      newState = newState.incrementTxIndex

      // remove all box indexes, tokens and address balances
      newState = newState.decrementBoxIndex
      while (newState.globalBoxIndex > boxTarget) {
        val iEb: IndexedErgoBox = detachedBox(NumericBoxIndex.getBoxByNumber(history, newState.globalBoxIndex).get)
        cfor(0)(_ < iEb.box.additionalTokens.length, _ + 1) { i =>
          history.typedExtraIndexById[IndexedToken](IndexedToken.fromBox(iEb, i).id).map { token =>
            if (token.boxId.get == iEb.id) { // token created, delete
              toRemove += token.id
              log.info(s"Removing token ${token.tokenId} created in box ${iEb.id} at height ${iEb.inclusionHeight}")
            } else // no token created, update
              toRemove ++= token.rollback(txTarget, boxTarget, _history)
          }
        }
        history.typedExtraIndexById[IndexedErgoAddress](hashErgoTree(iEb.box.ergoTree)).map { address =>
          address.spendBox(iEb)
          toRemove ++= address.rollback(txTarget, boxTarget, _history)
        }
        history.typedExtraIndexById[IndexedContractTemplate](hashTreeTemplate(iEb.box.ergoTree)).map { template =>
          template.spendBox(iEb)
          toRemove ++= template.rollback(txTarget, boxTarget, _history)
        }
        toRemove += iEb.id // box by id
        toRemove += bytesToId(NumericBoxIndex.indexToBytes(newState.globalBoxIndex)) // box id by number
        if (rentIndexEnabled) toRemove += StorageRentBox(iEb).id // unspent box by creation height
        newState = newState.decrementBoxIndex
      }
      newState = newState.incrementBoxIndex

      // Save changes
      val completedState = newState.copy(
        indexedHeight = height,
        rollbackTo = 0,
        caughtUp = height >= appliedHeight && fullChainHeaderAtHeight(height).exists(_.id == targetHeader.id),
        indexedHeaderId = Some(targetHeader.id)
      )
      removeRollbackIndexes(toRemove.toArray).get
      saveProgress(completedState).get
      completedState
  }

  private def indexedTipIsOnBestFullChain(state: IndexerState): Boolean = {
    state.indexedHeight == 0 ||
      (chainHeight >= state.indexedHeight &&
        state.indexedHeaderId == fullChainHeaderAtHeight(state.indexedHeight).map(_.id))
  }

  private def reconcileIndexedTip(state: IndexerState): Boolean = {
    // A reorg can invalidate both the indexed tip and its parents. Walk stored
    // headers because the normal history traversal hides invalid headers.
    @scala.annotation.tailrec
    def selectedIndexedAncestor(id: ModifierId, height: Int): Option[Header] = {
      historyStorage.modifierById(id) match {
        case Some(header: Header) if height > 0 && header.id == id && header.height == height =>
          if (fullChainHeaderAtHeight(height).exists(_.id == id)) Some(header)
          else if (height > 1) selectedIndexedAncestor(header.parentId, height - 1)
          else None
        case _ => None
      }
    }

    val rollbackHeaderOpt = state.indexedHeaderId
      .flatMap(id => selectedIndexedAncestor(id, state.indexedHeight))
      .filter(_.height < state.indexedHeight)

    rollbackHeaderOpt.exists { branchHeader =>
      beginRollback(state, branchHeader)
      true
    }
  }

  protected def beginRollback(state: IndexerState, targetHeader: Header, resume: Boolean = true): Unit = {
    resetTransientState()
    context.become(receive.orElse(loaded(state.copy(caughtUp = false, rollbackTo = targetHeader.height))))
    self ! RollbackToHeader(targetHeader, resume)
  }

  private def persistBuffered(state: IndexerState): Boolean = {
    saveProgress(state) match {
      case Success(_) => true
      case Failure(error) =>
        log.error(s"Failed to persist extra indexes at height ${state.indexedHeight}; retrying", error)
        scheduleRetry()
        false
    }
  }

  /**
    * Record the appropriate state-mode authority before handling an application event. Digest
    * never treats a generic FullBlockApplied header as a contiguous-body certificate.
    */
  protected def loaded(state: IndexerState): Receive = {
    val behavior = loadedBehavior(state)
    val recordingAppliedTip: Receive = {
      case _: FullBlockApplied if digestMode =>
      case applied: FullBlockApplied =>
        lastAppliedHeaderOpt = Some(applied.header)
        behavior(applied)
      case _: DigestFullBlockApplied if digestMode && state.rollbackInProgress => stash()
      case _: DigestFullBlockApplied if digestMode => suspendForDigestRefresh(state)
      case _: DigestFullBlockApplied =>
      case _: Rollback if digestMode && state.rollbackInProgress => stash()
      case _: Rollback if digestMode => suspendForDigestRefresh(state)
      case other =>
        behavior(other)
    }
    recordingAppliedTip
  }

  private def suspendForDigestRefresh(state: IndexerState): Unit = {
    cancelRetry()
    beginDigestRefresh(state, 0)
  }

  private def beginDigestRefresh(state: IndexerState, retryCount: Int): Unit = {
    digestRefreshGeneration += 1
    digestRefreshDirty = false
    val generation = digestRefreshGeneration
    context.become(awaitingDigestRefresh(state, generation, retryCount))
    if (!requestDigestAuthorityRefresh(generation)) {
      log.error("Cannot refresh Digest index authority without a node-view holder")
      stopIndexer()
    }
  }

  private def awaitingDigestRefresh(state: IndexerState,
                                    generation: Long,
                                    retryCount: Int): Receive = {
    case GetSegmentThreshold => sender() ! segmentThreshold
    case _: DigestFullBlockApplied | _: Rollback => digestRefreshDirty = true
    case DigestAuthorityRefreshed(`generation`, freshHistory, snapshotOpt) =>
      if (digestRefreshDirty) beginDigestRefresh(state, 0)
      else if (freshHistory ne _history) {
        log.error("Digest history handle changed while refreshing extra-index authority; stopping indexer")
        stopIndexer()
      } else if (digestFrontierQuarantined(freshHistory)) {
        log.error("Extra index became quarantined while refreshing Digest authority")
        stopIndexer()
      } else if (!digestIndexHasProvenance(freshHistory)) {
        quarantineDigestIndex("Digest index provenance disappeared during authority refresh")
      } else {
        val persisted = IndexerState.fromHistory(freshHistory)
        val authorityOpt = snapshotOpt.flatMap(digestIndexAuthority(freshHistory, _))
        val verificationDisabled = snapshotOpt.exists(s => !s.verifyTransactions)
        authorityOpt match {
          case Some(authority) =>
            val frontierHeight = authority.frontier.fold(0)(_.height)
            if (!checkpointOnSelectedFullChain(freshHistory)) {
              quarantineDigestIndex("persisted Digest checkpoint is no longer on the selected full chain")
            } else if (persisted.indexedHeight > frontierHeight) {
              quarantineDigestIndex(s"persisted indexed height ${persisted.indexedHeight} exceeds " +
                s"fresh Digest body frontier $frontierHeight")
            } else if (state.indexedHeight > frontierHeight || !indexedTipIsOnBestFullChain(state)) {
              // Buffered rows may belong to a different block at the same height. They must
              // never be flushed under a fresh certificate for another selected branch.
              quarantineDigestIndex(s"buffered indexed tip ${state.indexedHeaderId} at " +
                s"height ${state.indexedHeight} is outside the fresh Digest body frontier")
            } else {
              lastAppliedHeaderOpt = authority.frontier
              context.become(receive.orElse(loaded(state.copy(caughtUp = false))))
              unstashAll()
              self ! Index()
            }
          case None if verificationDisabled =>
            log.error("Digest transaction verification disabled; stopping extra indexer")
            stopIndexer()
          case None if !verificationDisabled && retryCount == 0 =>
            beginDigestRefresh(state, retryCount + 1)
          case None =>
            log.error("Digest state has no coherent selected-chain body authority; stopping extra indexer")
            stopIndexer()
        }
      }
    case _: DigestAuthorityRefreshed => // superseded holder response
    case _: FullBlockApplied | _: RemoteBlockApplied | Index() | _: RetryIndex =>
    case _ => stash()
  }

  private def loadedBehavior(state: IndexerState): Receive = {

    // Segmentation mutates parent references before persistence. Flush those rows
    // before either event-driven or self-driven indexing can consume them.
    case _: FullBlockApplied if persistencePending && !state.rollbackInProgress =>
      cancelRetry()
      context.become(receive.orElse(loaded(state.copy(caughtUp = false))))
      if (persistBuffered(state)) self ! Index()

    case Index() if persistencePending && !state.rollbackInProgress =>
      cancelRetry()
      context.become(receive.orElse(loaded(state.copy(caughtUp = false))))
      if (persistBuffered(state)) self ! Index()

    case Index() if !state.caughtUp && !state.rollbackInProgress =>
      cancelRetry()
      if (modCount < saveLimit || persistBuffered(state)) {
        // blocks above the applied height are not indexed: having reached it the indexer
        // waits, as caught up, for the next FullBlockApplied
        val targetHeight = appliedHeight
        if (state.indexedHeight >= targetHeight && indexedTipIsOnBestFullChain(state)) {
          val newState = state.copy(caughtUp = true)
          context.become(receive.orElse(loaded(newState)))
          self ! Index()
        } else {
          val nextHeaderOpt = appliedFullChainHeaderAtHeight(state.indexedHeight + 1)
          val extendsIndexedTip = nextHeaderOpt.forall { header =>
            state.indexedHeight == 0 || state.indexedHeaderId.contains(header.parentId)
          }
          if (extendsIndexedTip && nextHeaderOpt.isDefined) {
            index(state.incrementIndexedHeight, nextHeaderOpt.get, targetHeight) match {
              case Some(newState) =>
                context.become(receive.orElse(loaded(newState)))
                if (continueCatchUpAfterIndex(newState)) self ! Index()
              case None =>
                scheduleRetry()
            }
          } else if (!reconcileIndexedTip(state)) {
            log.info("Deferring catch-up because the next full-chain header does not extend the indexed tip")
            scheduleRetry()
          }
        }
      }

    case Index() if state.caughtUp && !state.rollbackInProgress && !indexedTipIsOnBestFullChain(state) =>
      if (!reconcileIndexedTip(state)) {
        val newState = state.copy(caughtUp = false)
        context.become(receive.orElse(loaded(newState)))
        scheduleRetry()
      }

    case Index() if state.caughtUp && !state.rollbackInProgress =>
      cancelRetry()
      if (modCount == 0 || persistBuffered(state)) {
        blockCache.clear()
        caughtUpHook()
        log.info(s"Indexer is at the applied tip: indexed ${state.indexedHeight}, best full block $chainHeight")
      }

    case Index() if state.rollbackInProgress =>

    // after the indexer caught up with the chain, stay up to date
    case FullBlockApplied(header: Header) if state.caughtUp && !state.rollbackInProgress =>
      val indexedTipStillBest = indexedTipIsOnBestFullChain(state)
      val isDirectSuccessor = header.height == state.indexedHeight + 1 &&
        (state.indexedHeight == 0 || state.indexedHeaderId.contains(header.parentId)) &&
        fullChainHeaderAtHeight(header.height).exists(_.id == header.id)

      if (isDirectSuccessor) {
        cancelRetry()
        // the block of this event is applied, blocks history holds above it may not be
        val targetHeight = appliedHeight
        index(state.incrementIndexedHeight, header, targetHeight) match {
          case Some(newState) =>
            context.become(receive.orElse(loaded(newState)))
            if (newState.caughtUp) {
              // While history holds blocks above this one further events are likely to follow:
              // the block stays buffered, as during catch-up, and the retry timer writes the
              // buffer if no event arrives.
              if (chainHeight <= header.height || modCount >= saveLimit) {
                if (persistBuffered(newState)) caughtUpHook(header.height)
              } else {
                scheduleRetry()
              }
            } else {
              self ! Index()
            }
          case None =>
            val newState = state.copy(caughtUp = false)
            context.become(receive.orElse(loaded(newState)))
            scheduleRetry()
        }
      } else if (!indexedTipStillBest) {
        log.info(s"Reconciling indexed tip before applying block ${header.id} at height ${header.height}")
        if (!reconcileIndexedTip(state)) {
          context.become(receive.orElse(loaded(state.copy(caughtUp = false))))
          scheduleRetry()
        }
      } else if (header.height > state.indexedHeight + 1) {
        context.become(receive.orElse(loaded(state.copy(caughtUp = false))))
        self ! Index()
      } else {
        log.warn(s"Skipping block ${header.id} applied at height ${header.height}, indexed height is ${state.indexedHeight}")
      }

    case _: FullBlockApplied if !state.rollbackInProgress =>
      self ! Index()

    case _: FullBlockApplied if state.rollbackInProgress => stash()

    case Rollback(branchPoint: ModifierId) =>
      cancelRetry()
      if (state.rollbackInProgress) {
        log.warn(s"Rollback already in progress")
        stash()
      } else if (indexedTipIsOnBestFullChain(state)) {
        log.info(s"Ignoring rollback to $branchPoint because the indexed tip is already on the best full chain")
        // the retry timer cancelled above may have been the one due to write buffered blocks
        if (!state.caughtUp || modCount > 0) self ! Index()
      } else if (!reconcileIndexedTip(state)) {
        log.info(s"Deferring rollback to $branchPoint until the indexed tip can be reconciled with the best full chain")
        val newState = state.copy(caughtUp = false, rollbackTo = 0)
        context.become(receive.orElse(loaded(newState)))
        scheduleRetry()
      }

    case RollbackToHeader(targetHeader, resume)
      if state.rollbackInProgress && state.rollbackTo == targetHeader.height =>
      blockCache.clear()
      readingUpTo = 0
      removeAfter(state, targetHeader) match {
        case Success(newState) =>
          context.become(receive.orElse(loaded(newState)))
          if (resume && !newState.caughtUp) self ! Index()
          caughtUpHook()
          log.info(s"Successfully rolled back indexes to ${targetHeader.height}")
          unstashAll()
        case Failure(error) =>
          log.error(s"Failed to roll back extra indexes to ${targetHeader.height}; stopping extra indexer until node restart", error)
          stopIndexer()
      }

    case RollbackToHeader(_, _) =>

    case RetryIndex(generation) if generation == retryGeneration && !state.rollbackInProgress =>
      self ! Index()

    case RetryIndex(_) =>

    case RemoveAfter(branchHeight) =>
      log.warn(s"Ignoring unsupported direct extra-index rollback request to height $branchHeight")

    case GetSegmentThreshold =>
      sender ! segmentThreshold

    case _ =>

  }

}


/**
  * Actor that constructs an index of database elements.
  *
  * @param cacheSettings     - cacheSettings to use for saveLimit size
  * @param ae                - ergo address encoder to use for handling addresses
  * @param rentIndexEnabled  - whether to maintain the storage-rent eligibility index
  *                            (`storageRentCollection` setting)
  */
class ExtraIndexer(cacheSettings: CacheSettings,
                   ae: ErgoAddressEncoder,
                   nodeViewHolderRefOpt: Option[ActorRef],
                   override protected val rentIndexEnabled: Boolean)
  extends ExtraIndexerBase {

  override val saveLimit: Int = cacheSettings.history.extraCacheSize * 20

  override implicit val segmentThreshold: Int = 512

  override implicit val addressEncoder: ErgoAddressEncoder = ae

  private var startupSnapshotRetried = false

  private def requestCurrentView(): Unit = nodeViewHolderRefOpt.foreach { nodeViewHolderRef =>
    nodeViewHolderRef ! GetDataFromCurrentView[ErgoStateReader, StartExtraIndexer] { view =>
      view.state match {
        case digest: DigestState =>
          StartExtraIndexer(view.history, None, digestMode = true,
            digestSnapshotOpt = ExtraIndexer.digestIndexSnapshot(
              digest, digest.ergoSettings.nodeSettings.verifyTransactions))
        case state =>
          StartExtraIndexer(view.history, Some(state.version))
      }
    }
  }

  override protected def requestDigestAuthorityRefresh(generation: Long): Boolean =
    nodeViewHolderRefOpt.exists { nodeViewHolderRef =>
      nodeViewHolderRef ! GetDataFromCurrentView[ErgoStateReader, DigestAuthorityRefreshed] { view =>
        val snapshotOpt = view.state match {
          case digest: DigestState => ExtraIndexer.digestIndexSnapshot(
            digest, digest.ergoSettings.nodeSettings.verifyTransactions)
          case _ => None
        }
        DigestAuthorityRefreshed(generation, view.history, snapshotOpt)
      }
      true
    }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[FullBlockApplied])
    context.system.eventStream.subscribe(self, classOf[DigestFullBlockApplied])
    context.system.eventStream.subscribe(self, classOf[Rollback])
    requestCurrentView()
  }

  override def postStop(): Unit = {
    log.error(s"Stopped extra indexer")
    super.postStop()
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(s"Attempted extra indexer restart due to ${reason.getMessage} ", reason)
    super.preRestart(reason, message)
  }

  override def receive: Receive = {

    case GetSegmentThreshold => sender ! segmentThreshold

    case _: FullBlockApplied if _history == null => stash()

    case _: DigestFullBlockApplied if _history == null => stash()

    case _: Rollback if _history == null => stash()

    case StartExtraIndexer(history: ErgoHistory, stateVersionOpt, isDigestMode, digestSnapshotOpt) =>
      log.info(s"Starting extra indexer")
      digestMode = isDigestMode
      val digestAuthorityOpt = if (digestMode) {
        digestSnapshotOpt.flatMap(ExtraIndexer.digestIndexAuthority(history, _))
      } else None
      // At genesis there is no header id. An absent Digest certificate also admits no body.
      lastAppliedHeaderOpt = if (digestMode) digestAuthorityOpt.flatMap(_.frontier) else {
        stateVersionOpt.map(versionToId).flatMap(id => history.typedModifierById[Header](id))
      }
      val state = IndexerState.fromHistory(history)
      val quarantined = ExtraIndexer.digestFrontierQuarantined(history)
      val modeProvenanceValid = ExtraIndexer.indexModeHasProvenance(history, digestMode)
      val missingDigestProvenance = digestMode && !modeProvenanceValid
      val runtimeModeMismatch = !digestMode && !modeProvenanceValid
      val invalidDigestAuthority = digestMode && digestAuthorityOpt.isEmpty
      val verificationDisabled = digestSnapshotOpt.exists(s => !s.verifyTransactions)
      val missingDigestCertificate = digestMode && digestAuthorityOpt.nonEmpty &&
        state.indexedHeight > 0 && lastAppliedHeaderOpt.isEmpty
      val certifiedHeight = lastAppliedHeaderOpt.fold(0)(_.height)
      val beyondDigestFrontier = digestMode && digestAuthorityOpt.nonEmpty &&
        state.indexedHeight > certifiedHeight
      val invalidDigestCheckpoint = digestMode && !ExtraIndexer.checkpointOnSelectedFullChain(history)
      if (runtimeModeMismatch) {
        log.error("Extra index provenance does not match the runtime UTXO state; stopping indexer")
        stopIndexer()
      } else if (quarantined || missingDigestProvenance || missingDigestCertificate ||
          beyondDigestFrontier || invalidDigestCheckpoint) {
        _history = history
        if (!quarantined) {
          quarantineDigestIndex(s"Digest provenance present = ${!missingDigestProvenance}; " +
            s"paired state valid = ${!invalidDigestAuthority}; indexed height ${state.indexedHeight}, " +
            s"selected body frontier $certifiedHeight at startup")
        } else {
          log.error("Extra index remains quarantined until a verified rebuild")
          stopIndexer()
        }
      } else if (verificationDisabled) {
        log.error("Digest transaction verification disabled; stopping extra indexer")
        stopIndexer()
      } else if (invalidDigestAuthority) {
        if (!verificationDisabled && !startupSnapshotRetried && nodeViewHolderRefOpt.nonEmpty) {
          startupSnapshotRetried = true
          log.warn("Digest state and selected history moved during extra-index startup; retrying current view once")
          requestCurrentView()
        } else {
          log.error("Digest state has no coherent selected-chain body authority; stopping extra indexer")
          stopIndexer()
        }
      } else {
        _history = history
        context.become(receive.orElse(loaded(state)))
        log.info(s"Started extra indexer at height ${state.indexedHeight}, applied tip is ${lastAppliedHeaderOpt.map(h => s"${h.id} at height ${h.height}")}")
        self ! Index()
        unstashAll()
      }

  }
}

object ExtraIndexer {

  type ExtraIndexTypeId = Byte

  object ReceivableMessages {
    /**
      * Initialize ExtraIndexer and start indexing.
      *
      * @param history         - handle to database
      * @param stateVersionOpt - UTXO state version, taken together with `history`.
      * @param digestMode      - require Digest certificate events instead of generic block events
      * @param digestSnapshotOpt - paired Digest state snapshot captured while the holder owns the view
      */
    private[extra] case class StartExtraIndexer(history: ErgoHistory,
                                                stateVersionOpt: Option[VersionTag],
                                                digestMode: Boolean = false,
                                                digestSnapshotOpt: Option[DigestIndexSnapshot] = None)

    private[extra] case class DigestAuthorityRefreshed(generation: Long,
                                                       history: ErgoHistory,
                                                       snapshotOpt: Option[DigestIndexSnapshot])

    /**
      * Retreive the currently used segment treshold
      */
    case class GetSegmentThreshold()

    /**
      * Index block at current indexer height
      */
    case class Index()

    /**
      * Remove and roll back all indexes after branchHeight
      *
      * @param branchHeight - height of last block to keep
      */
    case class RemoveAfter(branchHeight: Int)
  }

  /**
    * @return address constructed from the ErgoTree of this box
    */
  def getAddress(tree: ErgoTree)(implicit ae: ErgoAddressEncoder): ErgoAddress =
    tree.root match {
      case Right(_) => ae.fromProposition(tree).get // default most of the time
      case Left(_) => new Pay2SAddress(tree, tree.bytes) // needed for burn address 4MQyMKvMbnCJG3aJ
    }

  private val hexIndex: Array[Byte] = {
    val index = Array.fill[Byte](128)(0xff.toByte)
    "0123456789abcdef".toCharArray.zipWithIndex.foreach { case (c, i) =>
      index(c) = i.toByte
    }
    "abcdef".toCharArray.foreach { c =>
      index(c.toUpper) = index(c)
    }
    index
  }

  /**
    * Faster id to bytes - no safety checks
    *
    * @param id - ModifierId to convert to byte representation
    * @return an array of bytes
    */
  private[extra] def fastIdToBytes(id: ModifierId): Array[Byte] = {
    val x: Array[Byte] = new Array[Byte](id.length / 2)
    cfor(0)(_ < id.length, _ + 2) { i => x(i / 2) = ((hexIndex(id(i)) << 4) | hexIndex(id(i + 1))).toByte }
    x
  }

  /**
    * Current newest database schema version. Used to force extra database resync.
    */
  val NewestVersion: Int = 7
  val NewestVersionBytes: Array[Byte] = ByteBuffer.allocate(4).putInt(NewestVersion).array

  val IndexedHeightKey: Array[Byte] = Algos.hash("indexed height")
  val GlobalTxIndexKey: Array[Byte] = Algos.hash("txns height")
  val GlobalBoxIndexKey: Array[Byte] = Algos.hash("boxes height")
  val RollbackToKey: Array[Byte] = Algos.hash("rollback to")
  val IndexedHeaderIdKey: Array[Byte] = Algos.hash("indexed header id")
  val DigestFrontierQuarantineKey: Array[Byte] = Algos.hash("digest frontier index quarantine")
  val IndexModeProvenanceKey: Array[Byte] = Algos.hash("extra index state mode provenance")
  val UtxoIndexProvenanceBytes: Array[Byte] = Array(0.toByte)
  val DigestIndexProvenanceBytes: Array[Byte] = Array(1.toByte)
  val SchemaVersionKey: Array[Byte] = Algos.hash("schema version")

  /**
    * Marker key recording that the extra index contains storage-rent eligibility rows.
    * Rent rows are written only while `storageRentCollection` is on, which the schema
    * version can not capture: turning the flag back on after an off period must force a
    * rebuild (blocks indexed while off have no rent rows), turning it off must not destroy
    * the index. `ErgoHistory.readOrGenerate` maintains this marker accordingly.
    */
  val RentIndexEnabledKey: Array[Byte] = Algos.hash("rent index enabled")
  val RentIndexEnabledBytes: Array[Byte] = Array(1.toByte)

  def getIndex(key: Array[Byte], history: HistoryStorage): ByteBuffer =
    ByteBuffer.wrap(history.modifierBytesById(bytesToId(key)).getOrElse(Array.fill[Byte](8) {
      0
    }))

  def getIndex(key: Array[Byte], history: ErgoHistoryReader): ByteBuffer = {
    getIndex(key, history.historyStorage)
  }

  def digestFrontierQuarantined(history: ErgoHistoryReader): Boolean =
    history.historyStorage.modifierBytesById(bytesToId(DigestFrontierQuarantineKey)).isDefined

  def indexModeHasProvenance(history: ErgoHistoryReader, digestMode: Boolean): Boolean =
    history.historyStorage.modifierBytesById(bytesToId(IndexModeProvenanceKey))
      .exists(_.sameElements(if (digestMode) DigestIndexProvenanceBytes else UtxoIndexProvenanceBytes))

  def digestIndexHasProvenance(history: ErgoHistoryReader): Boolean =
    indexModeHasProvenance(history, digestMode = true)

  private[extra] case class DigestIndexSnapshot(stateVersion: VersionTag,
                                               stateRoot: Array[Byte],
                                               stateHeaderIdOpt: Option[ModifierId],
                                               genesisRoot: Array[Byte],
                                               bodyFrontierIdOpt: Option[ModifierId],
                                               verifyTransactions: Boolean)

  private[extra] case class DigestIndexAuthority(frontier: Option[Header])

  private[extra] def digestIndexSnapshot(state: ErgoStateReader,
                                         verifyTransactions: Boolean): Option[DigestIndexSnapshot] = Try {
    val context = state.stateContext
    DigestIndexSnapshot(state.version, state.rootDigest.clone(), context.lastHeaderOpt.map(_.id),
      context.genesisStateDigest.clone(), state.fullBlockBodyFrontier, verifyTransactions)
  }.toOption

  /** A state version, context, root, and body certificate must name one selected chain. */
  private[extra] def digestIndexAuthority(history: ErgoHistoryReader,
                                          snapshot: DigestIndexSnapshot): Option[DigestIndexAuthority] = {
    if (!snapshot.verifyTransactions) None
    else history.synchronized {
      // Header and full-chain markers are written under this lock; never mix two chain epochs.
      Try {
        snapshot.stateHeaderIdOpt match {
          case Some(stateHeaderId) =>
            history.typedModifierById[Header](stateHeaderId).filter { stateHeader =>
              snapshot.stateVersion == idToVersion(stateHeader.id) &&
                snapshot.stateRoot.sameElements(stateHeader.stateRoot)
            }.flatMap { stateHeader =>
              val stateOnBestHeaderChain = history.isInBestChain(stateHeader)
              snapshot.bodyFrontierIdOpt match {
                case Some(frontierId) =>
                  history.typedModifierById[Header](frontierId)
                    .filter { frontier =>
                      frontier.height <= stateHeader.height &&
                        FullBlockProcessor.isInBestFullChain(history.historyStorage, frontier.id) &&
                        ((stateOnBestHeaderChain &&
                          history.bestHeaderIdAtHeight(frontier.height).contains(frontier.id)) ||
                          FullBlockProcessor.isInBestFullChain(history.historyStorage, stateHeader.id))
                    }
                    .map(frontier => DigestIndexAuthority(Some(frontier)))
                case None if stateOnBestHeaderChain => Some(DigestIndexAuthority(None))
                case None => None
              }
            }
          case None if snapshot.stateVersion == ErgoState.genesisStateVersion &&
              snapshot.stateRoot.sameElements(snapshot.genesisRoot) &&
              snapshot.bodyFrontierIdOpt.isEmpty =>
            Some(DigestIndexAuthority(None))
          case _ => None
        }
      }.toOption.flatten
    }
  }

  /** Gate every Digest extra-index read against the paired state and its durable certificate. */
  def checkpointOnCertifiedDigestFullChain(history: ErgoHistoryReader,
                                           state: ErgoStateReader,
                                           verifyTransactions: Boolean): Boolean = history.synchronized {
    digestIndexHasProvenance(history) && checkpointOnSelectedFullChain(history) &&
      digestIndexSnapshot(state, verifyTransactions)
        .flatMap(digestIndexAuthority(history, _)).flatMap(_.frontier).exists { frontier =>
        frontier.height >= getIndex(IndexedHeightKey, history).getInt
      }
  }

  def checkpointOnSelectedFullChain(history: ErgoHistoryReader): Boolean = {
    val indexedHeight = getIndex(IndexedHeightKey, history).getInt
    if (digestFrontierQuarantined(history)) false
    else if (indexedHeight == 0) true
    else history.historyStorage.modifierBytesById(bytesToId(IndexedHeaderIdKey))
      .filter(_.length == ErgoNodeViewModifier.ModifierIdSize)
      .map(bytesToId)
      .flatMap(history.typedModifierById[Header])
      .exists { header =>
        header.height == indexedHeight &&
          FullBlockProcessor.isInBestFullChain(history.historyStorage, header.id)
      }
  }

  def apply(chainSettings: ChainSettings,
            cacheSettings: CacheSettings,
            nodeViewHolderRef: ActorRef)(implicit system: ActorSystem, rentIndexEnabled: Boolean): ActorRef = {
    val props = Props(new ExtraIndexer(cacheSettings, chainSettings.addressEncoder, Some(nodeViewHolderRef), rentIndexEnabled))
    system.actorOf(props.withDispatcher(GlobalConstants.IndexerDispatcher))
  }
}
