package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Stash, Timers}
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, GlobalConstants, Pay2SAddress}
import org.ergoplatform.consensus.ModifierSemanticValidity
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{FullBlockApplied, Rollback}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer._
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages._
import org.ergoplatform.nodeView.history.extra.IndexedContractTemplateSerializer.hashTreeTemplate
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.uniqueId
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockProcessor
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
    _history.headerIdsAtHeight(height)
      .find { id =>
        FullBlockProcessor.isInBestFullChain(historyStorage, id) &&
          _history.isSemanticallyValid(id) == ModifierSemanticValidity.Valid
      }
      .flatMap(id => _history.typedModifierById[Header](id))
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
    boxes.get(id).map(box => {
      box.asSpent(txId, height, spendingProof).box.additionalTokens.toArray.map(x => x._1.toArray.toSeq -> x._2).foreach { case (k, v) =>
        inputTokens.put(k, inputTokens.getOrElse(k, 0L) + v)
      }
      return true
    })
    history.typedExtraIndexById[IndexedErgoBox](id) match { // box not found in last saveLimit modifiers
      case Some(x) => // box found in DB, update
        boxes.put(id, x.asSpent(txId, height, spendingProof))
        x.box.additionalTokens.toArray.map(x => x._1.toArray.toSeq -> x._2).foreach { case (k, v) =>
          inputTokens.put(k, inputTokens.getOrElse(k, 0L) + v)
        }
        true
      case None => // box not found at all (this shouldn't happen)
        log.warn(s"Unknown box used as input: $id")
        false
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
  private def modCount: Int = general.length + boxes.size + trees.size + templates.size + tokens.size

  /**
    * Write buffered indexes to database and clear buffers.
    */
  private def saveProgress(state: IndexerState): Try[Unit] = Try {
    val start: Long = System.currentTimeMillis

    trees.values.foreach { tree =>
      tree.buffer.values.foreach(seg => segments.put(seg.id, seg))
      tree.splitToSegments.foreach(seg => segments.put(seg.id, seg))
    }
    templates.values.foreach { template =>
      template.buffer.values.foreach(seg => segments.put(seg.id, seg))
      template.splitToSegments.foreach(seg => segments.put(seg.id, seg))
    }
    tokens.values.foreach { token =>
      token.buffer.values.foreach(seg => segments.put(seg.id, seg))
      token.splitToSegments.foreach(seg => segments.put(seg.id, seg))
    }

    val indexedHeaderEntry = state.indexedHeaderId.map { id =>
      IndexedHeaderIdKey -> fastIdToBytes(id)
    }.toArray
    val objects = (general.iterator ++ boxes.valuesIterator ++ trees.valuesIterator ++
      templates.valuesIterator ++ tokens.valuesIterator ++ segments.valuesIterator).toArray
    historyStorage.insertExtraTry(
      Array(
        (IndexedHeightKey, ByteBuffer.allocate(4).putInt(state.indexedHeight).array),
        (GlobalTxIndexKey, ByteBuffer.allocate(8).putLong(state.globalTxIndex).array),
        (GlobalBoxIndexKey, ByteBuffer.allocate(8).putLong(state.globalBoxIndex).array),
        (RollbackToKey, ByteBuffer.allocate(4).putInt(state.rollbackTo).array)
      ) ++ indexedHeaderEntry,
      objects
    ).recoverWith { case error =>
      historyStorage.invalidateExtraCache(objects.iterator.map(_.id).toSeq)
      Failure(error)
    }.get

    log.debug(s"Processed ${trees.size} ErgoTrees with ${boxes.size} boxes and inserted them to database in ${System.currentTimeMillis - start}ms")
    general.clear()
    boxes.clear()
    trees.clear()
    templates.clear()
    tokens.clear()
    segments.clear()
  }

  /**
    * Process a batch of BlockTransactions into memory and occasionally write them to database.
    *
    * @param state     - current indexer state
    * @param header       - exact full-chain header to index
    * @param targetHeight - full-chain height captured for this catch-up pass
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
      general += IndexedErgoTransaction.fromTx(tx, n, height, newState.globalTxIndex, inputs, outputs)
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
        tx.inputNums.map(NumericBoxIndex.getBoxByNumber(history, _).get).foreach { iEb => // undo all spendings

          iEb.spendingHeightOpt = None
          iEb.spendingTxIdOpt = None
          iEb.spendingProofOpt = None

          val address = history.typedExtraIndexById[IndexedErgoAddress](hashErgoTree(iEb.box.ergoTree)).get.addBox(iEb, record = false)
          address.findAndModBox(iEb.globalIndex, history)

          val template = history.typedExtraIndexById[IndexedContractTemplate](hashTreeTemplate(iEb.box.ergoTree)).get
          template.findAndModBox(iEb.globalIndex, history)

          historyStorage.insertExtraTry(Array.empty, Array[ExtraIndex](iEb, address, template) ++ address.buffer.values ++ template.buffer.values).get

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
        val iEb: IndexedErgoBox = NumericBoxIndex.getBoxByNumber(history, newState.globalBoxIndex).get
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
        newState = newState.decrementBoxIndex
      }
      newState = newState.incrementBoxIndex

      // Save changes
      val completedState = newState.copy(
        indexedHeight = height,
        rollbackTo = 0,
        caughtUp = height == chainHeight && fullChainHeaderAtHeight(height).exists(_.id == targetHeader.id),
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
    val rollbackHeaderOpt = for {
      indexedHeaderId <- state.indexedHeaderId
      indexedHeader <- history.typedModifierById[Header](indexedHeaderId)
      bestFullBlock <- history.bestFullBlockOpt
      branchPointId <- history.chainToHeader(Some(indexedHeader), bestFullBlock.header)._1
      branchHeader <- history.typedModifierById[Header](branchPointId)
      if branchHeader.height < state.indexedHeight
    } yield branchHeader

    rollbackHeaderOpt.exists { branchHeader =>
      beginRollback(state, branchHeader)
      true
    }
  }

  private def validatedRollbackHeader(state: IndexerState, branchPoint: ModifierId): Option[Header] = {
    history.typedModifierById[Header](branchPoint).filter { header =>
      header.height < state.indexedHeight &&
        fullChainHeaderAtHeight(header.height).exists(_.id == header.id)
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

  protected def loaded(state: IndexerState): Receive = {

    case Index() if !state.caughtUp && !state.rollbackInProgress =>
      cancelRetry()
      if (modCount < saveLimit || persistBuffered(state)) {
        if (state.indexedHeight == chainHeight && indexedTipIsOnBestFullChain(state)) {
          val newState = state.copy(caughtUp = true)
          context.become(receive.orElse(loaded(newState)))
          self ! Index()
        } else {
          val nextHeaderOpt = fullChainHeaderAtHeight(state.indexedHeight + 1)
          val extendsIndexedTip = nextHeaderOpt.forall { header =>
            state.indexedHeight == 0 || state.indexedHeaderId.contains(header.parentId)
          }
          if (extendsIndexedTip && nextHeaderOpt.isDefined) {
            index(state.incrementIndexedHeight, nextHeaderOpt.get, chainHeight) match {
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
        log.info("Indexer caught up with chain")
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
        val targetHeight = chainHeight
        index(state.incrementIndexedHeight, header, targetHeight) match {
          case Some(newState) =>
            context.become(receive.orElse(loaded(newState)))
            if (newState.caughtUp) {
              if (persistBuffered(newState)) caughtUpHook(header.height)
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
      scheduleRetry()

    case _: FullBlockApplied if state.rollbackInProgress => stash()

    case Rollback(branchPoint: ModifierId) =>
      cancelRetry()
      if (state.rollbackInProgress) {
        log.warn(s"Rollback already in progress")
        stash()
      } else if (indexedTipIsOnBestFullChain(state)) {
        log.info(s"Ignoring rollback to $branchPoint because the indexed tip is already on the best full chain")
        if (!state.caughtUp) self ! Index()
      } else {
        validatedRollbackHeader(state, branchPoint) match {
          case Some(header) => beginRollback(state, header)
          case None if !reconcileIndexedTip(state) =>
            log.info(s"Deferring rollback to $branchPoint until the indexed tip can be reconciled with the best full chain")
            val newState = state.copy(caughtUp = false, rollbackTo = 0)
            context.become(receive.orElse(loaded(newState)))
            scheduleRetry()
          case None =>
        }
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
  * @param cacheSettings - cacheSettings to use for saveLimit size
  * @param ae            - ergo address encoder to use for handling addresses
  */
class ExtraIndexer(cacheSettings: CacheSettings,
                   ae: ErgoAddressEncoder)
  extends ExtraIndexerBase {

  override val saveLimit: Int = cacheSettings.history.extraCacheSize * 20

  override implicit val segmentThreshold: Int = 512

  override implicit val addressEncoder: ErgoAddressEncoder = ae

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[FullBlockApplied])
    context.system.eventStream.subscribe(self, classOf[Rollback])
    context.system.eventStream.subscribe(self, classOf[StartExtraIndexer])
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

    case StartExtraIndexer(history: ErgoHistory) =>
      log.info(s"Starting extra indexer")
      _history = history
      val state = IndexerState.fromHistory(history)
      context.become(receive.orElse(loaded(state)))
      log.info(s"Started extra indexer at height ${state.indexedHeight}")
      self ! Index()
      unstashAll()

  }
}

object ExtraIndexer {

  type ExtraIndexTypeId = Byte

  object ReceivableMessages {
    /**
      * Initialize ExtraIndexer and start indexing.
      *
      * @param history - handle to database
      */
    case class StartExtraIndexer(history: ErgoHistory)

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
  val SchemaVersionKey: Array[Byte] = Algos.hash("schema version")

  def getIndex(key: Array[Byte], history: HistoryStorage): ByteBuffer =
    ByteBuffer.wrap(history.modifierBytesById(bytesToId(key)).getOrElse(Array.fill[Byte](8) {
      0
    }))

  def getIndex(key: Array[Byte], history: ErgoHistoryReader): ByteBuffer = {
    getIndex(key, history.historyStorage)
  }

  def apply(chainSettings: ChainSettings, cacheSettings: CacheSettings)(implicit system: ActorSystem): ActorRef = {
    val props = Props.create(classOf[ExtraIndexer], cacheSettings, chainSettings.addressEncoder)
    system.actorOf(props.withDispatcher(GlobalConstants.IndexerDispatcher))
  }
}
