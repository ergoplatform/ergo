package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Stash}
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, GlobalConstants, Pay2SAddress}
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{FullBlockApplied, Rollback}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer._
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages._
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.uniqueId
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.{Algos, CacheSettings, ChainSettings}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigma.ast.ErgoTree
import sigma.Extensions._

import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import spire.syntax.all.cfor

import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.collection.concurrent
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.jdk.CollectionConverters._

/**
  * Base trait for extra indexer actor and its test.
  */
trait ExtraIndexerBase extends Actor with Stash with ScorexLogging {

  private implicit val ec: ExecutionContextExecutor = context.dispatcher

  /**
    * Max buffer size (determined by config)
    */
  protected val saveLimit: Int

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

  // fast access buffers
  protected val general: ArrayBuffer[ExtraIndex] = ArrayBuffer.empty[ExtraIndex]
  protected val boxes: mutable.HashMap[ModifierId, IndexedErgoBox] = mutable.HashMap.empty[ModifierId, IndexedErgoBox]
  protected val trees: mutable.HashMap[ModifierId, IndexedErgoAddress] = mutable.HashMap.empty[ModifierId, IndexedErgoAddress]
  protected val tokens: mutable.HashMap[ModifierId, IndexedToken] = mutable.HashMap.empty[ModifierId, IndexedToken]
  protected val segments: mutable.HashMap[ModifierId, Segment[_]] = mutable.HashMap.empty[ModifierId, Segment[_]]

  /**
    * Input tokens in a transaction, cleared after every transaction
    */
  private val inputTokens: ArrayBuffer[(TokenId, Long)] = ArrayBuffer.empty[(TokenId, Long)]

  /**
    * Holds upcoming blocks to be indexed, and when empty, it is filled back from multiple threads
    */
  private val blockCache: concurrent.Map[Int, BlockTransactions] = new ConcurrentHashMap[Int, BlockTransactions]().asScala
  private var readingUpTo: Int = 0

  /**
    * Get transactions for specified height, preferably from cache, or from database.
    * If indexer is getting close to emptying cache, asynchronously reads 1000 blocks into it
    *
    * @param height - blockheight to get transations from
    * @return transactions at height
    */
  private def getBlockTransactionsAt(height: Int): Option[BlockTransactions] = {
    blockCache.remove(height).orElse(history.bestBlockTransactionsAt(height)).map { txs =>
      if (height % 1000 == 0) blockCache.keySet.filter(_ < height).map(blockCache.remove)
      if (readingUpTo - height < 300 && chainHeight - height > 1000) {
        readingUpTo = math.min(height + 1001, chainHeight)

        if(height < history.fullBlockHeight - 1000) {
          val blockNums = height + 1 to readingUpTo by 250
          blockNums.zip(blockNums.tail).map { range => // ranges of 250 blocks for each thread to read
            Future {
              (range._1 until range._2).foreach { blockNum =>
                history.bestBlockTransactionsAt(blockNum).map(blockCache.put(blockNum, _))
              }
            }
          }
        } else {
          val blockNums = height + 1 to readingUpTo
          Future {
            blockNums.foreach { blockNum =>
              history.bestBlockTransactionsAt(blockNum).map(blockCache.put(blockNum, _))
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
    * @return whether or not the box was found in buffer or database -> should always return true
    */
  private def findAndSpendBox(id: ModifierId, txId: ModifierId, height: Int): Boolean = {
    boxes.get(id).map(box => {
      inputTokens ++= box.asSpent(txId, height).box.additionalTokens.toArray
      return true
    })
    history.typedExtraIndexById[IndexedErgoBox](id) match { // box not found in last saveLimit modifiers
      case Some(x) => // box found in DB, update
        boxes.put(id, x.asSpent(txId, height))
        inputTokens ++= x.box.additionalTokens.toArray
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

  /**
    * @return number of indexes in all buffers
    */
  private def modCount: Int = general.length + boxes.size + trees.size + tokens.size

  /**
    * Write buffered indexes to database and clear buffers.
    */
  private def saveProgress(state: IndexerState): Unit = {

    val start: Long = System.currentTimeMillis

    // perform segmentation on big addresses and save their internal segment buffer
    trees.values.foreach { tree =>
      tree.buffer.values.foreach(seg => segments.put(seg.id, seg))
      tree.splitToSegments.foreach(seg => segments.put(seg.id, seg))
    }

    // perform segmentation on big tokens and save their internal segment buffer
    tokens.values.foreach { token =>
      token.buffer.values.foreach(seg => segments.put(seg.id, seg))
      token.splitToSegments.foreach(seg => segments.put(seg.id, seg))
    }

    // insert modifiers and progress info to db
    historyStorage.insertExtra(
      Array(
        (IndexedHeightKey, ByteBuffer.allocate(4).putInt(state.indexedHeight).array),
        (GlobalTxIndexKey, ByteBuffer.allocate(8).putLong(state.globalTxIndex).array),
        (GlobalBoxIndexKey, ByteBuffer.allocate(8).putLong(state.globalBoxIndex).array),
        (RollbackToKey, ByteBuffer.allocate(4).putInt(state.rollbackTo).array)
      ),
      ((((general ++= boxes.values) ++= trees.values) ++= tokens.values) ++= segments.values).toArray
    )

    log.debug(s"Processed ${trees.size} ErgoTrees with ${boxes.size} boxes and inserted them to database in ${System.currentTimeMillis - start}ms")

    // clear buffers for next batch
    general.clear()
    boxes.clear()
    trees.clear()
    tokens.clear()
    segments.clear()
  }

  /**
    * Process a batch of BlockTransactions into memory and occasionally write them to database.
    *
    * @param state     - current indexer state
    * @param headerOpt - header to index block transactions of (used after caught up with chain)
    */
  protected def index(state: IndexerState, headerOpt: Option[Header] = None): IndexerState = {
    val btOpt = headerOpt.flatMap { header =>
      history.typedModifierById[BlockTransactions](header.transactionsId)
    }.orElse(getBlockTransactionsAt(state.indexedHeight))
    val height = headerOpt.map(_.height).getOrElse(state.indexedHeight)

    if (btOpt.isEmpty) {
      log.error(s"Could not read block $height / $chainHeight from database, waiting for new block until retrying")
      return state.decrementIndexedHeight.copy(caughtUp = true)
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
          if (findAndSpendBox(boxId, tx.id, height)) { // spend box and add tx
            val iEb = boxes(boxId)
            findAndUpdateTree(hashErgoTree(iEb.box.ergoTree), Left(iEb))(state)
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
        val iEb: IndexedErgoBox = new IndexedErgoBox(height, None, None, tx.outputs(i), newState.globalBoxIndex)
        boxes.put(iEb.id, iEb) // box by id
        general += NumericBoxIndex(newState.globalBoxIndex, iEb.id) // box id by global box number
        outputs(i) = iEb.globalIndex

        // box by address
        findAndUpdateTree(hashErgoTree(iEb.box.ergoTree), Right(boxes(iEb.id)))(state)

        // check if box is creating new tokens, if yes record them
        cfor(0)(_ < iEb.box.additionalTokens.length, _ + 1) { j =>
          if (!inputTokens.exists(x => java.util.Arrays.equals(x._1.toArray, iEb.box.additionalTokens(j)._1.toArray))) {
            val token = IndexedToken.fromBox(iEb, j)
            tokens.get(token.tokenId) match {
              case Some(t) => // same new token created in multiple boxes -> add amounts
                tokens.put(token.tokenId, t.addEmissionAmount(token.amount))
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

    val maxHeight = headerOpt.map(_.height).getOrElse(chainHeight)
    newState.copy(caughtUp = newState.indexedHeight == maxHeight)
  }

  /**
    * Remove all indexes after a given height and revert address balances.
    *
    * @param state  - current state of indexer
    * @param height - starting height
    */
  private def removeAfter(state: IndexerState, height: Int): IndexerState = {

    var newState: IndexerState = state

    saveProgress(newState)
    log.info(s"Rolling back indexes from ${state.indexedHeight} to $height")

    try {
      val lastTxToKeep: ErgoTransaction = history.bestBlockTransactionsAt(height).get.txs.last
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

          val address = history.typedExtraIndexById[IndexedErgoAddress](hashErgoTree(iEb.box.ergoTree)).get.addBox(iEb, record = false)
          address.findAndModBox(iEb.globalIndex, history)
          historyStorage.insertExtra(Array.empty, Array[ExtraIndex](iEb, address) ++ address.buffer.values)

          cfor(0)(_ < iEb.box.additionalTokens.length, _ + 1) { i =>
            history.typedExtraIndexById[IndexedToken](IndexedToken.fromBox(iEb, i).id).map { token =>
              token.findAndModBox(iEb.globalIndex, history)
              historyStorage.insertExtra(Array.empty, Array[ExtraIndex](token) ++ token.buffer.values)
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
            if (token.boxId == iEb.id) { // token created, delete
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
        toRemove += iEb.id // box by id
        toRemove += bytesToId(NumericBoxIndex.indexToBytes(newState.globalBoxIndex)) // box id by number
        newState = newState.decrementBoxIndex
      }
      newState = newState.incrementBoxIndex

      // Save changes
      newState = newState.copy(indexedHeight = height, rollbackTo = 0, caughtUp = true)
      historyStorage.removeExtra(toRemove.toArray)
      saveProgress(newState)
    } catch {
      case t: Throwable => log.error(s"removeAfter during rollback failed due to: ${t.getMessage}", t)
    }

    newState
  }

  protected def loaded(state: IndexerState): Receive = {

    case Index() if !state.caughtUp && !state.rollbackInProgress =>
      val newState = index(state.incrementIndexedHeight)
      if (modCount >= saveLimit) saveProgress(newState)
      context.become(loaded(newState))
      self ! Index()

    case Index() if state.caughtUp =>
      if (modCount > 0) saveProgress(state)
      blockCache.clear()
      log.info("Indexer caught up with chain")

    // after the indexer caught up with the chain, stay up to date
    case FullBlockApplied(header: Header) if state.caughtUp && !state.rollbackInProgress =>
      if (header.height == state.indexedHeight + 1) { // applied block is next in line
        val newState: IndexerState = index(state.incrementIndexedHeight, Some(header))
        saveProgress(newState)
        context.become(loaded(newState))
      } else if (header.height > state.indexedHeight + 1) { // applied block is ahead of indexer
        context.become(loaded(state.copy(caughtUp = false)))
        self ! Index()
      } else // applied block has already been indexed, skipping duplicate
        log.warn(s"Skipping block ${header.id} applied at height ${header.height}, indexed height is ${state.indexedHeight}")

    case Rollback(branchPoint: ModifierId) =>
      if (state.rollbackInProgress) {
        log.warn(s"Rollback already in progress")
        stash()
      } else {
        history.heightOf(branchPoint) match {
          case Some(branchHeight) =>
            if (branchHeight < state.indexedHeight) {
              context.become(loaded(state.copy(rollbackTo = branchHeight)))
              self ! RemoveAfter(branchHeight)
            }
          case None =>
            log.error(s"No rollback height found for $branchPoint")
            val newState = state.copy(rollbackTo = 0)
            context.become(loaded(newState))
            unstashAll()
        }
      }

    case RemoveAfter(branchHeight: Int) if state.rollbackInProgress =>
      blockCache.clear()
      readingUpTo = 0
      val newState = removeAfter(state, branchHeight)
      context.become(loaded(newState))
      log.info(s"Successfully rolled back indexes to $branchHeight")
      unstashAll()

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
      _history = history
      val state = IndexerState.fromHistory(history)
      context.become(loaded(state))
      log.info(s"Started extra indexer at height ${state.indexedHeight}")
      self ! Index()
      unstashAll()

    case _ => stash()

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
  val NewestVersion: Int = 5
  val NewestVersionBytes: Array[Byte] = ByteBuffer.allocate(4).putInt(NewestVersion).array

  val IndexedHeightKey: Array[Byte] = Algos.hash("indexed height")
  val GlobalTxIndexKey: Array[Byte] = Algos.hash("txns height")
  val GlobalBoxIndexKey: Array[Byte] = Algos.hash("boxes height")
  val RollbackToKey: Array[Byte] = Algos.hash("rollback to")
  val SchemaVersionKey: Array[Byte] = Algos.hash("schema version")

  def getIndex(key: Array[Byte], history: HistoryStorage): ByteBuffer =
    ByteBuffer.wrap(history.modifierBytesById(bytesToId(key)).getOrElse(Array.fill[Byte](8) {
      0
    }))

  def getIndex(key: Array[Byte], history: ErgoHistoryReader): ByteBuffer = getIndex(key, history.historyStorage)

  def apply(chainSettings: ChainSettings, cacheSettings: CacheSettings)(implicit system: ActorSystem): ActorRef = {
    val props = Props.create(classOf[ExtraIndexer], cacheSettings, chainSettings.addressEncoder)
    system.actorOf(props.withDispatcher(GlobalConstants.IndexerDispatcher))
  }
}
