package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoBox.{BoxId, TokenId}
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.{FullBlockApplied, Rollback}
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.{GlobalBoxIndexKey, GlobalTxIndexKey, IndexedHeightKey, getIndex}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.ReceivableMessages.Start
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddress.segmentTreshold
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.tokenRegistersSet
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.{Algos, CacheSettings, ChainSettings}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import java.nio.ByteBuffer
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import spire.syntax.all.cfor

/**
  * Base trait for extra indexer actor and its test.
  */
trait ExtraIndexerBase extends ScorexLogging {

  // Indexed block height
  protected var indexedHeight: Int = 0
  private val indexedHeightBuffer: ByteBuffer = ByteBuffer.allocate(4)

  // Indexed transaction count
  protected var globalTxIndex: Long = 0L
  private val globalTxIndexBuffer: ByteBuffer = ByteBuffer.allocate(8)

  // Indexed box count
  protected var globalBoxIndex: Long = 0L
  private val globalBoxIndexBuffer: ByteBuffer = ByteBuffer.allocate(8)

  // Last block height when buffer contents were saved to database
  protected var lastWroteToDB: Int = 0

  // Max buffer size (determined by config)
  protected val saveLimit: Int

  // Flag to signal when indexer has reached current block height
  protected var caughtUp: Boolean = false

  // Flag to signal a rollback
  protected var rollback: Boolean = false

  // Database handle
  protected var _history: ErgoHistory = null

  protected def chainHeight: Int = _history.fullBlockHeight
  protected def history: ErgoHistoryReader = _history.getReader
  protected def historyStorage: HistoryStorage = _history.historyStorage

  // fast access buffers
  private val general: ArrayBuffer[ExtraIndex] = ArrayBuffer.empty[ExtraIndex]
  private val boxes: ArrayBuffer[IndexedErgoBox] = ArrayBuffer.empty[IndexedErgoBox]
  private val trees: ArrayBuffer[IndexedErgoAddress] = ArrayBuffer.empty[IndexedErgoAddress]

  // input tokens in a tx
  protected val tokens: ArrayBuffer[(TokenId, Long)] = ArrayBuffer.empty[(TokenId, Long)]

  /**
    * Find a box in the boxes buffer.
    *
    * @param id - id of the wanted box
    * @return an Option containing the index of the wanted box in the boxes buffer or None if box was not found
    */
  private def findBox(id: BoxId): Option[Int] = {
    cfor(boxes.length - 1)(_ >= 0, _ - 1) { i => // loop backwards to test latest modifiers first
      if (java.util.Arrays.equals(boxes(i).serializedId, id))
        return Some(i) // box found in last saveLimit modifiers
    }
    None
  }

  /**
    * Spend an IndexedErgoBox from buffer or database. Also record tokens for later use in balance tracking logic.
    *
    * @param id     - id of the wanted box
    * @param txId   - id of the spending transaction
    * @param height - height of the block the spending transaction is included in
    * @return index of spent box in boxes buffer or -1 if an unknown box was spent
    */
  private def findAndSpendBox(id: BoxId, txId: ModifierId, height: Int): Int = {
    findBox(id) match {
      case Some(i) =>
        tokens ++= boxes(i).asSpent(txId, height).box.additionalTokens.toArray
        i
      case None =>
        history.typedExtraIndexById[IndexedErgoBox](bytesToId(id)) match { // box not found in last saveLimit modifiers
          case Some(x) => // box found in DB, update
            boxes += x.asSpent(txId, height)
            tokens ++= x.box.additionalTokens.toArray
            boxes.length - 1
          case None => // box not found at all (this shouldn't happen)
            log.warn(s"Unknown box used as input: ${bytesToId(id)}")
            -1
        }
    }
  }

  /**
    * Add or subtract a box from an address in the buffer or in database.
    *
    * @param id             - hash of the (ergotree) address
    * @param spendOrReceive - ErgoBox to spend or IndexedErgoBox to receive
    * @return index of updated tree in buffer or -1 if the tree was unknown
    */
  private def findAndUpdateTree(id: ModifierId, spendOrReceive: Either[ErgoBox, IndexedErgoBox]): Int = {
    cfor(trees.length - 1)(_ >= 0, _ - 1) { i => // loop backwards to test latest modifiers first
      if (trees(i).id == id) { // address found in last saveLimit modifiers
        spendOrReceive match {
          case Left(box) => trees(i).addTx(globalTxIndex).spendBox(box) // spend box
          case Right(iEb) => trees(i).addTx(globalTxIndex).addBox(iEb) // receive box
        }
        return i
      }
    }
    history.typedExtraIndexById[IndexedErgoAddress](id) match { // address not found in last saveLimit modifiers
      case Some(x) =>
        spendOrReceive match {
          case Left(box) => trees += x.addTx(globalTxIndex).spendBox(box) // spend box
          case Right(iEb) => trees += x.addTx(globalTxIndex).addBox(iEb) // receive box
        }
        trees.length - 1
      case None => // address not found at all
        spendOrReceive match {
          case Left(box) => log.warn(s"Unknown address spent box ${bytesToId(box.id)}") // spend box should never happen by an unknown address
          case Right(iEb) => trees += IndexedErgoAddress(id, ListBuffer(globalTxIndex), ListBuffer.empty[Long], Some(new BalanceInfo)).addBox(iEb) // receive box
        }
        -1
    }
  }

  /**
    * @return number of indexes in all buffers
    */
  private def modCount: Int = general.length + boxes.length + trees.length

  /**
    * Write buffered indexes to database and clear buffers.
    */
  private def saveProgress(writeLog: Boolean = true): Unit = {

    if(modCount == 0) return

    val start: Long = System.nanoTime()

    // perform segmentation on big modifiers
    val addressesLen: Int = trees.length
    cfor(0)(_ < addressesLen, _ + 1) { i =>
      if (trees(i).txs.length > segmentTreshold || trees(i).boxes.length > segmentTreshold) trees ++= trees(i).splitToSegments()
    }

    // merge all modifiers to an Array, avoids reallocations during concatenation (++)
    val all: Array[ExtraIndex] = new Array[ExtraIndex](modCount)
    val offset: Array[Int] = Array(0, general.length, general.length + boxes.length)
    cfor(0)(_ < general.length, _ + 1) { i => all(i + offset(0)) = general(i) }
    cfor(0)(_ < boxes.length, _ + 1) { i => all(i + offset(1)) = boxes(i) }
    cfor(0)(_ < trees.length, _ + 1) { i => all(i + offset(2)) = trees(i) }

    // insert modifiers and progress info to db
    indexedHeightBuffer.clear()
    globalTxIndexBuffer.clear()
    globalBoxIndexBuffer.clear()
    historyStorage.insertExtra(Array((IndexedHeightKey, indexedHeightBuffer.putInt(indexedHeight).array),
      (GlobalTxIndexKey, globalTxIndexBuffer.putLong(globalTxIndex).array),
      (GlobalBoxIndexKey, globalBoxIndexBuffer.putLong(globalBoxIndex).array)), all)

    val end: Long = System.nanoTime()

    if (writeLog)
      log.info(s"Processed ${trees.length} ErgoTrees with ${boxes.length} boxes and inserted them to database in ${(end - start) / 1000000D}ms")

    // clear buffers for next batch
    general.clear()
    boxes.clear()
    trees.clear()

    lastWroteToDB = indexedHeight
  }

  /**
    * Process a batch of BlockTransactions into memory and occasionally write them to database.
    *
    * @param bt     - BlockTransaction to process
    * @param height - height of the block containing the transactions
    */
  protected def index(bt: BlockTransactions, height: Int): Unit = {

    if (rollback || // rollback in progress
       (caughtUp && height <= indexedHeight)) // do not process older blocks again after caught up (due to actor message queue)
      return

    var boxCount: Int = 0

    // record transactions and boxes
    cfor(0)(_ < bt.txs.length, _ + 1) { n =>

      val tx: ErgoTransaction = bt.txs(n)
      val inputs: Array[Long] = Array.ofDim[Long](tx.inputs.length)

      tokens.clear()

      //process transaction inputs
      if (height != 1) { //only after 1st block (skip genesis box)
        cfor(0)(_ < tx.inputs.size, _ + 1) { i =>
          val boxIndex: Int = findAndSpendBox(tx.inputs(i).boxId, tx.id, height)
          if (boxIndex >= 0) { // spend box and add tx
            findAndUpdateTree(bytesToId(hashErgoTree(boxes(boxIndex).box.ergoTree)), Left(boxes(boxIndex).box))
            inputs(i) = boxes(boxIndex).globalIndex
          }
        }
      }

      //process transaction outputs
      cfor(0)(_ < tx.outputs.size, _ + 1) { i =>
        val box: ErgoBox = tx.outputs(i)
        boxes += new IndexedErgoBox(height, None, None, box, globalBoxIndex) // box by id
        general += NumericBoxIndex(globalBoxIndex, bytesToId(box.id)) // box id by global box number

        // box by address
        findAndUpdateTree(bytesToId(hashErgoTree(box.ergoTree)), Right(boxes(findBox(box.id).get)))

        // check if box is creating a new token, if yes record it
        if (tokenRegistersSet(box))
          cfor(0)(_ < box.additionalTokens.length, _ + 1) { j =>
            if (!tokens.exists(x => java.util.Arrays.equals(x._1, box.additionalTokens(j)._1))) {
              general += IndexedTokenSerializer.fromBox(box)
            }
          }

        globalBoxIndex += 1
        boxCount += 1

      }

      //process transaction
      general += IndexedErgoTransaction(tx.id, height, globalTxIndex, inputs)
      general += NumericTxIndex(globalTxIndex, tx.id)

      globalTxIndex += 1

    }

    log.info(s"Buffered block $height / $chainHeight [txs: ${bt.txs.length}, boxes: $boxCount] (buffer: $modCount / $saveLimit)")

    if (caughtUp) {

      indexedHeight = height // update height here after caught up with chain

      if (modCount >= saveLimit || // modifier limit reached to write to db
        history.fullBlockHeight == history.headersHeight) // write to db every block after chain synced
        saveProgress()

    } else if (modCount >= saveLimit)
      saveProgress() // active syncing, write to db after modifier limit

  }

  /**
    * Main indexer loop that tries to catch up with the already present blocks in database.
    */
  protected def run(): Unit = {

    indexedHeight  = getIndex(IndexedHeightKey)(history).getInt
    globalTxIndex  = getIndex(GlobalTxIndexKey)(history).getLong
    globalBoxIndex = getIndex(GlobalBoxIndexKey)(history).getLong

    log.info(s"Started extra indexer at height $indexedHeight")

    while (indexedHeight < chainHeight && !rollback) {
      indexedHeight += 1
      index(history.bestBlockTransactionsAt(indexedHeight).get, indexedHeight)
    }

    saveProgress(false) // flush any remaining data

    if (rollback)
      log.info("Stopping indexer to perform rollback")
    else {
      caughtUp = true
      log.info("Indexer caught up with chain")
    }

  }

  /**
    * Remove all indexes after a given height and revert address balances.
    *
    * @param height - starting height
    */
  protected def removeAfter(height: Int): Unit = {

    saveProgress(false)
    log.info(s"Rolling back indexes from $indexedHeight to $height")

    val lastTxToKeep: ErgoTransaction = history.bestBlockTransactionsAt(height).get.txs.last

    // remove all tx indexes
    val txTarget: Long = history.typedExtraIndexById[IndexedErgoTransaction](lastTxToKeep.id).get.globalIndex
    val txs: ArrayBuffer[ModifierId] = ArrayBuffer.empty[ModifierId]
    globalTxIndex -= 1
    while(globalTxIndex > txTarget) {
      val tx: IndexedErgoTransaction = NumericTxIndex.getTxByNumber(history, globalTxIndex).get
      tx.inputNums.map(NumericBoxIndex.getBoxByNumber(history, _).get).foreach(iEb => { // undo all spendings
          iEb.spendingHeightOpt = None
          iEb.spendingTxIdOpt = None
          val address: IndexedErgoAddress = history.typedExtraIndexById[IndexedErgoAddress](bytesToId(hashErgoTree(iEb.box.ergoTree))).get.addBox(iEb, false)
          historyStorage.insertExtra(Array.empty, Array(iEb, address))
      })
      txs += tx.id // tx by id
      txs += bytesToId(NumericTxIndex.indexToBytes(globalTxIndex)) // tx id by number
      globalTxIndex -= 1
    }
    globalTxIndex += 1
    historyStorage.removeExtra(txs.toArray)

    // remove all box indexes, tokens and address balances
    val boxTarget: Long = history.typedExtraIndexById[IndexedErgoBox](bytesToId(lastTxToKeep.outputs.last.id)).get.globalIndex
    val toRemove: ArrayBuffer[ModifierId] = ArrayBuffer.empty[ModifierId]
    globalBoxIndex -= 1
    while(globalBoxIndex > boxTarget) {
      val iEb: IndexedErgoBox = NumericBoxIndex.getBoxByNumber(history, globalBoxIndex).get
      val address: IndexedErgoAddress = history.typedExtraIndexById[IndexedErgoAddress](bytesToId(hashErgoTree(iEb.box.ergoTree))).get
      address.spendBox(iEb.box)
      if(tokenRegistersSet(iEb.box))
        history.typedExtraIndexById[IndexedToken](IndexedTokenSerializer.fromBox(iEb.box).id) match {
          case Some(token) => toRemove += token.id // token created, delete
          case None => // no token created
        }
      address.rollback(txTarget, boxTarget)(_history)
      toRemove += iEb.id // box by id
      toRemove += bytesToId(NumericBoxIndex.indexToBytes(globalBoxIndex)) // box id by number
      globalBoxIndex -= 1
    }
    globalBoxIndex += 1
    historyStorage.removeExtra(toRemove.toArray)

    saveProgress(false)

    log.info(s"Successfully rolled back indexes to $height")

    rollback = false

  }

}


/**
  * Actor that constructs an index of database elements.
  * @param cacheSettings - cacheSettings to use for saveLimit size
  */
class ExtraIndexer(cacheSettings: CacheSettings)
  extends Actor with ExtraIndexerBase {

  override val saveLimit: Int = cacheSettings.history.extraCacheSize * 10

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[FullBlockApplied])
    context.system.eventStream.subscribe(self, classOf[Rollback])
  }

  override def postStop(): Unit =
    log.info(s"Stopped extra indexer at height ${if(lastWroteToDB > 0) lastWroteToDB else indexedHeight}")

  override def receive: Receive = {

    case FullBlockApplied(header: Header) if caughtUp =>
      index(history.typedModifierById[BlockTransactions](header.transactionsId).get, header.height) // after the indexer caught up with the chain, stay up to date

    case Rollback(branchPoint: ModifierId) =>
      val branchHeight: Int = history.heightOf(branchPoint).get
      rollback = branchHeight < indexedHeight
      if(rollback) {
        removeAfter(branchHeight)
        run() // restart indexer
      }

    case Start(history: ErgoHistory) =>
      _history = history
      run()

  }
}

object ExtraIndexerRef {

  object ReceivableMessages {
    /**
      * Initialize ExtraIndexer and start indexing.
      * @param history - handle to database
      */
    case class Start(history: ErgoHistory)
  }

  private var _ae: ErgoAddressEncoder = _

  def getAddressEncoder: ErgoAddressEncoder = _ae

  def setAddressEncoder(encoder: ErgoAddressEncoder): Unit = if(_ae == null) _ae = encoder

  private val hexIndex: Array[Byte] = {
    val index = Array.fill[Byte](128)(0xff.toByte)
    "0123456789abcdef".toCharArray.zipWithIndex.foreach { case (c, i) =>
      index(c) = i.toByte
    }
    "abcdef".toCharArray.foreach{ c =>
      index(c.toUpper) = index(c)
    }
    index
  }

  /**
    * Faster id to bytes - no safety checks
    * @param id - ModifierId to convert to byte representation
    * @return an array of bytes
    */
  private[extra] def fastIdToBytes(id: ModifierId): Array[Byte] = {
    val x: Array[Byte] = new Array[Byte](id.length / 2)
    cfor(0)(_ < id.length, _ + 2) {i => x(i / 2) = ((hexIndex(id(i)) << 4) | hexIndex(id(i + 1))).toByte}
    x
  }

  val IndexedHeightKey: Array[Byte] = Algos.hash("indexed height")
  val GlobalTxIndexKey: Array[Byte] = Algos.hash("txns height")
  val GlobalBoxIndexKey: Array[Byte] = Algos.hash("boxes height")

  def getIndex(key: Array[Byte])(history: ErgoHistoryReader): ByteBuffer =
    ByteBuffer.wrap(history.modifierBytesById(bytesToId(key)).getOrElse(Array.fill[Byte](8){0}))

  def apply(chainSettings: ChainSettings, cacheSettings: CacheSettings)(implicit system: ActorSystem): ActorRef = {
    val actor = system.actorOf(Props.create(classOf[ExtraIndexer], cacheSettings))
    _ae = chainSettings.addressEncoder
    ExtraIndexerRefHolder.init(actor)
    actor
  }
}

object ExtraIndexerRefHolder {

  private var _actor: Option[ActorRef] = None
  private var running: Boolean = false

  /**
    * Start stored ExtraIndexer actor with history handle.
    * @param history - initialized history handle
    */
  private[history] def start(history: ErgoHistory): Unit = if(_actor.isDefined && !running) {
    _actor.get ! Start(history)
    running = true
  }

  /**
    * Store a Ref to an ExtraIndexer actor.
    * @param actor - Ref to ExtraIndexer
    */
  private[extra] def init(actor: ActorRef): Unit = _actor = Some(actor)
}
/**
  * Base trait for all additional indexes made by ExtraIndexer
  */
trait ExtraIndex {
  def id: ModifierId = bytesToId(serializedId)
  def serializedId: Array[Byte]
}
