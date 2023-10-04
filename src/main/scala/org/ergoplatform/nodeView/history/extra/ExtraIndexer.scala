package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoBox.TokenId
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder, Pay2SAddress}
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.{FullBlockApplied, Rollback}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.{GlobalBoxIndexKey, GlobalTxIndexKey, IndexedHeightKey, getIndex}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.{GetSegmentTreshold, StartExtraIndexer}
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddressSerializer.hashErgoTree
import org.ergoplatform.nodeView.history.extra.IndexedTokenSerializer.uniqueId
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.{Algos, CacheSettings, ChainSettings}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}
import sigmastate.Values.ErgoTree
import sigma.Extensions._

import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer
import spire.syntax.all.cfor

import scala.collection.mutable

/**
  * Base trait for extra indexer actor and its test.
  */
trait ExtraIndexerBase extends ScorexLogging {

  /**
   * Indexed block height
   */
  protected var indexedHeight: Int = 0

  /**
   * Indexed transaction count
   */
  protected var globalTxIndex: Long = 0L

  /**
   * Indexed box count
   */
  protected var globalBoxIndex: Long = 0L

  /**
   * Last block height when buffer contents were saved to database
   */
  protected var lastWroteToDB: Int = 0

  /**
   * Max buffer size (determined by config)
   */
  protected val saveLimit: Int

  /**
   * Number of transaction/box numberic indexes object segments contain
   */
  protected implicit val segmentTreshold: Int

  /**
   * Address encoder instance
   */
  protected implicit val addressEncoder: ErgoAddressEncoder

  /**
   * Flag to signal when indexer has reached current block height
   */
  protected var caughtUp: Boolean = false

  /**
   * Flag to signal a rollback
   */
  protected var rollback: Boolean = false

  /**
   * Database handle
   */
  protected var _history: ErgoHistory = _

  protected def chainHeight: Int = _history.fullBlockHeight
  protected def history: ErgoHistoryReader = _history.getReader
  protected def historyStorage: HistoryStorage = _history.historyStorage

  // fast access buffers
  protected val general: ArrayBuffer[ExtraIndex] = ArrayBuffer.empty[ExtraIndex]
  protected val boxes: mutable.HashMap[ModifierId,IndexedErgoBox] = mutable.HashMap.empty[ModifierId,IndexedErgoBox]
  protected val trees: mutable.HashMap[ModifierId,IndexedErgoAddress] = mutable.HashMap.empty[ModifierId,IndexedErgoAddress]
  protected val tokens: mutable.HashMap[ModifierId,IndexedToken] = mutable.HashMap.empty[ModifierId,IndexedToken]
  protected val segments: mutable.HashMap[ModifierId,Segment[_]] = mutable.HashMap.empty[ModifierId,Segment[_]]

  /**
   * Input tokens in a transaction
   */
  private val inputTokens: ArrayBuffer[(TokenId, Long)] = ArrayBuffer.empty[(TokenId, Long)]

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
  private def findAndUpdateTree(id: ModifierId, spendOrReceive: Either[IndexedErgoBox, IndexedErgoBox]): Unit = {
    trees.get(id).map { tree =>
      spendOrReceive match {
        case Left(iEb) => tree.addTx(globalTxIndex).spendBox(iEb, Some(history)) // spend box
        case Right(iEb) => tree.addTx(globalTxIndex).addBox(iEb) // receive box
      }
      return
    }
    history.typedExtraIndexById[IndexedErgoAddress](id) match { // address not found in last saveLimit modifiers
      case Some(x) =>
        spendOrReceive match {
          case Left(iEb) => trees.put(id, x.addTx(globalTxIndex).spendBox(iEb, Some(history))) // spend box
          case Right(iEb) => trees.put(id, x.addTx(globalTxIndex).addBox(iEb)) // receive box
        }
      case None => // address not found at all
        spendOrReceive match {
          case Left(iEb) => log.warn(s"Unknown address spent box ${bytesToId(iEb.box.id)}") // spend box should never happen by an unknown address
          case Right(iEb) => trees.put(id, IndexedErgoAddress(id).initBalance.addTx(globalTxIndex).addBox(iEb)) // receive box, new address
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
        log.warn(s"Unknown token $id") // spend box should never happen by an unknown token
    }
  }

  /**
    * @return number of indexes in all buffers
    */
  private def modCount: Int = general.length + boxes.size + trees.size + tokens.size

  /**
    * Write buffered indexes to database and clear buffers.
    */
  private def saveProgress(writeLog: Boolean = true): Unit = {

    val start: Long = System.currentTimeMillis

    // perform segmentation on big addresses and save their internal segment buffer
    trees.values.foreach { tree =>
      if(tree.buffer.nonEmpty) {
        tree.buffer.values.foreach(seg => segments.put(seg.id, seg))
        tree.buffer.clear()
      }
      if(tree.txs.length > segmentTreshold || tree.boxes.length > segmentTreshold)
        tree.splitToSegments.foreach(seg => segments.put(seg.id, seg))
    }

    // perform segmentation on big tokens and save their internal segment buffer
    tokens.values.foreach { token =>
      if(token.buffer.nonEmpty) {
        token.buffer.values.foreach(seg => segments.put(seg.id, seg))
        token.buffer.clear()
      }
      if(token.boxes.length > segmentTreshold)
        token.splitToSegments.foreach(seg => segments.put(seg.id, seg))
    }

    // insert modifiers and progress info to db
    historyStorage.insertExtra(Array((IndexedHeightKey, ByteBuffer.allocate(4).putInt(indexedHeight).array),
                                     (GlobalTxIndexKey, ByteBuffer.allocate(8).putLong(globalTxIndex).array),
                                     (GlobalBoxIndexKey,ByteBuffer.allocate(8).putLong(globalBoxIndex).array)),
      ((((general ++= boxes.values) ++= trees.values) ++= tokens.values) ++= segments.values).toArray)

    if (writeLog)
      log.info(s"Processed ${trees.size} ErgoTrees with ${boxes.size} boxes and inserted them to database in ${System.currentTimeMillis - start}ms")

    // clear buffers for next batch
    general.clear()
    boxes.clear()
    trees.clear()
    tokens.clear()
    segments.clear()

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

      inputTokens.clear()

      //process transaction inputs
      if (height != 1) { //only after 1st block (skip genesis box)
        cfor(0)(_ < tx.inputs.size, _ + 1) { i =>
          val boxId = bytesToId(tx.inputs(i).boxId)
          if (findAndSpendBox(boxId, tx.id, height)) { // spend box and add tx
            val iEb = boxes(boxId)
            findAndUpdateTree(hashErgoTree(iEb.box.ergoTree), Left(iEb))
            cfor(0)(_ < iEb.box.additionalTokens.length, _ + 1) { j =>
              findAndUpdateToken(iEb.box.additionalTokens(j)._1.toModifierId, Left(iEb))
            }
            inputs(i) = iEb.globalIndex
          }
        }
      }

      //process transaction outputs
      cfor(0)(_ < tx.outputs.size, _ + 1) { i =>
        val iEb: IndexedErgoBox = new IndexedErgoBox(height, None, None, tx.outputs(i), globalBoxIndex)
        boxes.put(iEb.id, iEb) // box by id
        general += NumericBoxIndex(globalBoxIndex, iEb.id) // box id by global box number

        // box by address
        findAndUpdateTree(hashErgoTree(iEb.box.ergoTree), Right(boxes(iEb.id)))

        // check if box is creating new tokens, if yes record them
        cfor(0)(_ < iEb.box.additionalTokens.length, _ + 1) { j =>
          if (!inputTokens.exists(x => x._1 == iEb.box.additionalTokens(j)._1)) {
            val token = IndexedToken.fromBox(iEb, j)
            tokens.get(token.tokenId) match {
              case Some(t) => // same new token created in multiple boxes -> add amounts
                val newToken = IndexedToken(t.tokenId, t.boxId, t.amount + token.amount, t.name, t.description, t.decimals, t.boxes)
                newToken.buffer ++= t.buffer
                tokens.put(token.tokenId, newToken)
              case None => tokens.put(token.tokenId, token) // new token
            }
          }
          findAndUpdateToken(iEb.box.additionalTokens(j)._1.toModifierId, Right(iEb))
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

    indexedHeight  = getIndex(IndexedHeightKey, history).getInt
    globalTxIndex  = getIndex(GlobalTxIndexKey, history).getLong
    globalBoxIndex = getIndex(GlobalBoxIndexKey, history).getLong

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
    val txTarget: Long = history.typedExtraIndexById[IndexedErgoTransaction](lastTxToKeep.id).get.globalIndex
    val boxTarget: Long = history.typedExtraIndexById[IndexedErgoBox](bytesToId(lastTxToKeep.outputs.last.id)).get.globalIndex
    val toRemove: ArrayBuffer[ModifierId] = ArrayBuffer.empty[ModifierId]

    // remove all tx indexes
    globalTxIndex -= 1
    while(globalTxIndex > txTarget) {
      val tx: IndexedErgoTransaction = NumericTxIndex.getTxByNumber(history, globalTxIndex).get
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
      toRemove += bytesToId(NumericTxIndex.indexToBytes(globalTxIndex)) // tx id by number
      globalTxIndex -= 1
    }
    globalTxIndex += 1

    // remove all box indexes, tokens and address balances
    globalBoxIndex -= 1
    while(globalBoxIndex > boxTarget) {
      val iEb: IndexedErgoBox = NumericBoxIndex.getBoxByNumber(history, globalBoxIndex).get
      cfor(0)(_ < iEb.box.additionalTokens.length, _ + 1) { i =>
        history.typedExtraIndexById[IndexedToken](IndexedToken.fromBox(iEb, i).id).map { token =>
          if(token.boxId == iEb.id) // token created, delete
            toRemove += token.id
          else // no token created, update
            toRemove ++= token.rollback(txTarget, boxTarget, _history)
        }
      }
      history.typedExtraIndexById[IndexedErgoAddress](hashErgoTree(iEb.box.ergoTree)).map { address =>
        address.spendBox(iEb)
        toRemove ++= address.rollback(txTarget, boxTarget, _history)
      }
      toRemove += iEb.id // box by id
      toRemove += bytesToId(NumericBoxIndex.indexToBytes(globalBoxIndex)) // box id by number
      globalBoxIndex -= 1
    }
    globalBoxIndex += 1

    // Reset indexer flags
    indexedHeight = height
    caughtUp = false
    rollback = false

    // Save changes
    saveProgress(false)
    historyStorage.removeExtra(toRemove.toArray)

    log.info(s"Successfully rolled back indexes to $height")
  }

}


/**
  * Actor that constructs an index of database elements.
  * @param cacheSettings - cacheSettings to use for saveLimit size
  * @param ae - ergo address encoder to use for handling addresses
  */
class ExtraIndexer(cacheSettings: CacheSettings,
                   ae: ErgoAddressEncoder)
  extends Actor with ExtraIndexerBase {

  override val saveLimit: Int = cacheSettings.history.extraCacheSize * 20

  override implicit val segmentTreshold: Int = 512

  override implicit val addressEncoder: ErgoAddressEncoder = ae

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[FullBlockApplied])
    context.system.eventStream.subscribe(self, classOf[Rollback])
    context.system.eventStream.subscribe(self, classOf[StartExtraIndexer])
  }

  override def postStop(): Unit =
    log.info(s"Stopped extra indexer at height ${if(lastWroteToDB > 0) lastWroteToDB else indexedHeight}")

  override def receive: Receive = {

    case FullBlockApplied(header: Header) if caughtUp =>
      index(history.typedModifierById[BlockTransactions](header.transactionsId).get, header.height) // after the indexer caught up with the chain, stay up to date

    case Rollback(branchPoint: ModifierId) if _history != null => // only rollback if indexing is enabled
      val branchHeight: Int = history.heightOf(branchPoint).get
      rollback = branchHeight < indexedHeight
      if(rollback) {
        removeAfter(branchHeight)
        run() // restart indexer
      }

    case StartExtraIndexer(history: ErgoHistory) =>
      _history = history
      run()

    case GetSegmentTreshold =>
      sender ! segmentTreshold
  }
}

object ExtraIndexer {

  type ExtraIndexTypeId = Byte

  object ReceivableMessages {
    /**
      * Initialize ExtraIndexer and start indexing.
      * @param history - handle to database
      */
    case class StartExtraIndexer(history: ErgoHistory)

    /**
      * Retreive the currently used segment treshold
      */
    case class GetSegmentTreshold()
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

  /**
   * Current newest database schema version. Used to force extra database resync.
   */
  val NewestVersion: Int = 4
  val NewestVersionBytes: Array[Byte] = ByteBuffer.allocate(4).putInt(NewestVersion).array

  val IndexedHeightKey: Array[Byte] = Algos.hash("indexed height")
  val GlobalTxIndexKey: Array[Byte] = Algos.hash("txns height")
  val GlobalBoxIndexKey: Array[Byte] = Algos.hash("boxes height")
  val SchemaVersionKey: Array[Byte] = Algos.hash("schema version")

  def getIndex(key: Array[Byte], history: HistoryStorage): ByteBuffer =
    ByteBuffer.wrap(history.modifierBytesById(bytesToId(key)).getOrElse(Array.fill[Byte](8){0}))

  def getIndex(key: Array[Byte], history: ErgoHistoryReader): ByteBuffer = getIndex(key, history.historyStorage)

  def apply(chainSettings: ChainSettings, cacheSettings: CacheSettings)(implicit system: ActorSystem): ActorRef =
    system.actorOf(Props.create(classOf[ExtraIndexer], cacheSettings, chainSettings.addressEncoder))
}

/**
  * Base trait for all additional indexes made by ExtraIndexer
  */
trait ExtraIndex {
  lazy val id: ModifierId = bytesToId(serializedId)
  def serializedId: Array[Byte]
}
