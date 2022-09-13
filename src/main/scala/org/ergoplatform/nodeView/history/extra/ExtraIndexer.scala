package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoBox.{BoxId, TokenId}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.{GlobalBoxIndexKey, GlobalTxIndexKey, IndexedHeightKey}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.ReceivableMessages.Start
import org.ergoplatform.nodeView.history.extra.IndexedErgoAddress.segmentTreshold
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.{Algos, CacheSettings, ChainSettings}
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import java.nio.ByteBuffer
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import spire.syntax.all.cfor

class ExtraIndex(chainSettings: ChainSettings, cacheSettings: CacheSettings)
  extends Actor with ScorexLogging {

  private var indexedHeight: Int = 0
  private val indexedHeightBuffer : ByteBuffer = ByteBuffer.allocate(4)

  private var globalTxIndex: Long = 0L
  private val globalTxIndexBuffer: ByteBuffer = ByteBuffer.allocate(8)

  private var globalBoxIndex: Long = 0L
  private val globalBoxIndexBuffer: ByteBuffer = ByteBuffer.allocate(8)

  private var lastWroteToDB: Int = 0

  private val saveLimit: Int = cacheSettings.history.extraCacheSize * 10

  private var caughtUp: Boolean = false

  private def chainHeight: Int = _history.fullBlockHeight

  private var _history: ErgoHistory = null
  private def history: ErgoHistoryReader = _history.getReader
  private def historyStorage: HistoryStorage = _history.historyStorage

  // fast access
  private val general: ArrayBuffer[BlockSection] = ArrayBuffer.empty[BlockSection]
  private val boxes: ArrayBuffer[IndexedErgoBox] = ArrayBuffer.empty[IndexedErgoBox]
  private val trees: ArrayBuffer[IndexedErgoAddress] = ArrayBuffer.empty[IndexedErgoAddress]

  // input tokens in a tx
  private val tokens: ArrayBuffer[(TokenId, Long)] = ArrayBuffer.empty[(TokenId, Long)]

  // returns index of box in boxes
  private def findAndSpendBox(id: BoxId, txId: ModifierId, height: Int): Int = {
    cfor(boxes.length - 1)(_ >= 0, _ - 1) { i => // loop backwards to test latest modifiers first
      if(java.util.Arrays.equals(boxes(i).serializedId, id)) { // box found in last saveLimit modifiers, update
        tokens ++= boxes(i).asSpent(txId, height).box.additionalTokens.toArray
        return i
      }
    }
    history.typedModifierById[IndexedErgoBox](bytesToId(id)) match { // box not found in last saveLimit modifiers
      case Some(x) => // box found in DB, update
        boxes += x.asSpent(txId, height)
        tokens ++= x.box.additionalTokens.toArray
        boxes.length - 1
      case None => // box not found at all (this shouldn't happen)
        log.warn(s"Unknown box used as input: ${bytesToId(id)}")
        -1
    }
  }

  private def findAndUpdateTree(id: ModifierId, boxToSpend: Option[ErgoBox]): Unit = {
    cfor(trees.length - 1)(_ >= 0, _ - 1) { i => // loop backwards to test latest modifiers first
      if(trees(i).treeHash == id) { // address found in last saveLimit modifiers
        if(boxToSpend.isDefined)
          trees(i).addTx(globalTxIndex).spendBox(boxToSpend.get) // spend box
        else
          trees(i).addTx(globalTxIndex).addBox(boxes.last) // receive box
        return
      }
    }
    history.typedModifierById[IndexedErgoAddress](id) match { // address not found in last saveLimit modifiers
      case Some(x) =>
        if(boxToSpend.isDefined) // address found in DB
          trees += x.addTx(globalTxIndex).spendBox(boxToSpend.get) // spend box
        else
          trees += x.addTx(globalTxIndex).addBox(boxes.last) // receive box
      case None => // address not found at all
        if(boxToSpend.isEmpty)
          trees += IndexedErgoAddress(id, ListBuffer(globalTxIndex), ListBuffer.empty[Long], Some(new BalanceInfo)).addBox(boxes.last) // receive box
        else
          log.warn(s"Unknown address spent box ${boxes.last.id}") // spend box should never happen by an unknown address
    }
  }

  private def modCount: Int = general.length + boxes.length + trees.length

  private def saveProgress(): Unit = {

    val start: Long = System.nanoTime()

    // perform segmentation on big modifiers
    val addressesLen: Int = trees.length
    cfor(0)(_ < addressesLen, _ + 1) { i =>
      if(trees(i).txs.length > segmentTreshold || trees(i).boxes.length > segmentTreshold) trees ++= trees(i).splitToSegment()
    }

    // merge all modifiers to an Array, avoids reallocations durin concatenation (++)
    val all: Array[BlockSection] = new Array[BlockSection](modCount)
    val offset: Array[Int] = Array(0, general.length, general.length + boxes.length)
    cfor(0)(_ < general.length, _ + 1) { i => all(i + offset(0)) = general(i) }
    cfor(0)(_ < boxes.length  , _ + 1) { i => all(i + offset(1)) = boxes(i) }
    cfor(0)(_ < trees.length  , _ + 1) { i => all(i + offset(2)) = trees(i) }

    // insert modifiers and progress info to db
    indexedHeightBuffer.clear()
    globalTxIndexBuffer.clear()
    globalBoxIndexBuffer.clear()
    historyStorage.insertExtra(Array((IndexedHeightKey , indexedHeightBuffer .putInt (indexedHeight ).array),
                                     (GlobalTxIndexKey , globalTxIndexBuffer .putLong(globalTxIndex ).array),
                                     (GlobalBoxIndexKey, globalBoxIndexBuffer.putLong(globalBoxIndex).array)), all)

    val end: Long = System.nanoTime()

    log.info(s"Processed ${trees.length} ErgoTrees with ${boxes.length} boxes and inserted them to database in ${(end - start) / 1000000D}ms")

    // clear buffers for next batch
    general.clear()
    boxes.clear()
    trees.clear()

    lastWroteToDB = indexedHeight
  }

  private def index(bt: BlockTransactions, height: Int): Unit = {

    if(caughtUp && height <= indexedHeight) return // do not process older blocks again after caught up (due to actor message queue)

    var boxCount: Int = 0

    cfor(0)(_ < bt.txs.size, _ + 1) { n =>

      val tx: ErgoTransaction = bt.txs(n)

      //process transaction
      general += IndexedErgoTransaction(tx.id, height, globalTxIndex)
      general += NumericTxIndex(globalTxIndex, tx.id)

      tokens.clear()

      //process transaction inputs
      if(height != 1) { //only after 1st block (skip genesis box)
        cfor(0)(_ < tx.inputs.size, _ + 1) { i =>
          val boxIndex: Int = findAndSpendBox(tx.inputs(i).boxId, tx.id, height)
          if(boxIndex >= 0) findAndUpdateTree(bytesToId(IndexedErgoAddressSerializer.hashErgoTree(boxes(boxIndex).box.ergoTree)), Some(boxes(boxIndex).box)) // spend box and add tx
        }
      }

      //process transaction outputs
      cfor(0)(_ < tx.outputs.size, _ + 1) { i =>
        val box: ErgoBox = tx.outputs(i)
        boxes += new IndexedErgoBox(Some(height), None, None, box, globalBoxIndex) // box by id
        general += NumericBoxIndex(globalBoxIndex, bytesToId(box.id)) // box id by global box number

        // box by address
        findAndUpdateTree(bytesToId(IndexedErgoAddressSerializer.hashErgoTree(box.ergoTree)), None)

        // check if box is creating a new token, if yes record it
        if(box.additionalTokens.length > 0 && IndexedTokenSerializer.tokenRegistersSet(box))
          cfor(0)(_ < box.additionalTokens.length, _ + 1) { j =>
            if(!tokens.exists(x => java.util.Arrays.equals(x._1, box.additionalTokens(j)._1))) {
              general += IndexedTokenSerializer.fromBox(box)
            }
          }

        globalBoxIndex += 1
        boxCount += 1

      }

      globalTxIndex += 1

    }

    log.info(s"Buffered block #$height / $chainHeight [txs: ${bt.txs.length}, boxes: $boxCount] (buffer: $modCount / $saveLimit)")

    if(caughtUp) {

      indexedHeight = height // update height here after caught up with chain

      if(modCount >= saveLimit || // modifier limit reached to write to db
         history.fullBlockHeight == history.headersHeight) // write to db every block after caught up
        saveProgress()

    }else if(modCount >= saveLimit) saveProgress() // active syncing, write to db after modifier limit

  }

  private def run(): Unit = {

    indexedHeight  = ByteBuffer.wrap(history.modifierBytesById(bytesToId(IndexedHeightKey)) .getOrElse(Array.fill[Byte](4){0})).getInt
    globalTxIndex  = ByteBuffer.wrap(history.modifierBytesById(bytesToId(GlobalTxIndexKey)) .getOrElse(Array.fill[Byte](8){0})).getLong
    globalBoxIndex = ByteBuffer.wrap(history.modifierBytesById(bytesToId(GlobalBoxIndexKey)).getOrElse(Array.fill[Byte](8){0})).getLong

    log.info(s"Started extra indexer at height $indexedHeight")

    while(indexedHeight < chainHeight) {
      indexedHeight += 1
      index(history.bestBlockTransactionsAt(indexedHeight).get, indexedHeight)
    }

    caughtUp = true

    log.info("Indexer caught up with chain")
  }

  override def preStart(): Unit =
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])

  override def postStop(): Unit =
    log.info(s"Stopped extra indexer at height $lastWroteToDB")

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(fb: ErgoFullBlock) if caughtUp =>
      index(fb.blockTransactions, fb.height) // after the indexer caught up with the chain, stay up to date
    case Start(history: ErgoHistory) =>
      _history = history
      run()
  }
}

object ExtraIndexerRef {

  object ReceivableMessages {
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

  // faster id to bytes - no safety checks
  private[extra] def fastIdToBytes(id: ModifierId): Array[Byte] = {
    val x: Array[Byte] = new Array[Byte](id.length / 2)
    cfor(0)(_ < id.length, _ + 2) {i => x(i / 2) = ((hexIndex(id(i)) << 4) | hexIndex(id(i + 1))).toByte}
    x
  }

  val IndexedHeightKey: Array[Byte] = Algos.hash("indexed height")
  val GlobalTxIndexKey: Array[Byte] = Algos.hash("txns height")
  val GlobalBoxIndexKey: Array[Byte] = Algos.hash("boxes height")

  def apply(chainSettings: ChainSettings, cacheSettings: CacheSettings)(implicit system: ActorSystem): ActorRef = {
    val actor = system.actorOf(Props.create(classOf[ExtraIndex], chainSettings, cacheSettings))
    _ae = chainSettings.addressEncoder
    ExtraIndexerRefHolder.init(actor)
    actor
  }
}

object ExtraIndexerRefHolder {

  private var _actor: Option[ActorRef] = None
  private var running: Boolean = false

  private[history] def start(history: ErgoHistory): Unit = if(_actor.isDefined && !running) {
    _actor.get ! Start(history)
    running = true
  }

  private[extra] def init(actor: ActorRef): Unit = _actor = Some(actor)
}
