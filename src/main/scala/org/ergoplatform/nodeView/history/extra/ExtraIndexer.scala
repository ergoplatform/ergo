package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox, Input}
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.ReceivableMessages.Start
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.{Algos, CacheSettings, ChainSettings}
import scorex.db.ByteArrayWrapper
import scorex.util.{ScorexLogging, bytesToId}

import java.nio.ByteBuffer
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import spire.syntax.all.cfor

class ExtraIndex(chainSettings: ChainSettings, cacheSettings: CacheSettings)
  extends Actor with ScorexLogging {

  private val IndexedHeightKey: Array[Byte] = ByteArrayWrapper.apply(Algos.hash("indexed height")).data
  private var indexedHeight: Int = 0
  private val indexedHeightBuffer : ByteBuffer = ByteBuffer.allocate(4)

  private val GlobalTxIndexKey: Array[Byte] = ByteArrayWrapper.apply(Algos.hash("txns height")).data
  private var globalTxIndex: Long = 0L
  private val globalTxIndexBuffer: ByteBuffer = ByteBuffer.allocate(8)

  private val GlobalBoxIndexKey: Array[Byte] = ByteArrayWrapper.apply(Algos.hash("boxes height")).data
  private var globalBoxIndex: Long = 0L
  private val globalBoxIndexBuffer: ByteBuffer = ByteBuffer.allocate(8)

  private val saveLimit: Int = cacheSettings.history.extraCacheSize * 10

  private var done: Boolean = false

  private def chainHeight: Int = _history.fullBlockHeight

  private var _history: ErgoHistory = null
  private def history: ErgoHistoryReader = _history.asInstanceOf[ErgoHistoryReader]
  private def historyStorage: HistoryStorage = _history.historyStorage

  // fast access
  private val modifiers: ArrayBuffer[BlockSection] = ArrayBuffer.empty[BlockSection]

  private def findModifierOpt(id: Array[Byte]): Option[Int] = {
    cfor(modifiers.length - 1)(_ >= 0, _ - 1) { i => // loop backwards to test latest modifiers first
      if(java.util.Arrays.equals(modifiers(i).serializedId, id)) return Some(i)
    }
    None
  }

  private def saveProgress(): Unit = {

    val start: Long = System.nanoTime()

    // perform segmentation on big modifiers
    val len: Int = modifiers.length
    cfor(0)(_ < len, _ + 1) { i =>
      modifiers(i) match {
        case m: IndexedErgoAddress if m.txs.length > IndexedErgoAddress.segmentTreshold && m.boxes.length > IndexedErgoAddress.segmentTreshold =>
          modifiers += modifiers(i).asInstanceOf[IndexedErgoAddress].splitToSegment()
        case m: IndexedErgoTree if m.boxes.length > IndexedErgoTree.segmentTreshold =>
          modifiers += modifiers(i).asInstanceOf[IndexedErgoTree].splitToSegment()
        case _ =>
      }
    }

    // insert modifiers and progress info to db
    indexedHeightBuffer.clear()
    globalTxIndexBuffer.clear()
    globalBoxIndexBuffer.clear()
    historyStorage.insertExtra(Array((IndexedHeightKey , indexedHeightBuffer .putInt (indexedHeight ).array),
                                     (GlobalTxIndexKey , globalTxIndexBuffer .putLong(globalTxIndex ).array),
                                     (GlobalBoxIndexKey, globalBoxIndexBuffer.putLong(globalBoxIndex).array)), modifiers.toArray)

    val end: Long = System.nanoTime()

    log.info(s"Wrote ${modifiers.size} extra indexes to database in ${(end - start) / 1000000000D}s")

    modifiers.clear()

  }

  private def index(bt: BlockTransactions, height: Int): Unit = {

    cfor(0)(_ < bt.txs.size, _ + 1) { n =>

      val tx: ErgoTransaction = bt.txs(n)

      //process transaction
      modifiers += IndexedErgoTransaction(tx.id, indexedHeight, globalTxIndex)
      modifiers += NumericTxIndex(globalTxIndex, tx.id)

      //process transaction inputs
      if(indexedHeight != 1) { //only after 1st block (skip genesis box)
        cfor(0)(_ < tx.inputs.size, _ + 1) { i =>
          val input: Input = tx.inputs(i)
          findModifierOpt(input.boxId) match {
            case Some(x) => modifiers(x).asInstanceOf[IndexedErgoBox].asSpent(tx.id, indexedHeight) // box found in last saveLimit modifiers, update
            case None => // box not found in last saveLimit blocks
              history.typedModifierById[IndexedErgoBox](bytesToId(input.boxId)) match {
                case Some(x) => modifiers += x.asSpent(tx.id, indexedHeight) // box found in DB, update
                case None => log.warn(s"Input for box ${bytesToId(input.boxId)} not found in database") // box not found at all (this shouldn't happen)
              }
          }
        }
      }

      //process transaction outputs
      cfor(0)(_ < tx.outputs.size, _ + 1) { i =>
        val box: ErgoBox = tx.outputs(i)
        modifiers += new IndexedErgoBox(Some(indexedHeight), None, None, box, globalBoxIndex, Some(chainHeight - indexedHeight)) // box by id
        modifiers += NumericBoxIndex(globalBoxIndex, bytesToId(box.id)) // box id by global box number

        // box by ergotree
        val treeHash: Array[Byte] = IndexedErgoTreeSerializer.ergoTreeHash(box.ergoTree)
        findModifierOpt(treeHash) match {
          case Some(x) => modifiers(x).asInstanceOf[IndexedErgoTree].addBox(globalBoxIndex) // ergotree found in last saveLimit modifiers, update
          case None    => // ergotree not found in last saveLimit blocks
            history.typedModifierById[IndexedErgoTree](bytesToId(treeHash)) match {
              case Some(x) => modifiers += x.addBox(globalBoxIndex) // ergotree found in DB, update
              case None    => modifiers += IndexedErgoTree(bytesToId(treeHash), ListBuffer(globalBoxIndex)) // ergotree not found at all, record
            }
        }

        // box by address
        val addrHash: Array[Byte] = IndexedErgoAddressSerializer.hashAddress(IndexedErgoBoxSerializer.getAddress(box.ergoTree))
        findModifierOpt(addrHash) match {
          case Some(x) => modifiers(x).asInstanceOf[IndexedErgoAddress].addTx(globalTxIndex).addBox(globalBoxIndex) // address found in last saveLimit modifiers, update
          case None => // address not found in last saveLimit blocks
            history.typedModifierById[IndexedErgoAddress](bytesToId(addrHash)) match {
              case Some(x) => modifiers += x.addTx(globalTxIndex).addBox(globalBoxIndex)//address found in DB, update
              case None => modifiers += IndexedErgoAddress(bytesToId(addrHash), ListBuffer(globalTxIndex), ListBuffer(globalBoxIndex)) //address not found at all, record
            }
        }

        // TODO token indexing

        globalBoxIndex += 1

      }

      globalTxIndex += 1

    }

    log.info(s"Indexed block #$indexedHeight [transactions: ${bt.txs.size}, boxes: ${bt.txs.map(_.outputs.size).sum}] - progress: $indexedHeight / $chainHeight (mods: ${modifiers.size})")

    if(done) indexedHeight = height // update height here after caught up with chain

    if(modifiers.length >= saveLimit || done) saveProgress() // save index every saveLimit modifiers or every block after caught up

  }

  private def run(): Unit = {

    indexedHeight  = ByteBuffer.wrap(historyStorage.get(bytesToId(IndexedHeightKey)) .getOrElse(Array.fill[Byte](4){0})).getInt
    globalTxIndex  = ByteBuffer.wrap(historyStorage.get(bytesToId(GlobalTxIndexKey)) .getOrElse(Array.fill[Byte](8){0})).getLong
    globalBoxIndex = ByteBuffer.wrap(historyStorage.get(bytesToId(GlobalBoxIndexKey)).getOrElse(Array.fill[Byte](8){0})).getLong

    log.info(s"Started extra indexer at height $indexedHeight")

    while(indexedHeight < chainHeight) {
      indexedHeight += 1
      index(history.bestBlockTransactionsAt(indexedHeight).get, indexedHeight)
    }

    done = true

    log.info("Indexer caught up with chain")
  }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier])
  }

  override def postStop(): Unit = {
    log.info(s"Stopped extra indexer at height ${indexedHeight - 1}")
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(fb: ErgoFullBlock) =>
      if(done) // after the indexer caught up with the chain, stay up to date
        index(fb.blockTransactions, fb.height)
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

  def start(history: ErgoHistory): Unit = if(_actor.isDefined && !running) {
    _actor.get ! Start(history)
    running = true
  }

  protected[extra] def init(actor: ActorRef): Unit = _actor = Some(actor)
}
