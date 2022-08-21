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
import org.ergoplatform.settings.{Algos, ChainSettings}
import scorex.db.ByteArrayWrapper
import scorex.util.{ScorexLogging, bytesToId}

import java.nio.ByteBuffer
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import spire.syntax.all.cfor

class ExtraIndex(chainSettings: ChainSettings)
  extends Actor with ScorexLogging {

  private val IndexedHeightKey: ByteArrayWrapper = ByteArrayWrapper.apply(Algos.hash("indexed height"))
  private var indexedHeight: Int = 0
  private val indexedHeightBuffer : ByteBuffer = ByteBuffer.allocate(4)

  private val GlobalTxIndexKey: ByteArrayWrapper = ByteArrayWrapper.apply(Algos.hash("txns height"))
  private var globalTxIndex: Long = 0L
  private val globalTxIndexBuffer: ByteBuffer = ByteBuffer.allocate(8)

  private val GlobalBoxIndexKey: ByteArrayWrapper = ByteArrayWrapper.apply(Algos.hash("boxes height"))
  private var globalBoxIndex: Long = 0L
  private val globalBoxIndexBuffer: ByteBuffer = ByteBuffer.allocate(8)

  private val saveLimit: Int = 100

  private var done: Boolean = false

  private def chainHeight: Int = _history.fullBlockHeight

  private var _history: ErgoHistory = null
  private def history: ErgoHistoryReader = _history.asInstanceOf[ErgoHistoryReader]
  private def historyStorage: HistoryStorage = _history.historyStorage

  // fast access
  private val modifiers: ArrayBuffer[BlockSection] = ArrayBuffer.empty[BlockSection]

  // timing
  private var txs: (Long, Long) = (0L, 0L)
  private var boxes: (Long, Long) = (0L, 0L)
  private var write: (Long, Long) = (0L, 0L)

  private def findModifierOpt(id: Array[Byte]): Option[Int] = {
    cfor(modifiers.size - 1)(_ >= 0, _ - 1) { i => // loop backwards == test latest modifiers first
      if(java.util.Arrays.equals(modifiers(i).serializedId, id)) return Some(i)
    }
    None
  }

  private def saveProgress(): Unit = {

    write = write.copy(_1 = System.nanoTime())

    indexedHeightBuffer.clear()
    globalTxIndexBuffer.clear()
    globalBoxIndexBuffer.clear()

    historyStorage.insert(Seq((IndexedHeightKey , indexedHeightBuffer .putInt (indexedHeight ).array),
                              (GlobalTxIndexKey , globalTxIndexBuffer .putLong(globalTxIndex ).array),
                              (GlobalBoxIndexKey, globalBoxIndexBuffer.putLong(globalBoxIndex).array)), modifiers)

    write = write.copy(_2 = System.nanoTime())

    log.info(s"Wrote ${modifiers.size} extra indexes to database in ${write._2 - write._1}ns")

    modifiers.clear()

  }

  private def index(bt: BlockTransactions, height: Int): Unit = {

    //process transactions
    txs = txs.copy(_1 = System.nanoTime())
    cfor(0)(_ < bt.txIds.size, _ + 1) { i =>
      modifiers += IndexedErgoTransaction(bytesToId(bt.txIds(i)), indexedHeight, globalTxIndex)
      modifiers += NumericTxIndex(globalTxIndex, bytesToId(bt.txIds(i)))
      globalTxIndex += 1
    }
    txs = txs.copy(_2 = System.nanoTime())

    boxes = boxes.copy(_1 = System.nanoTime())
    cfor(0)(_ < bt.txs.size, _ + 1) { n =>

      val tx: ErgoTransaction = bt.txs(n)

      //process tx inputs
      if(indexedHeight != 1) { //only after 1st block (skip genesis box)
        cfor(0)(_ < tx.inputs.size, _ + 1) { i =>
          val input: Input = tx.inputs(i)
          findModifierOpt(input.boxId) match {
            case Some(x) => modifiers(x).asInstanceOf[IndexedErgoBox].asSpent(tx.id, indexedHeight) // box found in last saveLimit blocks, update
            case None => // box not found in last saveLimit blocks
              history.typedModifierById[IndexedErgoBox](bytesToId(input.boxId)) match {
                case Some(x) => modifiers += x.asSpent(tx.id, indexedHeight) // box found in DB, update
                case None => log.warn(s"Input for box ${bytesToId(input.boxId)} not found in database") // box not found at all (this shouldn't happen)
              }
          }
        }
      }

      //process tx outputs
      cfor(0)(_ < tx.outputs.size, _ + 1) { i =>
        val box: ErgoBox = tx.outputs(i)
        modifiers += new IndexedErgoBox(Some(indexedHeight), None, None, box, globalBoxIndex, Some(chainHeight - indexedHeight)) // box by id
        modifiers += NumericBoxIndex(globalBoxIndex, bytesToId(box.id)) // box id by global box number
        globalBoxIndex += 1

        // box by ergotree
        val treeHash: Array[Byte] = IndexedErgoTreeSerializer.ergoTreeHash(box.ergoTree)
        findModifierOpt(treeHash) match {
          case Some(x) => modifiers(x).asInstanceOf[IndexedErgoTree].addBox(bytesToId(box.id)) // ergotree found in last saveLimit blocks, update
          case None    => // ergotree not found in last saveLimit blocks
            history.typedModifierById[IndexedErgoTree](bytesToId(treeHash)) match {
              case Some(x) => modifiers += x.addBox(bytesToId(box.id)) // ergotree found in DB, update
              case None    => modifiers += IndexedErgoTree(bytesToId(treeHash), ListBuffer(bytesToId(box.id))) // ergotree not found at all, record
            }
        }

        // box by address
        val addrHash: Array[Byte] = IndexedErgoAddressSerializer.hashAddress(IndexedErgoBoxSerializer.getAddress(box.ergoTree))
        findModifierOpt(addrHash) match {
          case Some(x) => modifiers(x).asInstanceOf[IndexedErgoAddress].addBox(box.id) // address found in last saveLimit blocks, update
          case None => // address not found in last saveLimit blocks
            history.typedModifierById[IndexedErgoAddress](bytesToId(addrHash)) match {
              case Some(x) => modifiers += x.addTx(tx.id).addBox(box.id)//address found in DB, update
              case None => modifiers += IndexedErgoAddress(bytesToId(addrHash), ListBuffer(tx.id), ListBuffer(box.id)) //address not found at all, record
            }
        }
      }

    }
    boxes = boxes.copy(_2 = System.nanoTime())

    log.info(s"Indexed block #$indexedHeight (transactions: ${bt.txs.size} [${txs._2 - txs._1}ns], boxes: ${bt.txs.map(_.outputs.size).sum} [${boxes._2 - boxes._1}ns]) - progress: $indexedHeight / $chainHeight (mods: ${modifiers.size})")

    if(done) indexedHeight = height // update after caught up with chain

    if(indexedHeight % saveLimit == 0 || done) saveProgress() // save index every saveLimit blocks or every block after caught up

  }

  private def run(): Unit = {

    indexedHeight  = ByteBuffer.wrap(historyStorage.getIndex(IndexedHeightKey ).getOrElse(Array.fill[Byte](4){0})).getInt
    globalTxIndex  = ByteBuffer.wrap(historyStorage.getIndex(GlobalTxIndexKey ).getOrElse(Array.fill[Byte](8){0})).getLong
    globalBoxIndex = ByteBuffer.wrap(historyStorage.getIndex(GlobalBoxIndexKey).getOrElse(Array.fill[Byte](8){0})).getLong

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

  def apply(chainSettings: ChainSettings)(implicit system: ActorSystem): ActorRef = {
    val actor = system.actorOf(Props.create(classOf[ExtraIndex], chainSettings))
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
