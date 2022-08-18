package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.ReceivableMessages.Start
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.{Algos, ChainSettings, ErgoAlgos}
import scorex.db.ByteArrayWrapper
import scorex.util.{ScorexLogging, bytesToId}

import java.nio.ByteBuffer
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

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

  private val saveLimit: Int = 10

  private var done: Boolean = false

  private def chainHeight: Int = _history.fullBlockHeight

  private var _history: ErgoHistory = null
  private def history: ErgoHistoryReader = _history.asInstanceOf[ErgoHistoryReader]
  private def historyStorage: HistoryStorage = _history.historyStorage

  // fast access
  private val modifiers: ArrayBuffer[BlockSection] = ArrayBuffer.empty[BlockSection]
  private val boxesGroupedByAddress: mutable.HashMap[Array[Byte], ListBuffer[BoxId]] = mutable.HashMap.empty[Array[Byte], ListBuffer[BoxId]]
  private val emptyBoxBuffer: ListBuffer[BoxId] = ListBuffer.empty[BoxId]

  private def findModifierOpt(id: Array[Byte]): Option[Int] =
    modifiers.indexWhere(x => java.util.Arrays.equals(x.serializedId, id)) match {
      case x: Int if x >= 0 => Some(x)
      case _ => None
    }

  private def saveProgress(): Unit = {
    indexedHeightBuffer.clear()
    globalTxIndexBuffer.clear()
    globalBoxIndexBuffer.clear()

    historyStorage.insert(Seq((IndexedHeightKey , indexedHeightBuffer .putInt (indexedHeight ).array),
                              (GlobalTxIndexKey , globalTxIndexBuffer .putLong(globalTxIndex ).array),
                              (GlobalBoxIndexKey, globalBoxIndexBuffer.putLong(globalBoxIndex).array)), modifiers)

    modifiers.clear()
    boxesGroupedByAddress.clear()
  }

  private def index(bt: BlockTransactions, height: Int): Unit = {

    //process transactions
    bt.txIds.foreach(id => {
      modifiers += IndexedErgoTransaction(bytesToId(id), indexedHeight, globalTxIndex)
      modifiers += NumericTxIndex(globalTxIndex, bytesToId(id))
      globalTxIndex += 1
    })

    bt.txs.foreach(tx => {

      //process tx inputs
      if(indexedHeight != 1) { //only after 1st block (skip genesis box)
        tx.inputs.foreach(in =>
          findModifierOpt(in.boxId) match {
            case Some(n) => modifiers(n).asInstanceOf[IndexedErgoBox].asSpent(tx.id, indexedHeight) // box found in last saveLimit blocks, update
            case None    => // box not found in this block
              history.typedModifierById[IndexedErgoBox](bytesToId(in.boxId)) match {
                case Some(x) => modifiers += x.asSpent(tx.id, indexedHeight) // box found in DB, update
                case None    => log.warn(s"Input for box ${ErgoAlgos.encode(in.boxId)} not found in database") // box not found at all (this shouldn't happen)
              }
          }
        )
      }

      //process tx outputs
      tx.outputs.foreach(box => {
        modifiers += new IndexedErgoBox(Some(indexedHeight), None, None, box, globalBoxIndex, Some(chainHeight - indexedHeight)) // box by id
        modifiers += NumericBoxIndex(globalBoxIndex, bytesToId(box.id)) // box id by global box number
        var tmp: Array[Byte] = IndexedErgoTreeSerializer.ergoTreeHash(box.ergoTree)
        findModifierOpt(tmp) match {
          case Some(n) => modifiers(n).asInstanceOf[IndexedErgoTree].addBox(bytesToId(box.id)) // ergotree found in last saveLimit blocks, update
          case None    => // ergotree not found in this TX
            history.typedModifierById[IndexedErgoTree](bytesToId(tmp)) match {
              case Some(x) => modifiers += x.addBox(bytesToId(box.id)) // ergotree found in DB, update
              case None    => modifiers += IndexedErgoTree(bytesToId(tmp), ListBuffer(bytesToId(box.id))) // ergotree not found at all, record
            }
        }
        globalBoxIndex += 1
        tmp = IndexedErgoAddressSerializer.hashAddress(IndexedErgoBoxSerializer.getAddress(box.ergoTree))
        boxesGroupedByAddress.put(tmp, boxesGroupedByAddress.getOrElse[ListBuffer[BoxId]](tmp, emptyBoxBuffer) :+ box.id)
      })

      //process boxes by address
      for(addressWithBoxes <- boxesGroupedByAddress)
        findModifierOpt(addressWithBoxes._1) match {
          case Some(n) => modifiers(n).asInstanceOf[IndexedErgoAddress].addTx(tx.id).addBoxes(addressWithBoxes._2) // address found in last saveLimit blocks, update
          case None    => // address not found in this block
            history.typedModifierById[IndexedErgoAddress](bytesToId(addressWithBoxes._1)) match {
              case Some(x) => modifiers += x.addTx(tx.id).addBoxes(addressWithBoxes._2) //address found in DB, update
              case None    => modifiers += IndexedErgoAddress(bytesToId(addressWithBoxes._1), ListBuffer(tx.id), addressWithBoxes._2) //address not found at all, record
            }
        }
    })

    log.info(s"Indexed block #$indexedHeight [transactions: ${bt.txs.size}, boxes: ${bt.txs.map(_.outputs.size).sum}] - progress: $indexedHeight / $chainHeight")

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
