package org.ergoplatform.nodeView.history.extra

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock}
import org.ergoplatform.{ErgoAddress, ErgoAddressEncoder}
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.ReceivableMessages.Start
import org.ergoplatform.nodeView.history.extra.ExtraIndexerRef.{box_indexNumHash, ergoTreeHash, tx_indexNumHash}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.settings.{Algos, ChainSettings, ErgoAlgos}
import scorex.db.ByteArrayWrapper
import scorex.util.{ScorexLogging, bytesToId}
import sigmastate.Values.ErgoTree

import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer

class ExtraIndex(chainSettings: ChainSettings)
  extends Actor with ScorexLogging {

  private val IndexedHeightKey: ByteArrayWrapper = ByteArrayWrapper.apply(Algos.hash("indexed height"))
  private var indexedHeight: Int = 0

  private val GlobalTxIndexKey: ByteArrayWrapper = ByteArrayWrapper.apply(Algos.hash("txns height"))
  private var globalTxIndex: Long = 0L

  private val GlobalBoxIndexKey: ByteArrayWrapper = ByteArrayWrapper.apply(Algos.hash("boxes height"))
  private var globalBoxIndex: Long = 0L

  private var done: Boolean = false

  private def chainHeight: Int = _history.fullBlockHeight

  private var _history: ErgoHistory = null
  private def history: ErgoHistoryReader = _history.asInstanceOf[ErgoHistoryReader]
  private def historyStorage: HistoryStorage = _history.historyStorage

  private def index(bt: BlockTransactions, height: Int) = {

    var txIndexTip: IndexedErgoTransaction = null
    var boxIndexTip: IndexedErgoBox = null

    //process transactions
    bt.txIds.indices.foreach(n => {
      val tx = IndexedErgoTransaction(bytesToId(bt.txIds(n)), height, globalTxIndex)
      historyStorage.insert(bt.txIds(n), tx.toBytes) // tx by id
      historyStorage.insert(tx_indexNumHash(globalTxIndex), 0.toByte +: bt.txIds(n)) // tx id by global tx number (repend 0 byte to circumvent "removing modifier type byte with .tail")
      globalTxIndex += 1
      txIndexTip = tx
    })

    bt.txs.foreach(tx => {

      //process tx inputs
      if(height != 1) { //only after 1st block (skip genesis box)
        tx.inputs.indices.foreach(n => {
          val id: BoxId = tx.inputs(n).boxId
          history.typedModifierById[IndexedErgoBox](bytesToId(id)) match {
            case Some(iEb) =>
              boxIndexTip = iEb.asSpent(tx.id, height)
              historyStorage.insert(id, boxIndexTip.toBytes) // box by id
            case None => log.warn(s"Input for box ${ErgoAlgos.encode(id)} not found in database (this shouldn't happen)")
          }
        })
      }

      //process tx outputs
      var outputs: ArrayBuffer[IndexedErgoBox] = ArrayBuffer.empty[IndexedErgoBox]
      tx.outputs.indices.foreach(n => {
        boxIndexTip = new IndexedErgoBox(Some(height), None, None, tx.outputs(n), globalBoxIndex, Some(chainHeight - height))
        historyStorage.insert(boxIndexTip.serializedId, boxIndexTip.toBytes) // box by id
        historyStorage.insert(box_indexNumHash(globalBoxIndex), 0.toByte +: boxIndexTip.serializedId) // box id by global box number (repend 0 byte to circumvent "removing modifier type byte with .tail")
        val iEt: IndexedErgoTree = history.typedModifierById[IndexedErgoTree](bytesToId(ergoTreeHash(boxIndexTip.box.ergoTree))) match {
          case Some(x) => IndexedErgoTree(x.treeHash, x.boxIds :+ bytesToId(boxIndexTip.box.id)) // ergotree found, update
          case None => IndexedErgoTree(bytesToId(ergoTreeHash(boxIndexTip.box.ergoTree)), Seq(bytesToId(boxIndexTip.box.id))) // ergotree not found, record
        }
        historyStorage.insert(iEt.serializedId, iEt.getBytes) // box id by ergotree
        outputs :+= boxIndexTip
        globalBoxIndex += 1
      })

      //process boxes by address
      val boxesGroupedByAddress : Array[(ErgoAddress, Seq[BoxId])] = outputs.map(x => (x.getAddress, x.box.id)).groupBy(_._1).mapValues(_.map(_._2).toSeq).toArray
      for(addressWithBoxes <- boxesGroupedByAddress) {
        val addr: IndexedErgoAddress =
          history.typedModifierById[IndexedErgoAddress](IndexedErgoAddressSerializer.addressToModifierId(addressWithBoxes._1)) match {
            case Some(iEa) => new IndexedErgoAddress(addressWithBoxes._1, iEa.txIds :+ tx.id, iEa.boxIds ++ addressWithBoxes._2) //address found, update
            case None      => new IndexedErgoAddress(addressWithBoxes._1, Seq(tx.id), addressWithBoxes._2) //address not found, record
          }
        historyStorage.insert(addr.serializedId, addr.getBytes)
      }

    })

    log.info(s"Indexed block #$height [transactions: ${bt.txs.size}, boxes: ${bt.txs.map(_.outputs.size).sum}] - progress: $height / $chainHeight")
    if(done) indexedHeight = height // after the indexer caught up with the chain height gets updated here
    writeProgress()
  }

  private val empty: Seq[BlockSection] = Seq.empty[BlockSection]
  private def writeProgress() = {
    historyStorage.insert(Seq((IndexedHeightKey , ByteBuffer.allocate(4).putInt (indexedHeight ).array),
                              (GlobalTxIndexKey , ByteBuffer.allocate(8).putLong(globalTxIndex ).array),
                              (GlobalBoxIndexKey, ByteBuffer.allocate(8).putLong(globalBoxIndex).array)), empty)
  }

  private def run() = {

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
    if(indexedHeight != 0) // only after done or stopped, not at shutdown
      log.info(s"Stopped extra indexer at height ${indexedHeight - 1}")
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(fb: ErgoFullBlock) => if(done) index(fb.blockTransactions, fb.height) // after the indexer caught up with the chain, stay up to date
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

  def tx_indexNumHash(index: Long): Array[Byte] = Algos.hash("txns height " + index)
  def box_indexNumHash(index: Long): Array[Byte] = Algos.hash("boxes height " + index)

  def ergoTreeHash(tree: ErgoTree): Array[Byte] = Algos.hash(tree.bytes)
}

object ExtraIndexerRefHolder {

  private var _actor: Option[ActorRef] = None
  private var running: Boolean = false

  def start(history: ErgoHistory) = if(_actor.isDefined && !running) {
    _actor.get ! Start(history)
    running = true
  }

  protected[extra] def init(actor: ActorRef) = _actor = Some(actor)
}
