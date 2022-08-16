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
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import java.nio.ByteBuffer
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

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

  private var done: Boolean = false

  private def chainHeight: Int = _history.fullBlockHeight

  private var _history: ErgoHistory = null
  private def history: ErgoHistoryReader = _history.asInstanceOf[ErgoHistoryReader]
  private def historyStorage: HistoryStorage = _history.historyStorage

  // fast access
  private val modifiers: ArrayBuffer[BlockSection] = ArrayBuffer.empty[BlockSection]
  private val boxesGroupedByAddress: mutable.HashMap[ModifierId, Seq[BoxId]] = mutable.HashMap.empty[ModifierId, Seq[BoxId]]
  private val emptyBoxSeq: Seq[BoxId] = Seq.empty[BoxId]

  // if any element "e" of type "T" satisfies "f(e) == true", the element and its index in the buffer are returned
  private def updateMatch[T <: BlockSection : ClassTag](f: T => Boolean): Option[(T, Int)] = {
    modifiers.indices.foreach({ n =>
      modifiers(n) match {
        case e: T if f(e) => return Some((e, n))
        case _ =>
      }
    })
    None
  }

  private def index(bt: BlockTransactions, height: Int) = {

    modifiers.clear()
    boxesGroupedByAddress.clear()

    //process transactions
    bt.txIds.indices.foreach(n => {
      modifiers += IndexedErgoTransaction(bytesToId(bt.txIds(n)), indexedHeight, globalTxIndex)
      modifiers += new NumericTxIndex(globalTxIndex, bytesToId(bt.txIds(n)))
      globalTxIndex += 1
    })

    bt.txs.foreach(tx => {

      //process tx inputs
      if(indexedHeight != 1) { //only after 1st block (skip genesis box)
        tx.inputs.foreach(in =>
          updateMatch[IndexedErgoBox](x => java.util.Arrays.equals(x.box.id, in.boxId)) match {
            case Some((iEb, n)) => modifiers.update(n, iEb.asSpent(tx.id, indexedHeight)) // box found in this block, update
            case None      => // box not found in this block
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
        modifiers += new NumericBoxIndex(globalBoxIndex, bytesToId(box.id)) // box id by global box number
        val maybeNewTree: ModifierId = bytesToId(IndexedErgoTreeSerializer.ergoTreeHash(box.ergoTree))
        updateMatch[IndexedErgoTree](_.treeHash.eq(maybeNewTree)) match {
          case Some((iEt, n)) => modifiers.update(n, IndexedErgoTree(iEt.treeHash, iEt.boxIds :+ bytesToId(box.id))) // ergotree found in this block, update
          case None      => // ergotree not found in this TX
            history.typedModifierById[IndexedErgoTree](maybeNewTree) match {
              case Some(x) => modifiers += IndexedErgoTree(x.treeHash, x.boxIds :+ bytesToId(box.id)) // ergotree found in DB, update
              case None    => modifiers += IndexedErgoTree(maybeNewTree, Seq(bytesToId(box.id))) // ergotree not found at all, record
            }
        }
        globalBoxIndex += 1
        val addrHash: ModifierId = IndexedErgoAddressSerializer.addressToModifierId(IndexedErgoBoxSerializer.getAddress(box.ergoTree))
        boxesGroupedByAddress.put(addrHash, boxesGroupedByAddress.getOrElse[Seq[BoxId]](addrHash, emptyBoxSeq) :+ box.id)
      })

      //process boxes by address
      for(addressWithBoxes <- boxesGroupedByAddress)
        updateMatch[IndexedErgoAddress](_.addressHash.eq(addressWithBoxes._1)) match {
          case Some((iEa, n)) => modifiers.update(n, IndexedErgoAddress(addressWithBoxes._1, iEa.txIds :+ tx.id, iEa.boxIds ++ addressWithBoxes._2)) // address found in this block, update
          case None      => // address not found in this block
            history.typedModifierById[IndexedErgoAddress](addressWithBoxes._1) match {
              case Some(x) => modifiers += IndexedErgoAddress(addressWithBoxes._1, x.txIds :+ tx.id, x.boxIds ++ addressWithBoxes._2) //address found in DB, update
              case None    => modifiers += IndexedErgoAddress(addressWithBoxes._1, Seq(tx.id), addressWithBoxes._2) //address not found at all, record
            }
        }
    })

    log.info(s"Indexed block #$indexedHeight [transactions: ${bt.txs.size}, boxes: ${bt.txs.map(_.outputs.size).sum}] - progress: $indexedHeight / $chainHeight")

    if(done) indexedHeight = height // update after caught up with chain

    indexedHeightBuffer.clear()
    globalTxIndexBuffer.clear()
    globalBoxIndexBuffer.clear()

    historyStorage.insert(Seq((IndexedHeightKey , indexedHeightBuffer .putInt (indexedHeight ).array),
                              (GlobalTxIndexKey , globalTxIndexBuffer .putLong(globalTxIndex ).array),
                              (GlobalBoxIndexKey, globalBoxIndexBuffer.putLong(globalBoxIndex).array)), modifiers)

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
