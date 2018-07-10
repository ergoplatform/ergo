package org.ergoplatform.nodeView.wallet

import akka.actor.Actor
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import scorex.crypto.authds.ADDigest
import sigmastate.interpreter.ContextExtension
import sigmastate.{AvlTreeData, Values}

import scala.collection.mutable
import scala.util.{Failure, Success}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


case class BoxUncertain(tx: ErgoTransaction, outIndex: Short, heightOpt: Option[Height]) {
  lazy val onChain: Boolean = heightOpt.isDefined

  lazy val box: ErgoBox = tx.outputCandidates.apply(outIndex).toBox(tx.id, outIndex)
}

case class BoxCertain(tx: ErgoTransaction, outIndex: Short, ergoValue: Long, assets: Map[ByteArrayWrapper, Long])



class ErgoWalletActor(seed: String) extends Actor {
  import ErgoWalletActor._

  private val prover = new ErgoProvingInterpreter(seed)

  private var height = 0
  private var lastBlockUtxoRootHash = ADDigest @@ Array.fill(32)(0: Byte)

  private val toTrack = prover.dlogSecrets.map(prover.bytesToTrack)

  private val quickScan = mutable.Queue[BoxUncertain]()

  private val certainOffChain = mutable.LongMap[BoxCertain]()
  private val certainOnChain = mutable.LongMap[BoxCertain]()
  private val confirmedIndex = mutable.TreeMap[Height, Seq[ByteArrayWrapper]]()

  private var balance: Long = 0
  private var assetBalances: mutable.Map[ByteArrayWrapper, Long] = mutable.Map()

  private var unconfirmedBalance: Long = 0
  private var unconfirmedAssetBalances: mutable.Map[ByteArrayWrapper, Long] = mutable.Map()

  var firstUnusedConfirmedId = 0
  var firstUnusedUnconfirmedId = Int.MaxValue + 1
  lazy val registry = mutable.Map[ByteArrayWrapper, Long]() //keep statuses

  private def increaseBalances(uncertainBox: BoxUncertain): Unit = {
    val box = uncertainBox.box
    val tokenDelta = box.value
    val assetDeltas = box.additionalTokens

    //todo: reduce boilerplate below?
    if(uncertainBox.onChain){
      balance += tokenDelta
      assetDeltas.foreach{ case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val updBalance = assetBalances.getOrElse(wid, 0L) + amount
        assetBalances.put(wid, updBalance)
      }
    } else {
      unconfirmedBalance += tokenDelta
      assetDeltas.foreach{ case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val updBalance = unconfirmedAssetBalances.getOrElse(wid, 0L) + amount
        unconfirmedAssetBalances.put(wid, updBalance)
      }
    }
  }

  private def register(boxId: ByteArrayWrapper, certainBox: BoxCertain, onchain: Boolean) = {
    val (toFill, internalId) = if(onchain) {
      certainOnChain -> firstUnusedConfirmedId
    } else {
      certainOffChain -> firstUnusedUnconfirmedId
    }
    registry.put(boxId, internalId)
    toFill.put(internalId, certainBox)
    if(onchain) firstUnusedConfirmedId += 1 else firstUnusedUnconfirmedId += 1
  }

  private def resolveUncertainty(): Unit = {
    if (quickScan.nonEmpty) {
      val uncertainBox = quickScan.dequeue()
      val tx = uncertainBox.tx
      val outIndex = uncertainBox.outIndex
      val box = uncertainBox.box

      val lastUtxoDigest = AvlTreeData(lastBlockUtxoRootHash, 32) // todo: real impl

      val testingTx = UnsignedErgoLikeTransaction(
        IndexedSeq(new UnsignedInput(box.id)),
        IndexedSeq(new ErgoBoxCandidate(1L, Values.TrueLeaf))
      )

      val context =
        ErgoLikeContext(height + 1, lastUtxoDigest, IndexedSeq(box), testingTx, box, ContextExtension.empty)

      prover.prove(box.proposition, context, testingTx.messageToSign) match {
        case Success(_) =>
          val assets = box.additionalTokens.map(t => ByteArrayWrapper(t._1) -> t._2).toMap
          val boxId = ByteArrayWrapper(box.id)
          val certainBox = BoxCertain(tx, outIndex, box.value, assets)
          println("Received: " + certainBox)

          register(boxId, certainBox, uncertainBox.onChain)
          increaseBalances(uncertainBox)
        case Failure(_) => quickScan.enqueue(uncertainBox)
      }
    }
  }

  def scan(tx: ErgoTransaction, heightOpt: Option[Height]): Boolean = {
    //todo: check uncertain boxes as well?
    //todo: consider double-spend in an unconfirmed tx
    tx.inputs.foreach{inp =>
      val boxId = ByteArrayWrapper(inp.boxId)
      registry.remove(boxId) match {
        case Some(internalId) =>
          val onchain = if(internalId <= Int.MaxValue) true else false
          if(onchain) {
            certainOnChain.remove(internalId)
          } else {
            //todo: what to do with unconfirmed?
          }
        case None   =>
      }
    }

    tx.outputCandidates.zipWithIndex.exists { case (outCandidate, outIndex) =>
      toTrack.find(t => outCandidate.propositionBytes.containsSlice(t)) match {
        case Some(_) =>
          val bu = BoxUncertain(tx, outIndex.toShort, heightOpt)
          heightOpt match {
            case Some(h) =>
              val wid = ByteArrayWrapper(tx.id)
              quickScan.enqueue(bu)
              confirmedIndex.put(h, confirmedIndex.getOrElse(h, Seq()) :+ wid)
            case None =>
              quickScan.enqueue(bu)
          }
          true
        case None =>
          false
      }
    }
  }

  private def extractFromTransactions(txs: Seq[ErgoTransaction]): Boolean = {
    txs.exists(tx => scan(tx, Some(height)))
  }

  private def extractFromHeader(h: Header): Unit = {
    height = h.height
    lastBlockUtxoRootHash = h.stateRoot
  }

  //todo: avoid magic number, use non-default executor? check that resolve is not scheduled already
  private def resolveAgain = if(quickScan.nonEmpty){
    context.system.scheduler.scheduleOnce(10.seconds)(self ! Resolve)
  }

  override def receive: Receive = {
    case ScanOffchain(tx) =>
      if (scan(tx, None)){
        resolveUncertainty()
      }
      resolveAgain

    case Resolve =>
      resolveUncertainty()
      resolveAgain

    case ScanOnchain(fullBlock) =>
      extractFromHeader(fullBlock.header)
      val queueLengthBefore = quickScan.length
      if (extractFromTransactions(fullBlock.transactions)) {
        val queueLengthAfter = quickScan.length
        (1 to (queueLengthAfter - queueLengthBefore)).foreach(_ =>
          resolveUncertainty()
        )
      }
      resolveAgain
  }
}

object ErgoWalletActor {
  private[ErgoWalletActor] case object Resolve
  case class ScanOffchain(tx: ErgoTransaction)
  case class ScanOnchain(block: ErgoFullBlock)
}