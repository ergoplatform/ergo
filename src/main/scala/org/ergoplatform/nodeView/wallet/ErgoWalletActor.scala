package org.ergoplatform.nodeView.wallet

import akka.actor.Actor
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.state.ErgoStateContext
import scorex.core.utils.ScorexLogging
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

case class BoxUnspent(tx: ErgoTransaction, outIndex: Short, ergoValue: Long, assets: Map[ByteArrayWrapper, Long]){
  lazy val box: ErgoBox = tx.outputCandidates.apply(outIndex).toBox(tx.id, outIndex)
}


case class BalancesSnapshot(height: Height, balance: Long, assetBalances: Map[ByteArrayWrapper, Long])


class ErgoWalletActor(seed: String) extends Actor with ScorexLogging {
  import ErgoWalletActor._

  //todo: pass as parameter, add to config
  val coinSelector: CoinSelector = new DefaultCoinSelector

  private val prover = new ErgoProvingInterpreter(seed)

  private var height = 0
  private var lastBlockUtxoRootHash = ADDigest @@ Array.fill(32)(0: Byte)

  private val toTrack = prover.dlogSecrets.map(prover.bytesToTrack)

  private val quickScan = mutable.Queue[BoxUncertain]()

  //todo: make BoxSpent class
  private val spentOffchain = mutable.LongMap[BoxUnspent]()
  private val spentOnchain = mutable.LongMap[BoxUnspent]()

  private val unspentOffChain = mutable.LongMap[BoxUnspent]()
  private val unspentOnChain = mutable.LongMap[BoxUnspent]()
  private val confirmedIndex = mutable.TreeMap[Height, Seq[ByteArrayWrapper]]()

  private var balance: Long = 0
  private var assetBalances: mutable.Map[ByteArrayWrapper, Long] = mutable.Map()

  private var unconfirmedBalance: Long = 0
  private var unconfirmedAssetBalances: mutable.Map[ByteArrayWrapper, Long] = mutable.Map()

  private var firstUnusedConfirmedId = 0
  private var firstUnusedUnconfirmedId = Int.MaxValue + 1
  private lazy val registry = mutable.Map[ByteArrayWrapper, Long]() //keep statuses

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

  private def decreaseBalances(uncertainBox: BoxUnspent, onChain: Boolean): Unit = {
    val box = uncertainBox.box
    val tokenDelta = box.value
    val assetDeltas = box.additionalTokens

    //todo: reduce boilerplate below?
    if(onChain){
      balance -= tokenDelta
      assetDeltas.foreach{ case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val currentBalance = assetBalances.getOrElse(wid, 0L)
        if(currentBalance == amount){
          assetBalances.remove(wid)
        }else {
          val updBalance = currentBalance - amount
          assetBalances.put(wid, updBalance)
        }
      }
    } else {
      unconfirmedBalance -= tokenDelta
      assetDeltas.foreach{ case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val currentBalance = unconfirmedAssetBalances.getOrElse(wid, 0L)
        if(currentBalance == amount){
          unconfirmedAssetBalances.remove(wid)
        }else {
          val updBalance = currentBalance - amount
          unconfirmedAssetBalances.put(wid, updBalance)
        }
      }
    }
  }

  private def register(boxId: ByteArrayWrapper, certainBox: BoxUnspent, onchain: Boolean) = {
    val (toFill, internalId) = if(onchain) {
      unspentOnChain -> firstUnusedConfirmedId
    } else {
      unspentOffChain -> firstUnusedUnconfirmedId
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
          val unspentBox = BoxUnspent(tx, outIndex, box.value, assets)
          println("Received: " + unspentBox)

          register(boxId, unspentBox, uncertainBox.onChain)
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
          val boxOnchain = if(internalId <= Int.MaxValue) true else false
          if (boxOnchain) {
            val removed = unspentOnChain.remove(internalId).map { unspent =>
              val txOnchain = heightOpt.isDefined

              if(txOnchain) {
                spentOnchain.put(internalId, unspent)
              } else {
                spentOffchain.put(internalId, unspent)
              }
              decreaseBalances(unspent, txOnchain)
            }.isDefined
            if(!removed) log.warn(s"Registered unspent box id is not found in the unspents: $boxId")
            //todo: decrease balances
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

    case ReadBalances =>
      sender() ! BalancesSnapshot(height, balance, assetBalances.toMap)  //todo: avoid .toMap?

      //todo: check boxes being spent
    case GenerateTransaction(payTo) =>
      //todo: add assets
      val targetBalance = payTo.map(_.value).sum

      def filterFn(bu: BoxUnspent) =
        registry.get(ByteArrayWrapper(bu.box.id)).forall(iid => !spentOffchain.contains(iid))

      val txOpt = coinSelector.select(unspentOnChain.valuesIterator, filterFn, targetBalance, balance, Map(), Map()).flatMap { r =>
        val inputs = r.boxes.toIndexedSeq
        val changeAssets = r.changeAssets
        val changeBalance = r.changeBalance

        //todo: fix proposition, assets and register
        val changeAddress = prover.dlogSecrets.head.publicImage
        val changeBoxCandidate = new ErgoBoxCandidate(changeBalance, changeAddress, Seq(), Map())

        val unsignedTx = new UnsignedErgoTransaction(
          inputs.map(_.id).map(id => new UnsignedInput(id)),
          (payTo :+ changeBoxCandidate).toIndexedSeq)

        prover.sign(unsignedTx, inputs, ErgoStateContext(height, lastBlockUtxoRootHash))
      }

      sender() ! txOpt
  }
}

object ErgoWalletActor {
  private[ErgoWalletActor] case object Resolve
  case class ScanOffchain(tx: ErgoTransaction)
  case class ScanOnchain(block: ErgoFullBlock)

  case class GenerateTransaction(payTo: Seq[ErgoBoxCandidate])

  case object ReadBalances
}