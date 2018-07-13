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


trait TrackedBox {
  val box: ErgoBox
  val heightOpt: Option[Height]

  lazy val onchain: Boolean = heightOpt.isDefined

  lazy val boxId = ByteArrayWrapper(box.id)
}

case class UncertainBox(tx: ErgoTransaction,
                        outIndex: Short,
                        heightOpt: Option[Height],
                        spendingTxOpt: Option[ErgoTransaction],
                        heightSpentOpt: Option[Height]) extends TrackedBox {

  lazy val box: ErgoBox = tx.outputCandidates.apply(outIndex).toBox(tx.id, outIndex)
}


trait CertainBox extends TrackedBox

case class BoxUnspent(tx: ErgoTransaction,
                      outIndex: Short,
                      heightOpt: Option[Height],
                      ergoValue: Long,
                      assets: Map[ByteArrayWrapper, Long]) extends CertainBox {
  override lazy val box: ErgoBox = tx.outputCandidates.apply(outIndex).toBox(tx.id, outIndex)

  def toSpent(spendingTx: ErgoTransaction, heightSpentOpt: Option[Height]): BoxSpent =
    BoxSpent(box, tx, spendingTx, heightOpt, heightSpentOpt, ergoValue, assets)
}

case class BoxSpent(override val box: ErgoBox,
                    parentTx: ErgoTransaction,
                    spendingTx: ErgoTransaction,
                    heightOpt: Option[Height],
                    heightSpentOpt: Option[Height],
                    ergoValue: Long,
                    assets: Map[ByteArrayWrapper, Long]) extends CertainBox {
  override lazy val onchain = heightSpentOpt.isDefined
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


  private var lastScannedId = Long.MinValue
  private val quickScan = mutable.TreeMap[Long, UncertainBox]()

  private val spentOffchain = mutable.LongMap[BoxSpent]()
  private val spentOnchain = mutable.LongMap[BoxSpent]()

  private val unspentOffChain = mutable.LongMap[BoxUnspent]()
  private val unspentOnChain = mutable.LongMap[BoxUnspent]()
  private val confirmedIndex = mutable.TreeMap[Height, Seq[ByteArrayWrapper]]()

  private var balance: Long = 0
  private val assetBalances: mutable.Map[ByteArrayWrapper, Long] = mutable.Map()

  private var unconfirmedBalance: Long = 0
  private val unconfirmedAssetBalances: mutable.Map[ByteArrayWrapper, Long] = mutable.Map()

  private var firstUncertaindId = Long.MinValue
  private var firstUnusedConfirmedId = 0
  private var firstUnusedUnconfirmedId = Int.MaxValue + 1

  /**
    * We store all the spent and unspent boxes here
    */
  private lazy val registry = mutable.Map[ByteArrayWrapper, Long]() //keep statuses


  private def increaseBalances(unspentBox: BoxUnspent): Unit = {
    val box = unspentBox.box
    val tokenDelta = box.value
    val assetDeltas = box.additionalTokens

    //todo: reduce boilerplate below?
    if (unspentBox.onchain) {
      balance += tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val updBalance = assetBalances.getOrElse(wid, 0L) + amount
        assetBalances.put(wid, updBalance)
      }
    } else {
      unconfirmedBalance += tokenDelta
      assetDeltas.foreach { case (id, amount) =>
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
    if (onChain) {
      balance -= tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val currentBalance = assetBalances.getOrElse(wid, 0L)
        if (currentBalance == amount) {
          assetBalances.remove(wid)
        } else {
          val updBalance = currentBalance - amount
          assetBalances.put(wid, updBalance)
        }
      }
    } else {
      unconfirmedBalance -= tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val currentBalance = unconfirmedAssetBalances.getOrElse(wid, 0L)
        if (currentBalance == amount) {
          unconfirmedAssetBalances.remove(wid)
        } else {
          val updBalance = currentBalance - amount
          unconfirmedAssetBalances.put(wid, updBalance)
        }
      }
    }
  }

  private def register(box: TrackedBox, heightOpt: Option[Height]) = {
    box match {
      case uncertainBox: UncertainBox =>
        quickScan.put(firstUncertaindId, uncertainBox)
        registry.put(box.boxId, firstUncertaindId)
        firstUncertaindId += 1
      case unspentBox: BoxUnspent if unspentBox.onchain =>
        unspentOnChain.put(firstUnusedConfirmedId, unspentBox)
        registry.put(box.boxId, firstUnusedConfirmedId)
        firstUnusedConfirmedId += 1
      case unspentBox: BoxUnspent if !unspentBox.onchain =>
        unspentOffChain.put(firstUnusedUnconfirmedId, unspentBox)
        registry.put(box.boxId, firstUnusedUnconfirmedId)
        firstUnusedUnconfirmedId += 1

      case _ => log.warn("wrong input")
    }
    heightOpt.foreach { h =>
      confirmedIndex.put(h, confirmedIndex.getOrElse(h, Seq()) :+ box.boxId)
    }
  }

  private def uncertainToCertain(uncertainBox: UncertainBox): CertainBox = {
    val box = uncertainBox.box
    val tx = uncertainBox.tx
    val outIndex = uncertainBox.outIndex
    val heightOpt = uncertainBox.heightOpt
    val assets = box.additionalTokens.map(t => ByteArrayWrapper(t._1) -> t._2).toMap
    val certainBox = uncertainBox.spendingTxOpt match {
      case None =>
        BoxUnspent(tx, outIndex, heightOpt, box.value, assets)
      case Some(spendingTx) =>
        BoxSpent(box, tx, spendingTx, heightOpt, uncertainBox.heightSpentOpt, box.value, assets)
    }
    println("Received: " + certainBox)

    val removalResult = registry.remove(uncertainBox.boxId).flatMap { internalId =>
      quickScan.remove(internalId)
    }

    if (removalResult.isEmpty) log.warn(s"Problem with removing ${uncertainBox.boxId}")

    register(certainBox, heightOpt)
    certainBox match {
      case unspent: BoxUnspent => increaseBalances(unspent)
      case _ =>
    }
    certainBox
  }
  
  private def nextInTheQueue(): UncertainBox = {
    val iter = quickScan.iteratorFrom(lastScannedId)
    if (iter.hasNext) {
      val (newScannedId, res) = iter.next()
      lastScannedId = newScannedId
      res
    } else {
      lastScannedId = Long.MinValue
      nextInTheQueue()
    }
  }

  private def resolveUncertainty(): Unit = {
    if (quickScan.nonEmpty) {
      val uncertainBox = nextInTheQueue()
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
          uncertainToCertain(uncertainBox)
        case Failure(_) =>
        //todo: remove after some time?
      }
    }
  }

  def scan(tx: ErgoTransaction, heightOpt: Option[Height]): Boolean = {
    //todo: consider double-spend in an unconfirmed tx
    tx.inputs.foreach { inp =>
      val boxId = ByteArrayWrapper(inp.boxId)
      registry.remove(boxId) match {
        case Some(internalId) =>
          internalId match {
            case i: Long if i < 0 =>
              val uncertainBox = quickScan(i)
              val updBox = uncertainBox.copy(spendingTxOpt = Some(tx), heightSpentOpt = heightOpt)
              quickScan.put(i, updBox)
            case i: Long if i <= Int.MaxValue =>
              val removed = unspentOnChain.remove(internalId).map { unspent =>
                val txOnchain = heightOpt.isDefined
                val spent = unspent.toSpent(tx, heightOpt)

                if (txOnchain) {
                  spentOnchain.put(internalId, spent)
                } else {
                  spentOffchain.put(internalId, spent)
                }
                decreaseBalances(unspent, txOnchain)
              }.isDefined
              if (!removed) log.warn(s"Registered unspent box id is not found in the unspents: $boxId")
            //todo: decrease balances
            case i: Long =>
            //todo: handle unconfirmed double-spend case
          }
        case None => // we do not track this box, nothing to do here
      }
    }

    tx.outputCandidates.zipWithIndex.exists { case (outCandidate, outIndex) =>
      toTrack.find(t => outCandidate.propositionBytes.containsSlice(t)) match {
        case Some(_) =>
          val bu = UncertainBox(tx, outIndex.toShort, heightOpt, None, None)
          register(bu, heightOpt)
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
  private def resolveAgain = if (quickScan.nonEmpty) {
    context.system.scheduler.scheduleOnce(10.seconds)(self ! Resolve)
  }

  override def receive: Receive = {
    case ScanOffchain(tx) =>
      if (scan(tx, None)) {
        resolveUncertainty()
      }
      resolveAgain

    case Resolve =>
      resolveUncertainty()
      resolveAgain

    case ScanOnchain(fullBlock) =>
      extractFromHeader(fullBlock.header)
      val queueLengthBefore = quickScan.size
      if (extractFromTransactions(fullBlock.transactions)) {
        val queueLengthAfter = quickScan.size
        (1 to (queueLengthAfter - queueLengthBefore)).foreach(_ =>
          resolveUncertainty()
        )
      }
      resolveAgain

    case ReadBalances =>
      sender() ! BalancesSnapshot(height, balance, assetBalances.toMap) //todo: avoid .toMap?

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