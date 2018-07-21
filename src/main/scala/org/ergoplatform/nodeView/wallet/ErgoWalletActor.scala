package org.ergoplatform.nodeView.wallet

import akka.actor.Actor
import io.iohk.iodb.ByteArrayWrapper
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


sealed trait Transition

case class ProcessRollback(toHeight: Int) extends Transition

case class CreationConfirmation(creationHeight: Height) extends Transition

case class ProcessSpending(spendingTransaction: ErgoTransaction, spendingHeightOpt: Option[Height]) extends Transition


sealed trait TrackedBox extends ScorexLogging {
  val creationTx: ErgoTransaction
  val creationOutIndex: Short

  val onchain: Boolean

  val box: ErgoBox

  lazy val value = box.value
  lazy val assets = box.additionalTokens.map { case (id, amt) =>
    ByteArrayWrapper(id) -> amt
  }.toMap


  lazy val boxId = ByteArrayWrapper(box.id)

  def register(): Unit = Registry.putToRegistry(this)

  def deregister(): Unit = Registry.removeFromRegistry(boxId)

  def transition(spendingTransaction: ErgoTransaction, spendingHeightOpt: Option[Height]): Option[TrackedBox]

  def transition(creationHeight: Height): Option[TrackedBox]

  def transitionBack(toHeight: Int): Option[TrackedBox]
}

object Registry {

  private val registry = mutable.Map[ByteArrayWrapper, TrackedBox]()

  //todo: build indexes instead of iteration
  def unspentBoxes: Iterator[UnspentBox] = registry.valuesIterator.flatMap { tb =>
    tb match {
      case ub: UnspentBox => Some(ub)
      case _ => None
    }
  }

  def uncertainBoxes: Iterator[UncertainBox] = registry.valuesIterator.flatMap { tb =>
    (tb match {
      case ub: UncertainBox => Some(ub)
      case _ => None
    }): Option[UncertainBox]
  }

  //todo: extract a random element, not head
  def nextUncertain(): Option[UncertainBox] = uncertainBoxes.toSeq.headOption

  def registryContains(boxId: ByteArrayWrapper): Boolean = registry.contains(boxId)

  def putToRegistry(trackedBox: TrackedBox) = synchronized {
    registry.put(trackedBox.boxId, trackedBox)
  }

  def removeFromRegistry(boxId: ByteArrayWrapper) = registry(boxId)

  private val confirmedIndex = mutable.TreeMap[Height, Seq[ByteArrayWrapper]]()

  def putToConfirmedIndex(height: Height, boxId: ByteArrayWrapper) = synchronized {
    confirmedIndex.put(height, confirmedIndex.getOrElse(height, Seq()) :+ boxId)
  }

  private var _confirmedBalance: Long = 0
  private val _confirmedAssetBalances: mutable.Map[ByteArrayWrapper, Long] = mutable.Map()

  private var _unconfirmedBalance: Long = 0
  private val _unconfirmedAssetBalances: mutable.Map[ByteArrayWrapper, Long] = mutable.Map()

  def confirmedBalance = _confirmedBalance
  def confirmedAssetBalances = _confirmedAssetBalances

  def unconfirmedBalance = _unconfirmedBalance
  def unconfirmedAssetBalances = _unconfirmedAssetBalances


  def increaseBalances(unspentBox: UnspentBox): Unit = synchronized {
    val box = unspentBox.box
    val tokenDelta = box.value
    val assetDeltas = box.additionalTokens

    //todo: reduce boilerplate below?
    if (unspentBox.onchain) {
      _confirmedBalance += tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val updBalance = _confirmedAssetBalances.getOrElse(wid, 0L) + amount
        _confirmedAssetBalances.put(wid, updBalance)
      }
    } else {
      _unconfirmedBalance += tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val updBalance = _unconfirmedAssetBalances.getOrElse(wid, 0L) + amount
        _unconfirmedAssetBalances.put(wid, updBalance)
      }
    }
  }

  def decreaseBalances(unspentBox: UnspentBox): Unit = synchronized {
    val box = unspentBox.box
    val tokenDelta = box.value
    val assetDeltas = box.additionalTokens

    //todo: reduce boilerplate below?
    if (unspentBox.onchain) {
      _confirmedBalance -= tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val currentBalance = _confirmedAssetBalances.getOrElse(wid, 0L)
        if (currentBalance == amount) {
          _confirmedAssetBalances.remove(wid)
        } else {
          val updBalance = currentBalance - amount
          _confirmedAssetBalances.put(wid, updBalance)
        }
      }
    } else {
      _unconfirmedBalance -= tokenDelta
      assetDeltas.foreach { case (id, amount) =>
        val wid = ByteArrayWrapper(id)
        val currentBalance = _unconfirmedAssetBalances.getOrElse(wid, 0L)
        if (currentBalance == amount) {
          _unconfirmedAssetBalances.remove(wid)
        } else {
          val updBalance = currentBalance - amount
          _unconfirmedAssetBalances.put(wid, updBalance)
        }
      }
    }
  }

  def makeTransition(boxId: ByteArrayWrapper, transition: Transition): Unit = {
    makeTransition(registry(boxId), transition)
  }

  def makeTransition(trackedBox: TrackedBox, transition: Transition): Unit = {
    val transitionResult: Option[TrackedBox] = transition match {
      case ProcessRollback(toHeight) =>
        trackedBox.transitionBack(toHeight)
      case CreationConfirmation(creationHeight) =>
        trackedBox.transition(creationHeight)
      case ProcessSpending(spendingTransaction, spendingHeightOpt) =>
        trackedBox.transition(spendingTransaction, spendingHeightOpt)
    }
    transitionResult match {
      case Some(newTrackedBox) =>
        makeTransition(trackedBox, newTrackedBox)
      case None =>
    }
  }

  def makeTransition(oldTrackedBox: TrackedBox, newTrackedBox: TrackedBox): Unit = {
    newTrackedBox.register()
    oldTrackedBox.deregister()
  }
}

trait UncertainBox extends TrackedBox {
  def makeCertain(): TrackedBox
}

trait CertainBox extends TrackedBox

trait UnspentBox extends TrackedBox

trait SpentBox extends TrackedBox {
  val spendingTx: ErgoTransaction
}

trait OffchainBox extends TrackedBox {
  override val onchain = false
}

trait OnchainBox extends TrackedBox {
  override val onchain = true
}


trait UnspentOffchainTrackedBox extends UnspentBox with OffchainBox {
  override def register(): Unit = {
    super.register()
    log.info("New offchain box arrived: " + this)
    if (this.isInstanceOf[CertainBox]) Registry.increaseBalances(this)
  }

  override def deregister(): Unit = {
    super.deregister()
    if (this.isInstanceOf[CertainBox]) Registry.decreaseBalances(this)
  }
}

trait UnspentOnchainTrackedBox extends UnspentBox with OnchainBox {
  val creationHeight: Int

  override def register(): Unit = {
    super.register()
    log.info("New onchain box arrived: " + this)
    Registry.putToConfirmedIndex(creationHeight, boxId)
    if (this.isInstanceOf[CertainBox]) Registry.increaseBalances(this)
  }

  override def deregister(): Unit = {
    super.deregister()
    if (this.isInstanceOf[CertainBox]) Registry.decreaseBalances(this)
  }
}

trait SpentOffchainTrackedBox extends SpentBox with OffchainBox {
  val creationHeight: Option[Int]
}

trait SpentOnchainTrackedBox extends SpentBox with OnchainBox {
  val creationHeight: Int
  val spendingHeight: Int


  override def register(): Unit = {
    super.register()
    Registry.putToConfirmedIndex(spendingHeight, boxId)
  }
}


case class UncertainUnspentOffchainBox(override val creationTx: ErgoTransaction,
                                       override val creationOutIndex: Short,
                                       override val box: ErgoBox) extends UncertainBox with UnspentOffchainTrackedBox {
  override def transition(creationHeight: Height): Option[TrackedBox] =
    Some(UncertainUnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box))

  override def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    heightOpt match {
      case Some(h) => log.warn(s"Onchain transaction ${spendingTransaction.id} is spending offchain box $box"); None
      case None => Some(UncertainSpentOffchainBox(creationTx, creationOutIndex, None, spendingTransaction, box))
    }
  }

  override def transitionBack(toHeight: Int): Option[TrackedBox] = None

  override def makeCertain(): TrackedBox = UnspentOffchainBox(creationTx, creationOutIndex, box)
}

case class UncertainUnspentOnchainBox(override val creationTx: ErgoTransaction,
                                      override val creationOutIndex: Short,
                                      override val creationHeight: Int,
                                      override val box: ErgoBox) extends UncertainBox with UnspentOnchainTrackedBox {

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    Some(heightOpt match {
      case Some(h) =>
        UncertainSpentOnchainBox(creationTx, creationOutIndex, creationHeight, spendingTransaction, h, box)
      case None =>
        UncertainSpentOffchainBox(creationTx, creationOutIndex, Some(creationHeight), spendingTransaction, box)
    })
  }

  def transition(creationHeight: Height): Option[TrackedBox] = {
    log.warn(s"Double creation of UncertainUnspentOnchainBox for $boxId")
    None
  }

  def transitionBack(toHeight: Int): Option[TrackedBox] = {
    if (creationHeight > toHeight) {
      Some(UncertainUnspentOffchainBox(creationTx, creationOutIndex, box))
    } else None
  }

  override def makeCertain(): TrackedBox = {
    UnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box)
  }
}

case class UncertainSpentOffchainBox(override val creationTx: ErgoTransaction,
                                     override val creationOutIndex: Short,
                                     override val creationHeight: Option[Int],
                                     override val spendingTx: ErgoTransaction,
                                     override val box: ErgoBox) extends UncertainBox with SpentOffchainTrackedBox {

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    heightOpt match {
      case Some(h) =>
        require(creationHeight.isDefined)
        Some(UncertainSpentOnchainBox(creationTx, creationOutIndex, creationHeight.get, spendingTransaction, h, box))
      case None =>
        log.warn(s"Double spending of an unconfirmed box $boxId")
        //todo: handle double-spending strategy for an unconfirmed tx
        None
    }
  }

  def transition(creationHeight: Height): Option[TrackedBox] = this.creationHeight match {
    case Some(_) => log.warn(s"Double creation of $boxId"); None
    case None => Some(this.copy(creationHeight = Some(creationHeight)))
  }

  def transitionBack(toHeight: Int): Option[TrackedBox] = creationHeight match {
    case Some(h) if h < toHeight => Some(UncertainSpentOffchainBox(creationTx, creationOutIndex, None, spendingTx, box))
    case _ => None
  }

  override def makeCertain(): TrackedBox = {
    SpentOffchainBox(creationTx, creationOutIndex, creationHeight, spendingTx, box)
  }
}

case class UncertainSpentOnchainBox(override val creationTx: ErgoTransaction,
                                    override val creationOutIndex: Short,
                                    override val creationHeight: Int,
                                    override val spendingTx: ErgoTransaction,
                                    override val spendingHeight: Int,
                                    override val box: ErgoBox) extends UncertainBox with SpentOnchainTrackedBox {
  def transition(spendingTransaction: ErgoTransaction, spendingHeightOpt: Option[Height]): Option[TrackedBox] = None

  def transition(creationHeight: Height): Option[TrackedBox] = None

  def transitionBack(toHeight: Int): Option[TrackedBox] = (toHeight < spendingHeight, toHeight < creationHeight) match {
    case (false, false) => None
    case (true, false) => Some(UncertainUnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box))
    case (true, true) => Some(UncertainUnspentOffchainBox(creationTx, creationOutIndex, box))
    case (false, true) => log.warn("Wrong state"); None
  }

  override def makeCertain(): TrackedBox =
    SpentOnchainBox(creationTx, creationOutIndex, creationHeight, spendingTx, spendingHeight, box)
}


case class UnspentOffchainBox(override val creationTx: ErgoTransaction,
                              override val creationOutIndex: Short,
                              override val box: ErgoBox) extends UnspentOffchainTrackedBox with CertainBox {

  override def transition(creationHeight: Height): Option[TrackedBox] =
    Some(UnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box))

  override def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    heightOpt match {
      case Some(h) => log.warn(s"Onchain transaction ${spendingTransaction.id} is spending offchain box $box"); None
      case None => Some(SpentOffchainBox(creationTx, creationOutIndex, None, spendingTransaction, box))
    }
  }

  override def transitionBack(toHeight: Int): Option[TrackedBox] = None
}

case class UnspentOnchainBox(override val creationTx: ErgoTransaction,
                             override val creationOutIndex: Short,
                             override val creationHeight: Int,
                             override val box: ErgoBox) extends UnspentOnchainTrackedBox with CertainBox {

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    Some(heightOpt match {
      case Some(h) =>
        SpentOnchainBox(creationTx, creationOutIndex, creationHeight, spendingTransaction, h, box)
      case None =>
        SpentOffchainBox(creationTx, creationOutIndex, Some(creationHeight), spendingTransaction, box)
    })
  }

  def transition(creationHeight: Height): Option[TrackedBox] = {
    log.warn(s"Double creation of UncertainUnspentOnchainBox for $boxId")
    None
  }

  def transitionBack(toHeight: Int): Option[TrackedBox] = {
    if (creationHeight > toHeight) {
      Some(UnspentOffchainBox(creationTx, creationOutIndex, box))
    } else None
  }
}

case class SpentOffchainBox(override val creationTx: ErgoTransaction,
                            override val creationOutIndex: Short,
                            override val creationHeight: Option[Int],
                            override val spendingTx: ErgoTransaction,
                            override val box: ErgoBox) extends SpentOffchainTrackedBox with CertainBox {

  def transition(spendingTransaction: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    heightOpt match {
      case Some(h) =>
        require(creationHeight.isDefined)
        Some(SpentOnchainBox(creationTx, creationOutIndex, creationHeight.get, spendingTransaction, h, box))
      case None =>
        log.warn(s"Double spending of an unconfirmed box $boxId")
        //todo: handle double-spending strategy for an unconfirmed tx
        None
    }
  }

  def transition(creationHeight: Height): Option[TrackedBox] = this.creationHeight match {
    case Some(_) => log.warn(s"Double creation of $boxId"); None
    case None => Some(this.copy(creationHeight = Some(creationHeight)))
  }

  def transitionBack(toHeight: Int): Option[TrackedBox] = creationHeight match {
    case Some(h) if h < toHeight => Some(SpentOffchainBox(creationTx, creationOutIndex, None, spendingTx, box))
    case _ => None
  }
}

case class SpentOnchainBox(override val creationTx: ErgoTransaction,
                           override val creationOutIndex: Short,
                           override val creationHeight: Int,
                           override val spendingTx: ErgoTransaction,
                           override val spendingHeight: Int,
                           override val box: ErgoBox) extends SpentOnchainTrackedBox with CertainBox {

  def transition(spendingTransaction: ErgoTransaction, spendingHeightOpt: Option[Height]): Option[TrackedBox] = None

  def transition(creationHeight: Height): Option[TrackedBox] = None

  def transitionBack(toHeight: Int): Option[TrackedBox] = (toHeight < spendingHeight, toHeight < creationHeight) match {
    case (false, false) => None
    case (true, false) => Some(UncertainUnspentOnchainBox(creationTx, creationOutIndex, creationHeight, box))
    case (true, true) => Some(UncertainUnspentOffchainBox(creationTx, creationOutIndex, box))
    case (false, true) => log.warn("Wrong state"); None
  }
}


case class BalancesSnapshot(height: Height, balance: Long, assetBalances: Map[ByteArrayWrapper, Long])


class ErgoWalletActor(seed: String) extends Actor with ScorexLogging {

  import ErgoWalletActor._

  import Registry._

  //todo: pass as parameter, add to config
  val coinSelector: CoinSelector = new DefaultCoinSelector

  private val prover = new ErgoProvingInterpreter(seed)

  private var height = 0
  private var lastBlockUtxoRootHash = ADDigest @@ Array.fill(32)(0: Byte)

  private val toTrack = prover.dlogPubkeys.map(prover.bytesToTrack)


  //todo: make resolveUncertainty(boxId, witness)
  private def resolveUncertainty(): Unit = {
    Registry.nextUncertain().map { uncertainBox =>
      val box = uncertainBox.box

      val lastUtxoDigest = AvlTreeData(lastBlockUtxoRootHash, 32)

      val testingTx = UnsignedErgoLikeTransaction(
        IndexedSeq(new UnsignedInput(box.id)),
        IndexedSeq(new ErgoBoxCandidate(1L, Values.TrueLeaf))
      )

      val context =
        ErgoLikeContext(height + 1, lastUtxoDigest, IndexedSeq(box), testingTx, box, ContextExtension.empty)

      prover.prove(box.proposition, context, testingTx.messageToSign) match {
        case Success(_) =>
          Registry.makeTransition(uncertainBox, uncertainBox.makeCertain())
        case Failure(_) =>
        //todo: remove after some time? remove spent after some time?
      }
    }
  }

  def scan(tx: ErgoTransaction, heightOpt: Option[Height]): Boolean = {
    tx.inputs.foreach { inp =>
      val boxId = ByteArrayWrapper(inp.boxId)
      if (Registry.registryContains(boxId)) {
        Registry.makeTransition(boxId, ProcessSpending(tx, heightOpt))
      }
    }

    tx.outputCandidates.zipWithIndex.exists { case (outCandidate, outIndex) =>
      toTrack.find(t => outCandidate.propositionBytes.containsSlice(t)) match {
        case Some(_) =>
          val idxShort = outIndex.toShort
          val box = outCandidate.toBox(tx.id, idxShort)
          val bu = heightOpt match {
            case Some(h) => UncertainUnspentOnchainBox(tx, idxShort, h, box)
            case None => UncertainUnspentOffchainBox(tx, idxShort, box)
          }
          bu.register()
          true
        case None =>
          false
      }
    }
  }

  private def extractFromBlock(fb: ErgoFullBlock): Int = {
    height = fb.header.height
    lastBlockUtxoRootHash = fb.header.stateRoot
    fb.transactions.count(tx => scan(tx, Some(height)))
  }

  //todo: avoid magic number, use non-default executor? check that resolve is not scheduled already
  private def resolveAgain = if (Registry.uncertainBoxes.nonEmpty) {
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
      val txsFound = extractFromBlock(fullBlock)
      (1 to txsFound).foreach(_ => resolveUncertainty())
      resolveAgain

    //todo: update utxo root hash
    case Rollback(heightTo) =>
      //height = heightTo
      log.warn("Rollback in the wallet is not implemented")

    case ReadBalances(confirmed) =>
      if(confirmed) {
        sender() ! BalancesSnapshot(height, confirmedBalance, confirmedAssetBalances.toMap) //todo: avoid .toMap?
      } else {
        sender() ! BalancesSnapshot(height, unconfirmedBalance, unconfirmedAssetBalances.toMap) //todo: avoid .toMap?
      }

    case GenerateTransaction(payTo) =>
      //todo: add assets
      val targetBalance = payTo.map(_.value).sum

      //we do not use offchain boxes to create a transaction
      def filterFn(bu: UnspentBox) = bu.onchain

      val txOpt = coinSelector.select(unspentBoxes, filterFn, targetBalance, confirmedBalance, Map(), Map()).flatMap { r =>
        val inputs = r.boxes.toIndexedSeq
        val changeAssets = r.changeAssets
        val changeBalance = r.changeBalance

        //todo: fix proposition, assets and register
        val changeAddress = prover.dlogPubkeys.head
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

  case class Rollback(height: Int)

  case class GenerateTransaction(payTo: Seq[ErgoBoxCandidate])

  case class ReadBalances(confirmed: Boolean)
}