package org.ergoplatform.nodeView.wallet

import java.math.BigInteger

import akka.actor.{Actor, ActorSystem, Props}
import io.iohk.iodb.ByteArrayWrapper
import org.bouncycastle.util.BigIntegers
import org.ergoplatform._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.{ScanOffchain, ScanOnchain}
import org.ergoplatform.settings.ErgoSettings
import scapi.sigma.DLogProtocol.{DLogProverInput, ProveDlog}
import scapi.sigma.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}
import scorex.core.VersionTag
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.encode.{Base16, Base58}
import scorex.crypto.hash.Blake2b256
import sigmastate.{AvlTreeData, Values}
import sigmastate.interpreter.{ContextExtension, ProverInterpreter}
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.CostTable

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class ErgoProvingInterpreter(seed: String, override val maxCost: Long = CostTable.ScriptLimit)
  extends ErgoLikeInterpreter(maxCost) with ProverInterpreter {

  def hash256(input: Array[Byte]) = Blake2b256(input)

  //todo: take Blake2b160 ?
  def hash160(input: Array[Byte]) = hash256(input).take(20)

  def bytesToTrack(secret: DLogProverInput): Array[Byte] = bytesToTrack(secret.publicImage)

  def bytesToTrack(pubkey: ProveDlog): Array[Byte] = {
    ValueSerializer.serialize(pubkey)
  }

  def address(pubkey: ProveDlog): String = {
    val bt = bytesToTrack(pubkey)
    val bth160 = hash160(bt)

    //add network identifier
    val withNetworkByte = (0: Byte) +: bth160

    val checksum = hash256(withNetworkByte).take(4)
    Base58.encode(withNetworkByte ++ checksum)
  }

  override lazy val secrets: Seq[SigmaProtocolPrivateInput[_, _]] =
    dlogSecrets ++ dhSecrets

  lazy val dlogSecrets: Seq[DLogProverInput] = ErgoWallet.secretsFromSeed(seed).map(DLogProverInput.apply)

  lazy val dhSecrets: Seq[DiffieHellmanTupleProverInput] =
    (1 to 4).map(_ => DiffieHellmanTupleProverInput.random())
}


case class BoxUncertain(tx: ErgoTransaction, outIndex: Short, heightOpt: Option[Height]) {
  lazy val onChain: Boolean = heightOpt.isDefined
}

case class BoxCertain(tx: ErgoTransaction, outIndex: Short, ergoValue: Long, assets: Map[ByteArrayWrapper, Long])


class ErgoWalletActor(seed: String) extends Actor {

  private val prover = new ErgoProvingInterpreter(seed)

  private var height = 0
  private var lastBlockUtxoRootHash = ADDigest @@ Array.fill(32)(0: Byte)

  private val toTrack = prover.dlogSecrets.map(prover.bytesToTrack)

  private val quickScan = mutable.Queue[BoxUncertain]()

  private val certainOffChain = mutable.Map[ByteArrayWrapper, BoxCertain]()
  private val certainOnChain = mutable.Map[ByteArrayWrapper, BoxCertain]()
  private val confirmedIndex = mutable.TreeMap[Height, Seq[ByteArrayWrapper]]()


  private def resolveUncertainty(toFill: mutable.Map[ByteArrayWrapper, BoxCertain]): Unit = {
    if (quickScan.nonEmpty) {
      val uncertainBoxData = quickScan.dequeue()
      val tx = uncertainBoxData.tx
      val outIndex = uncertainBoxData.outIndex
      val box = tx.outputCandidates.apply(outIndex).toBox(tx.id, outIndex)
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
          val txId = ByteArrayWrapper(tx.id)
          val certainBox = BoxCertain(tx, outIndex, box.value, assets)
          println("Received: " + certainBox)
          toFill.put(txId, certainBox)
        case Failure(_) => quickScan.enqueue(uncertainBoxData)
      }
    }
  }

  def scan(tx: ErgoTransaction, heightOpt: Option[Height]): Boolean = {
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

  override def receive: Receive = {
    case ScanOffchain(tx) =>
      if (scan(tx, None)) resolveUncertainty(certainOffChain)
    case ScanOnchain(fullBlock) =>
      extractFromHeader(fullBlock.header)
      if (extractFromTransactions(fullBlock.transactions)) {
        resolveUncertainty(certainOnChain)
      }
  }
}

object ErgoWalletActor {
  case class ScanOffchain(tx: ErgoTransaction)
  case class ScanOnchain(block: ErgoFullBlock)
}


class ErgoWallet(actorSystem: ActorSystem, seed: String) extends Vault[ErgoTransaction, ErgoPersistentModifier, ErgoWallet]
  with ScorexLogging {

  private lazy val actor = actorSystem.actorOf(Props(classOf[ErgoWalletActor], seed))

  lazy val registry = ??? //keep statuses


  override def scanOffchain(tx: ErgoTransaction): ErgoWallet = {
    actor ! ScanOffchain(tx)
    this
  }

  override def scanOffchain(txs: Seq[ErgoTransaction]): ErgoWallet = {
    txs.foreach(tx => scanOffchain(tx))
    this
  }

  override def scanPersistent(modifier: ErgoPersistentModifier): ErgoWallet = {
    modifier match {
      case fb: ErgoFullBlock =>
        actor ! ScanOnchain(fb)
      case _ =>
        log.warn("Only full block is expected in ErgoWallet.scanPersistent")
    }
    this
  }

  //todo: implement
  override def rollback(to: VersionTag): Try[ErgoWallet] = Success(this)

  override type NVCT = this.type
}


object ErgoWallet {

  def readOrGenerate(actorSystem: ActorSystem, settings: ErgoSettings): ErgoWallet =
    new ErgoWallet(actorSystem, settings.walletSettings.seed)

  /*
  def benchmark() = {
    val w = new ErgoWallet

    val inputs = (1 to 2).map(_ => Input(
      ADKey @@ Array.fill(32)(0: Byte),
      ProverResult(Array.emptyByteArray, ContextExtension.empty)))

    val box = ErgoBox(1L, Values.TrueLeaf) //w.secret.publicImage)
    val tx = ErgoTransaction(inputs, IndexedSeq(box))

    var t0 = System.currentTimeMillis()
    (1 to 3000).foreach { _ =>
      w.scanOffchain(tx)
    }
    var t = System.currentTimeMillis()
    println("time to scan: " + (t - t0))
  }
  benchmark()*/

  def secretsFromSeed(seedStr: String): IndexedSeq[BigInteger] = {
    val seed = Base16.decode(seedStr).get
    (1 to 4).map { i =>
      BigIntegers.fromUnsignedByteArray(Blake2b256.hash(i.toByte +: seed))
    }
  }
}