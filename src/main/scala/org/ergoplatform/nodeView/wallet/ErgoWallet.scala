package org.ergoplatform.nodeView.wallet

import akka.actor.{Actor, ActorSystem, Props}
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.{BestHeader, ScanOffchain, ScanOnchain}
import org.ergoplatform.settings.ErgoSettings
import scapi.sigma.DLogProtocol.{DLogProverInput, ProveDlog}
import scapi.sigma.{DiffieHellmanTupleProverInput, SigmaProtocolPrivateInput}
import scorex.core.VersionTag
import scorex.core.transaction.wallet.Vault
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.{ADDigest, ADKey}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import sigmastate.{AvlTreeData, Values}
import sigmastate.interpreter.{ContextExtension, ProverInterpreter, ProverResult}
import sigmastate.serialization.ValueSerializer
import sigmastate.utxo.CostTable

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class ErgoProvingInterpreter(override val maxCost: Long = CostTable.ScriptLimit)
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

  lazy val dlogSecrets: Seq[DLogProverInput] =
    (1 to 4).map(_ => DLogProverInput.random())

  lazy val dhSecrets: Seq[DiffieHellmanTupleProverInput] =
    (1 to 4).map(_ => DiffieHellmanTupleProverInput.random())
}


case class BoxUncertain(tx: ErgoTransaction, outIndex: Short)
case class BoxCertain(tx: ErgoTransaction, outIndex: Short, ergoValue: Long, assets: Map[ByteArrayWrapper, Long])


class ErgoWalletActor extends Actor {

  private val prover = new ErgoProvingInterpreter()

  var height = 0
  var lastBlockUtxoRootHash = ADDigest @@ Array.fill(32)(0: Byte)

  val secret = prover.dlogSecrets.head
  val toTrack = prover.dlogSecrets.map(prover.bytesToTrack)

  val quickScanOffchain = mutable.Map[ByteArrayWrapper, BoxUncertain]()
  val quickScanOnchain = mutable.Map[ByteArrayWrapper, BoxUncertain]()

  val certainOffChain = mutable.Map[ByteArrayWrapper, BoxCertain]()
  val certainOnChain = mutable.Map[ByteArrayWrapper, BoxCertain]()
  val confirmedIndex = mutable.TreeMap[Height, Seq[ByteArrayWrapper]]()


  private def resolveUncertainty(toScan: mutable.Map[ByteArrayWrapper, BoxUncertain],
                                 toFill: mutable.Map[ByteArrayWrapper, BoxCertain]): Unit = {
    toScan.foreach { case (txId, uncertainBoxData) =>
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
          toFill.put(txId, BoxCertain(tx, outIndex, box.value, assets))
          toScan.remove(txId)
        case Failure(_) =>
      }
    }
  }

  def scan(tx: ErgoTransaction, heightOpt: Option[Height]): Unit = {
    tx.outputCandidates.zipWithIndex.foreach { case (outCandidate, outIndex) =>
      toTrack.find(t => outCandidate.propositionBytes.containsSlice(t)) match {
        case Some(_) =>
          val bu = BoxUncertain(tx, outIndex.toShort)
          heightOpt match {
            case Some(h) =>
              val wid = ByteArrayWrapper(tx.id)
              quickScanOnchain.put(wid, bu)
              confirmedIndex.put(h, confirmedIndex.getOrElse(h, Seq()) :+ wid)
            case None =>
              quickScanOffchain.put(ByteArrayWrapper(tx.id), bu)
          }
        case None =>
      }
    }
  }

  private def extractFromTransactions(txs: Seq[ErgoTransaction]): Unit = {
    txs.foreach(tx => scan(tx, Some(height)))
  }

  private def extractFromHeader(h: Header): Unit = {
    height = h.height
    lastBlockUtxoRootHash = h.stateRoot
  }

  override def receive: Receive = {
    case ScanOffchain(tx) => scan(tx, None)
    case BestHeader(h) => extractFromHeader(h)
    case ScanOnchain(bt) => extractFromTransactions(bt.transactions)
  }
}

object ErgoWalletActor {
  case class ScanOffchain(tx: ErgoTransaction)
  case class ScanOnchain(bt: BlockTransactions)
  case class BestHeader(h: Header)
}


class ErgoWallet extends Vault[ErgoTransaction, ErgoPersistentModifier, ErgoWallet]
  with ScorexLogging {

  //todo: pass system from outside
  val actor = ActorSystem("ff").actorOf(Props(classOf[ErgoWalletActor]))


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
      case h: Header =>
        actor ! BestHeader(h)
      case bt: BlockTransactions =>
        //todo: check that this is correct bt
        actor ! ScanOnchain(bt)
      case fb: ErgoFullBlock =>
        actor ! BestHeader(fb.header)
        actor ! ScanOnchain(fb.blockTransactions)
    }
    this
  }

  //todo: implement
  override def rollback(to: VersionTag): Try[ErgoWallet] = Success(this)

  override type NVCT = this.type
}


object ErgoWallet extends App {
  @SuppressWarnings(Array("UnusedMethodParameter"))
  def readOrGenerate(settings: ErgoSettings): ErgoWallet = new ErgoWallet

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

  benchmark()
}