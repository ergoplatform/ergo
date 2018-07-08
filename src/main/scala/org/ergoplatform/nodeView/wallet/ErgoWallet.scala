package org.ergoplatform.nodeView.wallet

import org.ergoplatform._
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
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
    val withNetworkByte = (0:Byte) +: bth160

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

class ErgoWallet extends Vault[ErgoTransaction, ErgoPersistentModifier, ErgoWallet]
  with ScorexLogging {

  var height = 0L
  var lastBlockUtxoRootHash = ADDigest @@ Array.fill(32)(0: Byte)

  val prover = new ErgoProvingInterpreter()

  val secret = prover.dlogSecrets.head

  val toTrack = prover.dlogSecrets.map(prover.bytesToTrack)

  case class OffchainBoxUncertain(tx: ErgoTransaction, outIndex: Short)

  val quickScanOffchain = mutable.Buffer[OffchainBoxUncertain]()
  val quickScanOnchain = mutable.Seq[ErgoBox]()

  lazy val registry = ??? //keep statuses

  //todo: implement
  override def scanOffchain(tx: ErgoTransaction): ErgoWallet = {
    tx.outputCandidates.zipWithIndex.foreach{case (outCandidate, outIndex) =>
      toTrack.find(t => outCandidate.propositionBytes.containsSlice(t)) match {
        case Some(_) =>
          quickScanOffchain += OffchainBoxUncertain(tx, outIndex.toShort)
        case None =>
      }
    }
    this
  }

  def resolveUncertainty(): Unit = {
    quickScanOffchain.foreach {uncertainBoxData =>
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
        case Failure(_) =>
      }
    }
  }

  //todo: implement
  override def scanOffchain(txs: Seq[ErgoTransaction]): ErgoWallet = {
    txs.foreach(tx => scanOffchain(tx))
    this
  }

  private def extractFromHeader(h: Header) = {
    height = h.height
    lastBlockUtxoRootHash = h.stateRoot
  }

  //todo: implement
  override def scanPersistent(modifier: ErgoPersistentModifier): ErgoWallet = {
    modifier match {
      case h: Header => extractFromHeader(h)
      case bt: BlockTransactions =>
      case fb: ErgoFullBlock => extractFromHeader(fb.header)
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

    val box = ErgoBox(1L, w.secret.publicImage)
    val tx = ErgoTransaction(inputs, IndexedSeq(box))

    var t0 = System.currentTimeMillis()
    (1 to 3000).foreach { _ =>
      w.scanOffchain(tx)
    }
    var t = System.currentTimeMillis()
    println("time to scan: " + (t - t0))

    t0 = System.currentTimeMillis()
    w.resolveUncertainty()
    t = System.currentTimeMillis()
    println("time to resolve: " + (t - t0))
  }

  benchmark()
}
