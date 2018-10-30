package org.ergoplatform.utils.generators

import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.mining.{AutoleakusPowScheme, DefaultFakePowScheme}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ExtensionCandidate, Header, HeaderChain}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.nodeView.wallet.ErgoProvingInterpreter
import org.ergoplatform.settings.{Constants, Parameters}
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.{ErgoBox, Input}
import scapi.sigma.DLogProtocol.{DLogProverInput, ProveDlog}
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.{ADDigest, ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.concurrent.duration._
import scala.util.Random

trait ChainGenerator {

  val timeProvider: NetworkTimeProvider = ErgoTestHelpers.defaultTimeProvider
  val afterGenesisDigest: ADDigest = ADDigest @@ Base16.decode("04c3b15906e39b9d9659ded8fc24e9cea7ca96468516136ec6738256730d400901").get
  val emptyStateContext: ErgoStateContext = ErgoStateContext.empty(afterGenesisDigest)
  val defaultProver = new ErgoProvingInterpreter("test seed", 1)
  val defaultMinerSecret: DLogProverInput = defaultProver.secrets.head
  val defaultMinerSecretNumber: BigInt = defaultProver.secrets.head.w
  val defaultMinerPk: ProveDlog = defaultMinerSecret.publicImage

  val powScheme: AutoleakusPowScheme = DefaultFakePowScheme
  private val EmptyStateRoot = ADDigest @@ Array.fill(HashLength + 1)(0.toByte)
  private val EmptyDigest32 = Digest32 @@ Array.fill(HashLength)(0.toByte)
  val defaultDifficultyControl = new LinearDifficultyControl(1.minute, 8, 256)
  val defaultExtension: ExtensionCandidate = ExtensionCandidate(Seq(), Seq((EmptyDigest32, EmptyDigest32)))
  val emptyExtension: ExtensionCandidate = ExtensionCandidate(Seq(), Seq())

  private def emptyProofs = SerializedAdProof @@ scorex.utils.Random.randomBytes(Random.nextInt(5000))

  /** Generates a [[HeaderChain]] of given height starting from a History last block
    */
  def genHeaderChain(height: Int, history: ErgoHistory): HeaderChain =
    genHeaderChain(height, history.bestHeaderOpt, history.difficultyCalculator)

  /** Generates a [[HeaderChain]] of given height starting from a given header
    */
  final def genHeaderChain(height: Int,
                           prefix: Option[Header] = None,
                           control: LinearDifficultyControl = defaultDifficultyControl,
                           extensionHash: Digest32 = EmptyDigest32): HeaderChain =
    HeaderChain(headerStream(prefix, control, extensionHash).take(height + prefix.size))

  /** Generates a minimal [[HeaderChain]] that satisfies the given condition
    */
  final def genHeaderChain(until: Seq[Header] => Boolean,
                           prefix: Option[Header],
                           control: LinearDifficultyControl): HeaderChain = {
    val headers = headerStream(prefix, control)
    val chain = Iterator.from(prefix.size).map(size => headers.take(size)).find(until).get
    HeaderChain(chain)
  }

  private def headerStream(prefix: Option[Header], control: LinearDifficultyControl,
                           extensionHash: Digest32 = EmptyDigest32): Stream[Header] = {
    val firstHeader = nextHeader(prefix, control, extensionHash)
    lazy val headers: Stream[Header] = firstHeader #:: headers.map(cur => nextHeader(Option(cur), control, extensionHash))
    prefix.toSeq ++: headers
  }

  def nextHeader(prev: Option[Header], control: LinearDifficultyControl,
                 extensionHash: Digest32 = EmptyDigest32): Header =
    powScheme.prove(
      prev,
      Constants.InitialNBits,
      EmptyStateRoot,
      EmptyDigest32,
      EmptyDigest32,
      prev.map(_.timestamp + control.desiredInterval.toMillis).getOrElse(0),
      extensionHash,
      defaultMinerSecretNumber
    ).get

  def genChain(height: Int): Seq[ErgoFullBlock] =
    blockStream(None).take(height)

  def genChain(height: Int, prefix: ErgoFullBlock): Seq[ErgoFullBlock] =
    blockStream(Option(prefix)).take(height + 1)

  def genChain(height: Int,
               history: ErgoHistory,
               nBits: Long = Constants.InitialNBits,
               extension: ExtensionCandidate = defaultExtension): Seq[ErgoFullBlock] = {
    val prefix = history.bestFullBlockOpt
    blockStream(prefix, nBits, extension).take(height + prefix.size)
  }

  protected def blockStream(prefix: Option[ErgoFullBlock],
                            nBits: Long = Constants.InitialNBits,
                            extension: ExtensionCandidate = defaultExtension): Stream[ErgoFullBlock] = {
    val proof = ProverResult(Array(0x7c.toByte), ContextExtension.empty)
    val inputs = IndexedSeq(Input(ADKey @@ Array.fill(32)(0: Byte), proof))
    val b = ErgoBox(Int.MaxValue, Constants.TrueLeaf)
    val outputs = IndexedSeq(ErgoBox(b.bytes.length * Parameters.MinValuePerByte, Constants.TrueLeaf))

    def txs(i: Long) = Seq(ErgoTransaction(inputs, outputs))

    lazy val blocks: Stream[ErgoFullBlock] =
      nextBlock(prefix, txs(1), extension, nBits) #::
        blocks.zip(Stream.from(2)).map({ case (prev, i) => nextBlock(Option(prev), txs(i), extension, nBits) })
    prefix ++: blocks
  }

  def nextBlock(prev: Option[ErgoFullBlock],
                txs: Seq[ErgoTransaction],
                extension: ExtensionCandidate,
                nBits: Long = Constants.InitialNBits): ErgoFullBlock =
    powScheme.proveBlock(
      prev.map(_.header),
      nBits,
      EmptyStateRoot,
      emptyProofs,
      txs,
      Math.max(timeProvider.time(), prev.map(_.header.timestamp + 1).getOrElse(timeProvider.time())),
      extension,
      defaultMinerSecretNumber
    ).get

  def applyHeaderChain(historyIn: ErgoHistory, chain: HeaderChain): ErgoHistory = {
    var history = historyIn
    chain.headers.foreach { header =>
      history = history.append(header).get._1
    }
    history
  }

  def applyChain(historyIn: ErgoHistory, blocks: Seq[ErgoFullBlock]): ErgoHistory = {
    blocks.foldLeft(historyIn) { (history, block) =>
      val historyWithBlockHeader = if (history.contains(block.header)) history else history.append(block.header).get._1
      val historyWithTxs = historyWithBlockHeader.append(block.blockTransactions).get._1
      val historyWithExtension = historyWithTxs.append(block.extension).get._1
      block.adProofs.map(p => historyWithExtension.append(p).get._1).getOrElse(historyWithExtension)
    }
  }

  def applyBlock(historyIn: ErgoHistory, block: ErgoFullBlock): ErgoHistory = applyChain(historyIn, Seq(block))
}
