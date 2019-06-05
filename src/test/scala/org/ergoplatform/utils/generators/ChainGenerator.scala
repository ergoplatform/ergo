package org.ergoplatform.utils.generators

import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.modifiers.{BlockSection, ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.{BoxUtils, ErgoTestConstants}
import org.ergoplatform.{ErgoBox, Input}
import scorex.core.PersistentNodeViewModifier
import scorex.crypto.authds.{ADKey, SerializedAdProof}
import scorex.crypto.hash.Digest32
import scorex.util.ModifierId
import sigmastate.Values
import sigmastate.interpreter.{ContextExtension, ProverResult}
import sigmastate.eval._
import scala.util.Random

trait ChainGenerator extends ErgoTestConstants {

  private def emptyProofs = SerializedAdProof @@ scorex.utils.Random.randomBytes(Random.nextInt(5000))

  /** Generates a [[HeaderChain]] of given height starting from a History last block
    */
  def genHeaderChain(height: Int, history: ErgoHistory): HeaderChain = {
    val bestHeaderOpt = history.bestHeaderOpt
    val bestHeaderInterlinksOpt = bestHeaderOpt
      .flatMap(h => history.typedModifierById[Extension](h.extensionId))
      .map(ext => PoPowAlgos.unpackInterlinks(ext.fields).get)
      .getOrElse(Seq.empty)
    genHeaderChain(height, bestHeaderOpt, bestHeaderInterlinksOpt, history.difficultyCalculator)
  }

  /** Generates a [[HeaderChain]] of given height starting from a given header
    */
  final def genHeaderChain(height: Int,
                           prefixOpt: Option[Header] = None,
                           prefixInterlinksOpt: Seq[ModifierId] = Seq.empty,
                           control: LinearDifficultyControl = defaultDifficultyControl,
                           extensionHash: Digest32 = EmptyDigest32): HeaderChain =
    HeaderChain(headerStream(prefixOpt, control, extensionHash).take(height + prefixOpt.size))

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
      Header.CurrentVersion,
      settings.chainSettings.initialNBits,
      EmptyStateRoot,
      EmptyDigest32,
      EmptyDigest32,
      prev.map(_.timestamp + control.desiredInterval.toMillis).getOrElse(0),
      extensionHash,
      Array.fill(3)(0: Byte),
      defaultMinerSecretNumber
    ).get

  def genChain(height: Int): Seq[ErgoFullBlock] =
    blockStream(None).take(height)

  def genChain(height: Int, prefix: ErgoFullBlock): Seq[ErgoFullBlock] =
    blockStream(Option(prefix)).take(height + 1)

  def genChain(height: Int,
               history: ErgoHistory,
               nBits: Long = settings.chainSettings.initialNBits,
               extension: ExtensionCandidate = defaultExtension): Seq[ErgoFullBlock] = {
    val prefix = history.bestFullBlockOpt
    blockStream(prefix, nBits, extension).take(height + prefix.size)
  }

  protected def blockStream(prefix: Option[ErgoFullBlock],
                            nBits: Long = settings.chainSettings.initialNBits,
                            extension: ExtensionCandidate = defaultExtension): Stream[ErgoFullBlock] = {
    val proof = ProverResult(Array(0x7c.toByte), ContextExtension.empty)
    val inputs = IndexedSeq(Input(ADKey @@ Array.fill(32)(0: Byte), proof))
    val minimalAmount = BoxUtils.minimalErgoAmountSimulated(Constants.TrueLeaf, Colls.emptyColl, Map(), parameters)
    val outputs = IndexedSeq(ErgoBox(minimalAmount, Constants.TrueLeaf, creationHeight = startHeight))

    def txs(i: Long) = Seq(ErgoTransaction(inputs, outputs))

    lazy val blocks: Stream[ErgoFullBlock] =
      nextBlock(prefix, txs(1), extension, nBits) #::
        blocks.zip(Stream.from(2)).map({ case (prev, i) => nextBlock(Option(prev), txs(i), extension, nBits) })
    prefix ++: blocks
  }

  def nextBlock(prev: Option[ErgoFullBlock],
                txs: Seq[ErgoTransaction],
                extension: ExtensionCandidate,
                nBits: Long = settings.chainSettings.initialNBits): ErgoFullBlock = {
    val interlinks = prev.toSeq.flatMap(x =>
      PoPowAlgos.updateInterlinks(x.header, PoPowAlgos.unpackInterlinks(x.extension.fields).get))
    val validExtension = extension ++ PoPowAlgos.interlinksToExtension(interlinks)
    powScheme.proveBlock(
      prev.map(_.header),
      Header.CurrentVersion,
      nBits,
      EmptyStateRoot,
      emptyProofs,
      txs,
      Math.max(timeProvider.time(), prev.map(_.header.timestamp + 1).getOrElse(timeProvider.time())),
      validExtension,
      Array.fill(3)(0: Byte),
      defaultMinerSecretNumber
    ).get
  }

  def applyHeaderChain(historyIn: ErgoHistory, chain: HeaderChain): ErgoHistory = {
    var history = historyIn
    chain.headers.foreach { header =>
      history = history.append(header).get._1
    }
    history
  }

  def applyChain(historyIn: ErgoHistory, blocks: Seq[ErgoFullBlock]): ErgoHistory = {
    def appendOrPass(mod: ErgoPersistentModifier, history: ErgoHistory) =
      if (history.contains(mod)) history else history.append(mod).get._1
    blocks.foldLeft(historyIn) { (history, block) =>
      val historyWithBlockHeader = appendOrPass(block.header, history)
      val historyWithTxs = appendOrPass(block.blockTransactions, historyWithBlockHeader)
      val historyWithExtension = appendOrPass(block.extension, historyWithTxs)
      block.adProofs.map(p => appendOrPass(p, historyWithExtension)).getOrElse(historyWithExtension)
    }
  }

  def applyBlock(historyIn: ErgoHistory, block: ErgoFullBlock): ErgoHistory = applyChain(historyIn, Seq(block))

  def applySection(historyIn: ErgoHistory, section: BlockSection): ErgoHistory = historyIn.append(section).get._1

}
