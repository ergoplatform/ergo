package org.ergoplatform.utils

import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Constants.hashLength
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds._
import scorex.crypto.hash._

import scala.concurrent.duration._
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

trait ChainGenerator {

  val powScheme = DefaultFakePowScheme
  private val EmptyStateRoot = ADDigest @@ Array.fill(hashLength + 1)(0.toByte)
  private val EmptyDigest32 = Digest32 @@ Array.fill(hashLength)(0.toByte)
  val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(ErgoSettings.read(None).scorexSettings.ntp)
  val defaultDifficultyControl = new LinearDifficultyControl(1.minute, 8, 256)
  val defaultVotes = Array.fill(5)(0.toByte)

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
                           votes: Array[Byte] = defaultVotes): HeaderChain =
    HeaderChain(headerStream(prefix, control, votes).take(height + prefix.size))

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
                           votes: Array[Byte] = defaultVotes): Stream[Header] = {
    val firstHeader = nextHeader(prefix, control, votes)
    lazy val headers: Stream[Header] = firstHeader #:: headers.map(cur => nextHeader(Option(cur), control, votes))
    prefix.toSeq ++: headers
  }

  def nextHeader(prev: Option[Header], control: LinearDifficultyControl,
                 votes: Array[Byte] = defaultVotes): Header =
    powScheme.prove(
      prev,
      Constants.InitialNBits,
      EmptyStateRoot,
      EmptyDigest32,
      EmptyDigest32,
      prev.map(_.timestamp + control.desiredInterval.toMillis).getOrElse(0),
      votes
    )

  def genChain(height: Int): Seq[ErgoFullBlock] =
    blockStream(None).take(height)

  def genChain(height: Int, prefix: ErgoFullBlock): Seq[ErgoFullBlock] =
    blockStream(Option(prefix)).take(height + 1)

  def genChain(height: Int, history: ErgoHistory): Seq[ErgoFullBlock] = {
    val prefix = history.bestFullBlockOpt
    blockStream(prefix).take(height + prefix.size)
  }

  protected def blockStream(prefix: Option[ErgoFullBlock]): Stream[ErgoFullBlock] = {
    def txs(i: Long) = Seq(AnyoneCanSpendTransaction(IndexedSeq(i), IndexedSeq(1L)))
    lazy val blocks: Stream[ErgoFullBlock] =
      nextBlock(prefix, txs(1)) #::
      blocks.zip(Stream.from(2)).map({case (prev, i) => nextBlock(Option(prev), txs(i))})
    prefix ++: blocks
  }

  def nextBlock(prev: Option[ErgoFullBlock], txs: Seq[AnyoneCanSpendTransaction],
                votes: Array[Byte] = defaultVotes): ErgoFullBlock =
    powScheme.proveBlock(
      prev.map(_.header),
      Constants.InitialNBits,
      EmptyStateRoot,
      emptyProofs,
      txs,
      Math.max(timeProvider.time(), prev.map(_.header.timestamp + 1).getOrElse(timeProvider.time())),
      votes
    )

  def applyHeaderChain(historyIn: ErgoHistory, chain: HeaderChain): ErgoHistory = {
    var history = historyIn
    chain.headers.foreach { header =>
      history = history.append(header).get._1
    }
    history
  }

  def applyChain(historyIn: ErgoHistory, blocks: Seq[ErgoFullBlock]): ErgoHistory = {
    blocks.foldLeft(historyIn) { (history, block) =>
      val historyWithBlockHeader = history.append(block.header).get._1
      val historyWithTxs = historyWithBlockHeader.append(block.blockTransactions).get._1
        .ensuring(_.contains(block.blockTransactions.id))
      block.aDProofs.map(p => historyWithTxs.append(p).get._1).getOrElse(historyWithTxs)
    }
  }
}
