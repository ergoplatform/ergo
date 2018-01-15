package org.ergoplatform.utils

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history._
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.settings.Constants.hashLength
import scorex.core.utils.{NetworkTime, NetworkTimeProvider}
import scorex.crypto.authds._
import scorex.crypto.hash._

import scala.annotation.tailrec
import scala.util.Random

trait ChainGenerator {

  val powScheme = DefaultFakePowScheme
  private val EmptyStateRoot = ADDigest @@ Array.fill(hashLength + 1)(0.toByte)
  private val EmptyDigest32 = Digest32 @@ Array.fill(hashLength)(0.toByte)
  val timeProvider: NetworkTimeProvider = new NetworkTimeProvider(ErgoSettings.read(None).scorexSettings.ntp)

  private def emptyProofs = SerializedAdProof @@ scorex.utils.Random.randomBytes(Random.nextInt(5000))

  def genHeaderChain(height: Int, history: ErgoHistory): HeaderChain =
    genHeaderChain(height, history.bestHeaderOpt.toSeq)

  final def genHeaderChain(height: Int, accIn: Seq[Header]): HeaderChain =
    genHeaderChain(acc => acc.length == accIn.length + height, accIn)

  @tailrec
  final def genHeaderChain(until: Seq[Header] => Boolean, acc: Seq[Header]): HeaderChain = if (until(acc)) {
    HeaderChain(acc.reverse)
  } else {
    val header = powScheme.prove(
      acc.headOption,
      Constants.InitialNBits,
      EmptyStateRoot,
      EmptyDigest32,
      EmptyDigest32,
      Math.max(timeProvider.time(), acc.headOption.map(_.timestamp + 1).getOrElse(timeProvider.time())),
      Array.fill(5)(0.toByte)
    ): Header
    genHeaderChain(until, header +: acc)
  }

  @tailrec
  final def genChain(height: Int, acc: Seq[ErgoFullBlock]): Seq[ErgoFullBlock] = if (height == 0) {
    acc.reverse
  } else {
    val txs = Seq(AnyoneCanSpendTransaction(IndexedSeq(height.toLong), IndexedSeq(1L)))
    val votes = Array.fill(5)(0.toByte)

    val block = powScheme.proveBlock(acc.headOption.map(_.header),
      Constants.InitialNBits,
      EmptyStateRoot,
      emptyProofs,
      txs,
      Math.max(timeProvider.time(), acc.headOption.map(_.header.timestamp + 1).getOrElse(timeProvider.time())),
      votes)

    genChain(height - 1, block +: acc)
  }

  def applyHeaderChain(historyIn: ErgoHistory, chain: HeaderChain): ErgoHistory = {
    var history = historyIn
    chain.headers.foreach { header =>
      history = history.append(header).get._1
    }
    history
  }

  def applyChain(historyIn: ErgoHistory, blocks: Seq[ErgoFullBlock]): ErgoHistory = {
    blocks.foldLeft(historyIn) { (history, block) =>
      val historyWithTxs = history.append(block.header).get._1.append(block.blockTransactions).get._1
        .ensuring(_.contains(block.blockTransactions.id))
      block.aDProofs.map(p => historyWithTxs.append(p).get._1).getOrElse(historyWithTxs)
    }
  }
}
