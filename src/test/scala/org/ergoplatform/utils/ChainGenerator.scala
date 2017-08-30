package org.ergoplatform.utils

import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProof, BlockTransactions, Header, HeaderChain}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Constants
import org.ergoplatform.settings.Constants.hashLength
import scorex.core.utils.NetworkTime

import scala.annotation.tailrec
import scala.util.Random

trait ChainGenerator {

  def genHeaderChain(height: Int, history: ErgoHistory): HeaderChain =
    genHeaderChain(height, history.bestHeaderOpt.toSeq)

  final def genHeaderChain(height: Int, accIn: Seq[Header]): HeaderChain =
    genHeaderChain(acc => acc.length == accIn.length + height, accIn)

  @tailrec
  final def genHeaderChain(until: Seq[Header] => Boolean, acc: Seq[Header]): HeaderChain = if (until(acc)) {
    HeaderChain(acc.reverse)
  } else {
    val header = Miner.genHeader(Constants.InitialNBits,
      acc.headOption,
      Array.fill(hashLength + 1)(0.toByte),
      Array.fill(hashLength)(0.toByte),
      Array.fill(hashLength)(0.toByte),
      Array.fill(5)(0.toByte),
      Math.max(NetworkTime.time(), acc.headOption.map(_.timestamp + 1).getOrElse(NetworkTime.time())),
      96,
      4
    ): Header
    genHeaderChain(until, header +: acc)
  }


  @tailrec
  final def genChain(height: Int, acc: Seq[ErgoFullBlock]): Seq[ErgoFullBlock] = if (height == 0) {
    acc.reverse
  } else {
    val txs = Seq(AnyoneCanSpendTransaction(IndexedSeq(height.toLong), IndexedSeq(1L)))
    val txsRoot = BlockTransactions.rootHash(txs.map(_.id))
    val proofs = scorex.utils.Random.randomBytes(Random.nextInt(5000))
    val proofsRoot = ADProof.proofDigest(proofs)
    val stateRoot = Array.fill(32 + 1)(0.toByte)
    val votes = Array.fill(5)(0.toByte)

    val header = Miner.genHeader(Constants.InitialNBits,
      acc.headOption.map(_.header),
      stateRoot,
      proofsRoot,
      txsRoot,
      votes,
      Math.max(NetworkTime.time(), acc.headOption.map(_.header.timestamp + 1).getOrElse(NetworkTime.time())),
      96,
      5): Header
    val blockTransactions: BlockTransactions = BlockTransactions(header.id, txs)
    val aDProofs: ADProof = ADProof(header.id, proofs)

    val block = ErgoFullBlock(header, blockTransactions, Some(aDProofs))
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
      assert(historyWithTxs.contains(block.blockTransactions.id))
      block.aDProofs.map(p => historyWithTxs.append(p).get._1).getOrElse(historyWithTxs)
    }
  }

}
