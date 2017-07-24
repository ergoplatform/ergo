package org.ergoplatform

import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProof, BlockTransactions, Header, HeaderChain}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.{AnyoneCanSpendNoncedBox, AnyoneCanSpendProposition}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Algos
import scorex.core.utils.NetworkTime
import Algos.hashLength

import scala.annotation.tailrec
import scala.util.Random

trait ChainGenerator {
  @tailrec
  final def genHeaderChain(height: Int, acc: Seq[Header]): HeaderChain = if (height == 0) {
    HeaderChain(acc.reverse)
  } else {
    val header = Miner.genHeader(BigInt(1),
      acc.head,
      Array.fill(hashLength)(0.toByte),
      Array.fill(hashLength)(0.toByte),
      Array.fill(hashLength)(0.toByte),
      NetworkTime.time()): Header
    genHeaderChain(height - 1, header +: acc)
  }


  @tailrec
  final def genChain(height: Int, acc: Seq[ErgoFullBlock]): Seq[ErgoFullBlock] = if (height == 0) {
    acc.reverse
  } else {
    val txs = Seq(AnyoneCanSpendTransaction(IndexedSeq((new AnyoneCanSpendProposition, height.toLong)),
       IndexedSeq((new AnyoneCanSpendProposition, 1L)),
      NetworkTime.time()))
    val txsRoot = BlockTransactions.rootHash(txs.map(_.id))
    val proofs = scorex.utils.Random.randomBytes(Random.nextInt(5000))
    val proofsRoot = ADProof.proofDigest(proofs)
    val stateRoot = Array.fill(32)(0.toByte)

    val header = Miner.genHeader(BigInt(1),
      acc.head.header,
      stateRoot,
      proofsRoot,
      txsRoot,
      NetworkTime.time()): Header
    val blockTransactions: BlockTransactions = BlockTransactions(header.id, txs)
    val aDProofs: ADProof = ADProof(header.id, proofs)

    val block = ErgoFullBlock(header, blockTransactions, aDProofs)
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
    var history = historyIn
    blocks.foreach { block =>
      history = history.append(block.header).get._1.append(block.aDProofs).get._1.append(block.blockTransactions).get._1
    }
    history
  }

}
