package org.ergoplatform

import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.Algos
import scorex.core.block.Block._
import scorex.core.utils.NetworkTime

import scala.annotation.tailrec

trait ChainGenerator {
  @tailrec
  final def genHeaderChain(height: Int, acc: Seq[Header]): Seq[Header] = if (height == 0) {
    acc.reverse
  } else {
    val block = Miner.genHeader(BigInt(1),
      acc.head,
      Array.fill(32)(0.toByte),
      Array.fill(32)(0.toByte),
      Array.fill(32)(0.toByte),
      Array.fill(32)(0.toByte),
      Array.fill(5)(0.toByte),
      NetworkTime.time()
    ): Header
    genHeaderChain(height - 1, block +: acc)
  }


  @tailrec
  final def genChain(height: Int, acc: Seq[ErgoFullBlock]): Seq[ErgoFullBlock] = if (height == 0) {
    acc.reverse
  } else {
    val block = ???
    genChain(height - 1, block +: acc)
  }

  def applyChain(historyIn: ErgoHistory, blocks: Seq[ErgoFullBlock]): ErgoHistory = {
    var history = historyIn
    blocks.foreach { block =>
      history = history.append(block.header).get._1.append(block.aDProofs).get._1.append(block.blockTransactions).get._1
    }
    history
  }

}
