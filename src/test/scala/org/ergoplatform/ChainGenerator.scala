package org.ergoplatform

import org.ergoplatform.mining.Miner
import org.ergoplatform.modifiers.block.ErgoFullBlock
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.utils.NetworkTime

import scala.annotation.tailrec

trait ChainGenerator {
  @tailrec
  final def genChain(height: Int, acc: Seq[ErgoFullBlock]): Seq[ErgoFullBlock] = if (height == 0) {
    acc.reverse
  } else {
    val block = Miner.genBlock(BigInt(1), acc.head.header, Array.fill(32)(0.toByte), Seq(), NetworkTime.time())
    genChain(height - 1, block +: acc)
  }

  def applyChain(historyIn: ErgoHistory, blocks: Seq[ErgoFullBlock]): ErgoHistory = {
    var history = historyIn
    blocks.foreach { block =>
      history = history.append(block.header).get._1.append(block).get._1
    }
    history
  }

}
