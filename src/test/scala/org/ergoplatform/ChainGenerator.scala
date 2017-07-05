package org.ergoplatform

import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.history.ErgoHistory

import scala.annotation.tailrec

trait ChainGenerator {
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
