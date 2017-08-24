package org.ergoplatform.mining

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.settings.Constants

object MinerTest extends App {
  val n = 48
  val k = 5
  val b = Miner.genBlock(
    RequiredDifficulty.encodeCompactBits(Constants.InitialDifficulty),
    ErgoFullBlock.genesis.header, Array.emptyByteArray, ErgoFullBlock.genesis.aDProofs.get, Seq(AnyoneCanSpendTransaction(IndexedSeq.empty, IndexedSeq(10L))), 1L, Array.emptyByteArray, n, k)
  assert(Miner.isBlockValid(b, n, k))
  val invB = b.header.equihashSolutions.clone()
  invB(0) = 1
  assert(!Miner.isBlockValid(b.copy(header = b.header.copy(equihashSolutions = invB)), n, k))
  assert(!Miner.isBlockValid(b.copy(header = b.header.copy(height = 3)), n, k))
  println(b.id)
}
