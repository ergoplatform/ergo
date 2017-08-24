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
  println(b.id)
}
