package org.ergoplatform.mining

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.settings.Constants

object MinerTest extends App {
  val b = Miner.genBlock(
    RequiredDifficulty.encodeCompactBits(Constants.InitialDifficulty),
    ErgoFullBlock.genesis.header, Array.emptyByteArray, ErgoFullBlock.genesis.aDProofs.get, Seq(AnyoneCanSpendTransaction(IndexedSeq.empty, IndexedSeq(10L))), 1L, Array.emptyByteArray, 96, 5)
  println(b.id)
}
