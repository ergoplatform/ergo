package org.ergoplatform.mining

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class MinerSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators {
  val n = 48
  val k = 5

  private def createValidBlock: ErgoFullBlock = {
    Miner.genBlock(
      RequiredDifficulty.encodeCompactBits(Constants.InitialDifficulty),
      ErgoFullBlock.genesis.header, Array.emptyByteArray, ErgoFullBlock.genesis.aDProofs.get, Seq(AnyoneCanSpendTransaction(IndexedSeq.empty, IndexedSeq(10L))), 1L, Array.emptyByteArray, n, k)
  }

  property("Miner should generate valid block") {
    val b = createValidBlock
    Miner.isBlockValid(b, n, k) shouldBe true
  }

  property("Valid block should be invalid by pow after equihash solutions modification") {
    val b = createValidBlock
    val invB = b.header.equihashSolutions.clone()
    invB(0) = 1
    assert(!Miner.isBlockValid(b.copy(header = b.header.copy(equihashSolutions = invB)), n, k))
  }

  property("Valid block should be invalid by pow after height modification") {
    val b = createValidBlock
    assert(!Miner.isBlockValid(b.copy(header = b.header.copy(height = 3)), n, k))
  }

  property("Valid block should be invalid by pow after AD proofs root modification") {
    val b = createValidBlock
    assert(!Miner.isBlockValid(b.copy(header = b.header.copy(ADProofsRoot = Array.emptyByteArray)), n, k))
  }
}
