package org.ergoplatform.mining

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.ADProof
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

  private val nDefault = 48: Char
  private val kDefault = 5: Char

  private def createValidBlock(n: Char = nDefault, k: Char = kDefault): ErgoFullBlock = {
    Miner.genBlock(
      RequiredDifficulty.encodeCompactBits(Constants.InitialDifficulty),
      None, Array.emptyByteArray, ADProof(Array(1.toByte), Array(1.toByte)),
      Seq(AnyoneCanSpendTransaction(IndexedSeq.empty, IndexedSeq(10L))), 1L, Array.emptyByteArray, n, k)
  }

  property("Miner should generate valid block") {
    val b = createValidBlock()
    Miner.isValidBlock(b, nDefault, kDefault) shouldBe true
  }

  property("Valid block should be invalid by pow after equihash solutions modification") {
    val b = createValidBlock()
    val invB = b.header.equihashSolutions.clone()
    invB(0) = 1
    Miner.isValidBlock(b.copy(header = b.header.copy(equihashSolutions = invB)), nDefault, kDefault) shouldBe false
  }

  property("Valid block should be invalid by pow after height modification") {
    val b = createValidBlock()
    assert(!Miner.isValidBlock(b.copy(header = b.header.copy(height = 3)), nDefault, kDefault))
  }

  property("Valid block should be invalid by pow after AD proofs root modification") {
    val b = createValidBlock()
    Miner.isValidBlock(b.copy(header = b.header.copy(ADProofsRoot = Array.emptyByteArray)), nDefault, kDefault) shouldBe false
  }
}