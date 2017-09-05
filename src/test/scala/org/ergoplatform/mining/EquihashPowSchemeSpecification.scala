package org.ergoplatform.mining

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.EquihashPowScheme
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class EquihashPowSchemeSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators {

  private val nDefault = 48: Char
  private val kDefault = 5: Char

  val powScheme = new EquihashPowScheme(nDefault, kDefault)

  private def createValidBlock(n: Char = nDefault, k: Char = kDefault): ErgoFullBlock = {
    powScheme.proveBlock(
      None,
      RequiredDifficulty.encodeCompactBits(Constants.InitialDifficulty),
      Array.fill(33)(0: Byte),
      Array.emptyByteArray,
      Seq(AnyoneCanSpendTransaction(IndexedSeq.empty, IndexedSeq(10L))), 1L, Array.emptyByteArray)
  }

  property("Miner should generate valid block") {
    val b = createValidBlock()
    powScheme.verify(b.header) shouldBe true
  }

  property("Valid block should be invalid by pow after equihash solutions modification") {
    val b = createValidBlock()
    val invB = b.header.equihashSolutions.clone()
    invB(0) = 1
    powScheme.verify(b.copy(header = b.header.copy(equihashSolutions = invB)).header) shouldBe false
  }

  property("Valid block should be invalid by pow after height modification") {
    val b = createValidBlock()
    assert(!powScheme.verify(b.copy(header = b.header.copy(height = 3)).header))
  }

  property("Valid block should be invalid by pow after AD proofs root modification") {
    val b = createValidBlock()
    powScheme.verify(b.copy(header = b.header.copy(ADProofsRoot = Array.emptyByteArray)).header) shouldBe false
  }
}