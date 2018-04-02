package org.ergoplatform.mining

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoGenerators
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash._

class EquihashPowSchemeSpecification extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with ErgoGenerators {

  private val nDefault = 48: Char
  private val kDefault = 5: Char

  val powScheme = new EquihashPowScheme(nDefault, kDefault)

  @SuppressWarnings(Array("TryGet"))
  private def createValidBlock(n: Char = nDefault, k: Char = kDefault): ErgoFullBlock = powScheme
    .proveBlock(
      None,
      RequiredDifficulty.encodeCompactBits(Constants.InitialDifficulty),
      ADDigest @@ Array.fill(33)(0: Byte),
      SerializedAdProof @@ Array.emptyByteArray,
      Seq(AnyoneCanSpendTransaction(IndexedSeq.empty, IndexedSeq(10L))),
      1L,
      Array.emptyByteArray,
      Long.MinValue,
      Long.MaxValue
    ).get

  property("Miner should generate valid block") {
    val h = createValidBlock().header
    powScheme.verify(h) shouldBe true
  }

  property("Valid block should be invalid by pow after equihash solutions modification") {
    val h = createValidBlock().header
    val invB = EquihashSolution(h.equihashSolution.ints.updated(0, 1))
    powScheme.verify(h.copy(equihashSolution = invB)) shouldBe false
  }

  property("Valid block should be invalid by pow after height modification") {
    val h = createValidBlock().header
    powScheme.verify(h.copy(height = 3)) shouldBe false
  }

  property("Valid block should be invalid by pow after AD proofs root modification") {
    val h = createValidBlock().header
    powScheme.verify(h.copy(ADProofsRoot = Digest32 @@ Array.emptyByteArray)) shouldBe false
  }
}
