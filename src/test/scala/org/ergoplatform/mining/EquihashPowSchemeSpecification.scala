package org.ergoplatform.mining

import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.settings.{Algos, Constants}
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash._

class EquihashPowSchemeSpecification extends ErgoPropertyTest {

  private val nDefault = 48: Char
  private val kDefault = 5: Char

  override val powScheme = new EquihashPowScheme(nDefault, kDefault)

  @SuppressWarnings(Array("TryGet"))
  private def createValidBlock(n: Char = nDefault, k: Char = kDefault): ErgoFullBlock = {
    def loop(ts: Long): ErgoFullBlock = {
      powScheme.proveBlock(
        None,
        RequiredDifficulty.encodeCompactBits(Constants.InitialDifficulty),
        ADDigest @@ Array.fill(33)(0: Byte),
        SerializedAdProof @@ Array.emptyByteArray,
        Seq(AnyoneCanSpendTransaction(IndexedSeq.empty, IndexedSeq(10L))),
        ts,
        Algos.hash(Array.emptyByteArray)
      ).getOrElse(loop(ts + 1))
    }

    loop(0)
  }

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
