package org.ergoplatform.mining

import org.ergoplatform.ErgoBoxCandidate
import org.ergoplatform.autoleakus._
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.ExtensionCandidate
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.ErgoPropertyTest
import scorex.crypto.authds.{ADDigest, SerializedAdProof}
import scorex.crypto.hash._
import sigmastate.Values.TrueLeaf

class AutoleakusPowSchemeSpecification extends ErgoPropertyTest {

  private val nDefault = 129
  private val kDefault = 128

  override val powScheme = new AutoleakusPowScheme(kDefault, nDefault)

  @SuppressWarnings(Array("TryGet"))
  private def createValidBlock(): ErgoFullBlock = {
    def loop(ts: Long): ErgoFullBlock = {
      powScheme.proveBlock(
        None,
        RequiredDifficulty.encodeCompactBits(Constants.InitialDifficulty),
        ADDigest @@ Array.fill(33)(0: Byte),
        SerializedAdProof @@ Array.emptyByteArray,
        Seq(ErgoTransaction(IndexedSeq.empty, IndexedSeq(new ErgoBoxCandidate(10, TrueLeaf)))),
        ts,
        ExtensionCandidate(Seq(), Seq()),
        defaultMinerSecret
      ).getOrElse(loop(ts + 1))
    }

    loop(0)
  }

  property("Miner should generate valid block") {
    val h = createValidBlock().header
    powScheme.verify(h) shouldBe true
  }

  property("Valid block should be invalid by pow after solution modification") {
    val h = createValidBlock().header
    val randPk = genPk(randomSecret())
    val invB = h.powSolution.copy(pk = randPk)
    powScheme.verify(h.copy(powSolution = invB)) shouldBe false
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
