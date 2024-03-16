package org.ergoplatform.local

import org.ergoplatform.modifiers.history.popow.{PoPowHeader, PoPowParams}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec


class NipopowVerifierSpec extends AnyPropSpec with Matchers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._


  private val poPowParams = PoPowParams(30, 30, continuous = false)
  val toPoPoWChain = (c: Seq[ErgoFullBlock]) => c.map(b => PoPowHeader.fromBlock(b).get)

  property("processes new proofs") {
    val sizes = Seq(1000)
    sizes.foreach { size =>
      val baseChain = genChain(size)
      val branchPoint = baseChain.last
      val shortChain = toPoPoWChain(baseChain)
      val longChain = toPoPoWChain(baseChain ++ genChain(5, branchPoint).tail)
      val longestChain = toPoPoWChain(baseChain ++ genChain(50, branchPoint).tail)

      val shortProof = nipopowAlgos.prove(shortChain)(poPowParams).get
      val longProof = nipopowAlgos.prove(longChain)(poPowParams).get
      val longestProof = nipopowAlgos.prove(longestChain)(poPowParams).get

      val verifier = new NipopowVerifier(Some(baseChain.head.id))
      verifier.bestChain.length shouldBe 0

      verifier.process(shortProof)
      verifier.bestChain.length should be > 0

      verifier.process(longProof)
      verifier.bestChain.last.id shouldBe longProof.headersChain.last.id

      verifier.process(longestProof)
      verifier.bestChain.last.id shouldBe longestProof.headersChain.last.id

      verifier.process(shortProof)
      verifier.bestChain.last.id shouldBe longestProof.headersChain.last.id
    }
  }
}
