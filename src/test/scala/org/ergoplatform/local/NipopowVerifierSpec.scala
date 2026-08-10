package org.ergoplatform.local

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

import org.ergoplatform.modifiers.history.popow.{PoPowHeader, PoPowParams}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec


class NipopowVerifierSpec extends AnyPropSpec with Matchers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.generators.ChainGenerator._


  private val poPowParams = PoPowParams(30, 30, continuous = false).get
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

  property("rejects proofs with invalid security parameters") {
    val baseChain = genChain(100)
    val params = PoPowParams(5, 5, continuous = false).get
    val proof = nipopowAlgos.prove(toPoPoWChain(baseChain))(params).get

    Seq(
      proof.copy(m = 0),
      proof.copy(k = 0),
      proof.copy(m = Int.MaxValue, k = 1)
    ).foreach { invalidProof =>
      invalidProof.isValid shouldBe false
      an[IllegalArgumentException] should be thrownBy
        invalidProof.serializer.toBytes(invalidProof)

      val verifier = new NipopowVerifier(Some(baseChain.head.id))
      verifier.process(invalidProof) shouldBe ValidationError
      verifier.bestChain shouldBe empty
    }
  }

  property("returns when a duplicate invalid proof is processed") {
    val baseChain = genChain(100)
    val params = PoPowParams(5, 5, continuous = false).get
    val invalidProof = nipopowAlgos.prove(toPoPoWChain(baseChain))(params).get.copy(m = 0)
    invalidProof.isValid shouldBe false
    an[IllegalArgumentException] should be thrownBy
      invalidProof.serializer.toBytes(invalidProof)
    val verifier = new NipopowVerifier(Some(baseChain.head.id))

    val firstResult = verifier.process(invalidProof)
    val secondResult = new AtomicReference[NipopowProofVerificationResult]()
    val completed = new CountDownLatch(1)
    val worker = new Thread(new Runnable {
      override def run(): Unit =
        try secondResult.set(verifier.process(invalidProof))
        finally completed.countDown()
    })
    worker.setDaemon(true)
    worker.start()

    completed.await(2, TimeUnit.SECONDS) shouldBe true
    firstResult shouldBe ValidationError
    secondResult.get() shouldBe ValidationError
    verifier.bestChain shouldBe empty
  }
}
