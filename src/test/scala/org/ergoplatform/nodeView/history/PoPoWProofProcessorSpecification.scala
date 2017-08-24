package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.{PoPoWProof, PoPoWProofSerializer}
import org.ergoplatform.settings.Constants
import org.scalacheck.Gen

import scala.util.Random

class PoPoWProofProcessorSpecification extends HistorySpecification {

  val MaxM = 10
  val MaxK = 10

  private def genHistory() =
    generateHistory(verify = false, adState = true, popow = false, toKeep = 0, nonce = Random.nextLong(), epoch = 1000)
      .ensuring(_.bestFullBlockOpt.isEmpty)

  val history = genHistory()
  val chain = genHeaderChain(acc => acc.dropRight(MaxM).count(_.realDifficulty > Constants.InitialDifficulty * 2) > MaxK,
    history.bestHeaderOpt.toSeq)
  private lazy val popowHistory = applyHeaderChain(history, chain)

  property("Valid PoPoWProof generation") {
    forAll(mkGen) { case (m, k) =>
      val proof = popowHistory.constructPoPoWProof(m, k)
      proof shouldBe 'success
      PoPoWProof.validate(proof.get) shouldBe 'success
    }
  }

  property("PoPoW history should be able to apply PoPoWProof proofs") {
    forAll(mkGen) { case (m, k) =>
      exitOnError {
        val proof = popowHistory.constructPoPoWProof(m, k).get

        var newHistory = generateHistory(verify = false, adState = true, popow = true, 0, Random.nextLong())
        newHistory.applicable(proof) shouldBe true
        newHistory = newHistory.append(proof).get._1
        newHistory.bestHeaderOpt.isDefined shouldBe true
      }
    }
  }

  property("non-PoPoW history should ignore PoPoWProof proofs") {
    forAll(mkGen) { case (m, k) =>
      val proof = popowHistory.constructPoPoWProof(m, k).get
      val newHistory = generateHistory(verify = false, adState = true, popow = false, 0, Random.nextLong())
      newHistory.applicable(proof) shouldBe false
    }
  }

  property("constructPoPoWProof() should generate valid proof") {
    forAll(mkGen) { case (m, k) =>
      val proof = popowHistory.constructPoPoWProof(m + 1, k + 1).get
      PoPoWProof.validate(proof) shouldBe 'success
    }
  }

  property("Valid PoPoWProof serialization") {
    forAll(mkGen) { case (m, k) =>
      val proof = popowHistory.constructPoPoWProof(m + 1, k + 1).get
      val recovered = PoPoWProofSerializer.parseBytes(PoPoWProofSerializer.toBytes(proof)).get
      PoPoWProofSerializer.toBytes(proof) shouldEqual PoPoWProofSerializer.toBytes(recovered)
    }
  }

  def mkGen: Gen[(Int, Int)] = for {
    m <- Gen.choose(1, MaxM)
    k <- Gen.choose(1, MaxK)
  } yield (m, k)

  //todo: interlink check test
}
