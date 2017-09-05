package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.{FakePowScheme, PoPoWProof, PoPoWProofSerializer}
import org.ergoplatform.settings.Constants
import org.ergoplatform.utils.NoShrink
import org.scalacheck.Gen

class PoPoWProofProcessorSpecification extends HistorySpecification with NoShrink {

  val MaxM = 10
  val MaxK = 10

  private def genHistory() =
    generateHistory(verifyTransactions = false, ADState = true, PoPoWBootstrap = false, blocksToKeep = 0, epochLength = 1000)
      .ensuring(_.bestFullBlockOpt.isEmpty)

  val history = genHistory()
  val chain = genHeaderChain(acc => acc.dropRight(MaxM).count(_.realDifficulty > Constants.InitialDifficulty * 2) > MaxK,
    history.bestHeaderOpt.toSeq).ensuring(_.headers.count(_.realDifficulty > Constants.InitialDifficulty * 2) > MaxK)
  private lazy val popowHistory = applyHeaderChain(history, chain)

  property("PoPoWProof.constructInterlinkVector"){

    //genesis
    val h1 = powScheme.prove(None, Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 0L, Array.fill(5)(0:Byte))

    h1.interlinks.length shouldBe 0

    val zeroLevelPowScheme = new FakePowScheme(Some(0))

    //first after genesis
    val h2 = zeroLevelPowScheme.prove(Some(h1), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 0L, Array.fill(5)(0:Byte))

    h2.interlinks.length shouldBe 1

    val h3 = zeroLevelPowScheme.prove(Some(h2), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 0L, Array.fill(5)(0:Byte))

    h3.interlinks.length shouldBe 1

    val oneLevelPowScheme = new FakePowScheme(Some(1))

    val h4 = oneLevelPowScheme.prove(Some(h3), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 0L, Array.fill(5)(0:Byte))

    h4.interlinks.length shouldBe 1

    val h5 = zeroLevelPowScheme.prove(Some(h4), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 0L, Array.fill(5)(0:Byte))

    h5.interlinks.length shouldBe 2

    val h6 = zeroLevelPowScheme.prove(Some(h5), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 0L, Array.fill(5)(0:Byte))

    h6.interlinks.length shouldBe 2

    val twoLevelPowScheme = new FakePowScheme(Some(2))

    val h7 = twoLevelPowScheme.prove(Some(h6), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 0L, Array.fill(5)(0:Byte))

    h7.interlinks.length shouldBe 2

    val h8 = zeroLevelPowScheme.prove(Some(h7), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 0L, Array.fill(5)(0:Byte))

    h8.interlinks.length shouldBe 3
  }

  property("Valid PoPoWProof generation") {
    forAll(mkGen) { case (m, k) =>
      val proof = popowHistory.constructPoPoWProof(m, k)
      proof shouldBe 'success
      PoPoWProof.validate(proof.get) shouldBe 'success
    }
  }

  property("PoPoW history should be able to apply PoPoWProof proofs") {
    forAll(mkGen) { case (m, k) =>
      val proof = popowHistory.constructPoPoWProof(m, k).get

      var newHistory = generateHistory(verifyTransactions = false, ADState = true, PoPoWBootstrap = true, 0)
      newHistory.applicable(proof) shouldBe true
      newHistory = newHistory.append(proof).get._1
      newHistory.bestHeaderOpt.isDefined shouldBe true
    }
  }

  property("non-PoPoW history should ignore PoPoWProof proofs") {
    forAll(mkGen) { case (m, k) =>
      val proof = popowHistory.constructPoPoWProof(m, k).get
      val newHistory = generateHistory(verifyTransactions = false, ADState = true, PoPoWBootstrap = false, 0)
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
