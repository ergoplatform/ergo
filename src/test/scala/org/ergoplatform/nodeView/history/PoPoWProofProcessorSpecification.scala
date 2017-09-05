package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history._
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
  val chain = genHeaderChain(acc => acc.dropRight(MaxM).count(h=> powScheme.realDifficulty(h) > Constants.InitialDifficulty * 2) > MaxK,
    history.bestHeaderOpt.toSeq).ensuring(_.headers.count(h => powScheme.realDifficulty(h) > Constants.InitialDifficulty * 2) > MaxK)

  private lazy val popowHistory = applyHeaderChain(history, chain)

  property("PoPoWProof.constructInterlinkVector") {

    //genesis
    val h1 = powScheme.prove(None, Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 0L, Array.fill(5)(0: Byte))

    h1.interlinks.length shouldBe 0

    val zeroLevelPowScheme = new FakePowScheme(Some(0))

    //first after genesis
    val h2 = zeroLevelPowScheme.prove(Some(h1), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 1L, Array.fill(5)(0: Byte))

    h2.interlinks.length shouldBe 1

    val h3 = zeroLevelPowScheme.prove(Some(h2), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 2L, Array.fill(5)(0: Byte))

    h3.interlinks.length shouldBe 1

    val oneLevelPowScheme = new FakePowScheme(Some(1))

    val h4 = oneLevelPowScheme.prove(Some(h3), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 3L, Array.fill(5)(0: Byte))

    h4.interlinks.length shouldBe 1

    val h5 = zeroLevelPowScheme.prove(Some(h4), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 4L, Array.fill(5)(0: Byte))

    h5.interlinks.length shouldBe 2

    val h6 = zeroLevelPowScheme.prove(Some(h5), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 5L, Array.fill(5)(0: Byte))

    h6.interlinks.length shouldBe 2

    val twoLevelPowScheme = new FakePowScheme(Some(2))

    val h7 = twoLevelPowScheme.prove(Some(h6), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 6L, Array.fill(5)(0: Byte))

    DefaultFakePowScheme.realDifficulty(h7) shouldBe h7.requiredDifficulty * 4

    h7.interlinks.length shouldBe 2

    val h8 = zeroLevelPowScheme.prove(Some(h7), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 7L, Array.fill(5)(0: Byte))

    h8.interlinks.length shouldBe 3

    val h9 = twoLevelPowScheme.prove(Some(h8), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 8L, Array.fill(5)(0: Byte))

    DefaultFakePowScheme.realDifficulty(h9) shouldBe h9.requiredDifficulty * 4

    h9.interlinks.length shouldBe 3

    val h10 = oneLevelPowScheme.prove(Some(h9), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 9L, Array.fill(5)(0: Byte))

    h10.interlinks.length shouldBe 3

    h10.interlinks(0).sameElements(h1.id) shouldBe true
    h10.interlinks(1).sameElements(h9.id) shouldBe true
    h10.interlinks(2).sameElements(h9.id) shouldBe true

    val h11 = zeroLevelPowScheme.prove(Some(h10), Constants.InitialNBits, Array.fill(33)(0: Byte), Array.fill(32)(0: Byte),
      Array.fill(32)(0: Byte), 10L, Array.fill(5)(0: Byte))

    h11.interlinks.length shouldBe 3

    h11.interlinks(0).sameElements(h1.id) shouldBe true
    h11.interlinks(1).sameElements(h10.id) shouldBe true
    h11.interlinks(2).sameElements(h9.id) shouldBe true

    val history = applyHeaderChain(genHistory(), HeaderChain(Seq(h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11)))
    val proof = history.constructPoPoWProof(m = 2, k = 1).get

    proof.suffix.size shouldBe 1
    proof.suffix.head.id.sameElements(h11.id) shouldBe true


    proof.i shouldBe 2

    proof.innerchain.size shouldBe 2

    proof.innerchain(0) shouldBe h7

    proof.innerchain(1) shouldBe h9


    println(h7.bytes)
    println(proof.innerchain(0).bytes)

    DefaultFakePowScheme.realDifficulty(proof.innerchain(0)) shouldBe (proof.innerchain(0).requiredDifficulty * 4)

    new PoPoWProofUtils(DefaultFakePowScheme).validate(proof).isSuccess shouldBe true
  }

  property("Valid PoPoWProof generation") {
    forAll(mkGen) { case (m, k) =>
      val proof = popowHistory.constructPoPoWProof(m, k)
      proof shouldBe 'success
      new PoPoWProofUtils(popowHistory.powScheme).validate(proof.get) shouldBe 'success
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
      new PoPoWProofUtils(popowHistory.powScheme).validate(proof) shouldBe 'success
    }
  }

  property("Valid PoPoWProof serialization") {
    forAll(mkGen) { case (m, k) =>
      val proof = popowHistory.constructPoPoWProof(m + 1, k + 1).get
      val serializer = new PoPoWProofSerializer(popowHistory.powScheme)
      val recovered = serializer.parseBytes(serializer.toBytes(proof)).get
      serializer.toBytes(proof) shouldEqual serializer.toBytes(recovered)
    }
  }

  def mkGen: Gen[(Int, Int)] = for {
    m <- Gen.choose(1, MaxM)
    k <- Gen.choose(1, MaxK)
  } yield (m, k)

  //todo: interlink check test
}
