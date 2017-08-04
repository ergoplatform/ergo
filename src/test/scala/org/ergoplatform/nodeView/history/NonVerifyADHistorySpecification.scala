package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.HeaderChain
import scorex.core.consensus.History.HistoryComparisonResult

class NonVerifyADHistorySpecification extends HistorySpecification {


  var history = generateHistory(verify = false, adState = true, popow = false,0)
  assert(history.bestFullBlockIdOpt.isEmpty)


  property("non-PoPoW history should ignore PoPoWProof proofs") {
    history = ensureMinimalHeight(history, 100)
    val proof = history.constructPoPoWProof(5, 5).get
    val newHistory = generateHistory(verify = false, adState = true, popow = false, 0)
    newHistory.applicable(proof) shouldBe false
  }

  property("constructPoPoWProof() should generate valid proof") {
    history = ensureMinimalHeight(history, 100)
    forAll(smallInt, smallInt) { (m, k) =>
      val proof = history.constructPoPoWProof(m + 1, k + 1).get
      proof.validate.get
      proof.validate shouldBe 'success
    }
  }

  property("lastHeaders() should return correct number of blocks") {
    history = ensureMinimalHeight(history, BlocksInChain)
    forAll(smallInt) { m =>
      val lastHeaders = history.lastHeaders(m)
      if (m > 0) {
        lastHeaders.last shouldBe history.bestHeader
      }
      lastHeaders.length shouldBe m
    }
  }

  property("Compare headers chain") {
    def getInfo(c: HeaderChain) = ErgoSyncInfo(answer = true, c.headers.map(_.id), None)
    val fork1 = genHeaderChain(BlocksInChain, Seq(history.bestHeader))
    val fork2 = genHeaderChain(BlocksInChain + 1, Seq(history.bestHeader))

    history = applyHeaderChain(history, fork1.tail)
    history.bestHeader shouldBe fork1.last

    history.compare(getInfo(fork2)) shouldBe HistoryComparisonResult.Older
    history.compare(getInfo(fork1)) shouldBe HistoryComparisonResult.Equal
    history.compare(getInfo(fork1.take(BlocksInChain - 1))) shouldBe HistoryComparisonResult.Younger
    history.compare(getInfo(fork2.take(BlocksInChain - 1))) shouldBe HistoryComparisonResult.Younger
    history.compare(getInfo(fork2.tail)) shouldBe HistoryComparisonResult.Nonsense
  }

  property("continuationIds() for light history should contain ids of next headers in our chain") {
    history = ensureMinimalHeight(history, BlocksInChain + 1)
    val chain = history.lastHeaders(BlocksInChain)

    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > 1 && chain.size > forkLength) {
        val si = ErgoSyncInfo(answer = true, Seq(chain.headers(chain.size - forkLength - 1).id), None)
        val continuation = history.continuationIds(si, forkLength).get
        continuation.length shouldBe forkLength
        continuation.last._2 shouldEqual chain.last.id
        continuation.head._2 shouldEqual chain.headers(chain.size - forkLength).id
      }
    }
  }

  property("commonBlockThenSuffixes()") {
    val forkDepth = BlocksInChain / 2
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > forkDepth) {

        val fork1 = genHeaderChain(forkLength, Seq(history.bestHeader)).tail
        val common = fork1.headers(forkDepth)
        val fork2 = fork1.take(forkDepth) ++ genHeaderChain(forkLength + 1, Seq(common))

        history = applyHeaderChain(history, fork1)
        history.bestHeader shouldBe fork1.last

        val (our, their) = history.commonBlockThenSuffixes(fork2, history.bestHeader)
        our.head shouldBe their.head
        our.head shouldBe common
        our.last shouldBe fork1.last
        their.last shouldBe fork2.last
      }
    }
  }

  property("Append headers to best chain in history") {
    val chain = genHeaderChain(BlocksInChain, Seq(history.bestHeader)).tail
    chain.headers.foreach { header =>
      val inHeight = history.heightOf(header.parentId).get
      history.contains(header) shouldBe false
      history.applicable(header) shouldBe true

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.applicable(header) shouldBe false
      history.bestHeader shouldBe header
      history.openSurfaceIds() shouldEqual Seq(header.id)
      history.heightOf(header.id).get shouldBe (inHeight + 1)
    }
  }

}
