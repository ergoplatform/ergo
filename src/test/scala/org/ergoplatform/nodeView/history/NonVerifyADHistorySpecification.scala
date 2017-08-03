package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.HeaderChain
import scorex.core.consensus.History.HistoryComparisonResult

class NonVerifyADHistorySpecification extends HistorySpecification {


  var history = generateHistory(verify = false, adState = true, 0)
  assert(history.bestFullBlockIdOpt.isEmpty)

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
    val chain = genHeaderChain(BlocksInChain, Seq(history.bestHeader)).tail

    history = applyHeaderChain(history, chain)
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > 1) {
        val si = ErgoSyncInfo(answer = true, Seq(chain.headers(chain.size - forkLength).id), None)
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
