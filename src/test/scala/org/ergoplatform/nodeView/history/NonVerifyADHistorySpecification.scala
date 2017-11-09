package org.ergoplatform.nodeView.history

import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk
import org.ergoplatform.settings.Constants
import scorex.core.consensus.History.HistoryComparisonResult

class NonVerifyADHistorySpecification extends HistorySpecification {

  private def genHistory() =
    generateHistory(verifyTransactions = false, ADState = true, PoPoWBootstrap = false, blocksToKeep = 0, epochLength = 1000)
      .ensuring(_.bestFullBlockOpt.isEmpty)

  private lazy val popowHistory = ensureMinimalHeight(genHistory(), 100)

  property("Should apply UTXOSnapshotChunks") {
    forAll(randomUTXOSnapshotChunkGen) { snapshot: UTXOSnapshotChunk =>
      popowHistory.applicable(snapshot) shouldBe true
      val processInfo = popowHistory.append(snapshot).get._2
      processInfo.toApply shouldEqual Some(snapshot)
      popowHistory.applicable(snapshot) shouldBe false
    }
  }

  property("Should calculate difficulty correctly") {
    val epochLength = 2
    val blocksBeforeRecalculate = epochLength * LinearDifficultyControl.UseLastEpochs + 1

    var history = generateHistory(verifyTransactions = false, ADState = true, PoPoWBootstrap = false, blocksToKeep = 0,
      epochLength = epochLength)

    history = applyHeaderChain(history, genHeaderChain(blocksBeforeRecalculate, history))
    history.requiredDifficulty should not be Constants.InitialDifficulty
  }

  property("lastHeaders() should return correct number of blocks") {
    forAll(smallInt) { m =>
      val lastHeaders = popowHistory.lastHeaders(m)
      if (m > 0) {
        lastHeaders.last shouldBe popowHistory.bestHeaderOpt.get
      }
      lastHeaders.length shouldBe m
    }
  }

  property("Compare headers chain") {
    var history = genHistory()

    def getInfo(c: HeaderChain) = ErgoSyncInfo(answer = true, c.headers.map(_.id), None)

    val common = genHeaderChain(BlocksInChain, history)
    history = applyHeaderChain(history, common)

    val fork1 = genHeaderChain(BlocksInChain, history)
    val fork2 = genHeaderChain(BlocksInChain + 1, history)

    history = applyHeaderChain(history, fork1.tail)
    history.bestHeaderOpt.get shouldBe fork1.last

    history.compare(getInfo(fork2)) shouldBe HistoryComparisonResult.Older
    history.compare(getInfo(fork1)) shouldBe HistoryComparisonResult.Equal
    history.compare(getInfo(fork1.take(BlocksInChain - 1))) shouldBe HistoryComparisonResult.Younger
    history.compare(getInfo(fork2.take(BlocksInChain - 1))) shouldBe HistoryComparisonResult.Younger
    history.compare(getInfo(fork2.tail)) shouldBe HistoryComparisonResult.Nonsense
  }

  property("continuationIds() for light history should contain ids of next headers in our chain") {
    var history = genHistory()

    history = ensureMinimalHeight(history, BlocksInChain + 1)
    val chain = history.lastHeaders(BlocksInChain)

    forAll(smallPositiveInt) { forkLength: Int =>
      whenever(forkLength > 1 && chain.size > forkLength) {
        val si = ErgoSyncInfo(answer = true, Seq(chain.headers(chain.size - forkLength - 1).id), None)
        val continuation = history.continuationIds(si, forkLength).get
        continuation.length shouldBe forkLength
        continuation.last._2 shouldEqual chain.last.id
        continuation.head._2 shouldEqual chain.headers(chain.size - forkLength).id
      }
    }
  }

  property("continuationHeaderChains()") {
    var history = genHistory()
    //put 2 blocks
    val inChain = genHeaderChain(2, history)
    history = applyHeaderChain(history, inChain)
    //apply 2 different forks
    val fork1 = genHeaderChain(2, history).tail
    val fork2 = genHeaderChain(3, history).tail
    history = applyHeaderChain(history, fork1)
    history = applyHeaderChain(history, fork2)
    //get continuationHeaderChains
    val continuations = history.continuationHeaderChains(inChain.last)
    continuations.length shouldBe 2
    continuations.flatMap(_.headers.tail).map(_.encodedId).toSet should contain theSameElementsAs
      (fork1.headers ++ fork2.headers).map(_.encodedId).toSet
  }

  property("commonBlockThenSuffixes()") {
    var history = genHistory()

    history = ensureMinimalHeight(history, BlocksInChain + 1)

    val forkDepth = BlocksInChain / 2
    forAll(smallInt) { forkLength: Int =>
      whenever(forkLength > forkDepth) {

        val fork1 = genHeaderChain(forkLength, history).tail
        val common = fork1.headers(forkDepth)
        val fork2 = fork1.take(forkDepth) ++ genHeaderChain(forkLength + 1, Seq(common))

        history = applyHeaderChain(history, fork1)
        history.bestHeaderOpt.get shouldBe fork1.last

        val (our, their) = history.commonBlockThenSuffixes(fork2, history.bestHeaderOpt.get, 1000)
        our.head shouldBe their.head
        our.head shouldBe common
        our.last shouldBe fork1.last
        their.last shouldBe fork2.last
      }
    }
  }

  property("Append headers to best chain in history") {
    var history = genHistory()

    val chain = genHeaderChain(BlocksInChain, history)

    chain.headers.foreach { header =>
      val inHeight = history.heightOf(header.parentId).getOrElse(-1)

      history.contains(header) shouldBe false
      history.applicable(header) shouldBe true

      history = history.append(header).get._1

      history.contains(header) shouldBe true
      history.applicable(header) shouldBe false
      history.bestHeaderOpt.get shouldBe header
      history.openSurfaceIds() shouldEqual Seq(header.id)
      history.heightOf(header.id).get shouldBe (inHeight + 1)
    }
  }
}
