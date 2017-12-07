package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.{Header, HeaderChain}
import org.ergoplatform.modifiers.state.UTXOSnapshotChunk
import org.ergoplatform.settings.Constants
import scorex.core.consensus.History.HistoryComparisonResult
import scorex.crypto.encode.Base58

class NonVerifyADHistorySpecification extends HistorySpecification {

  private def genHistory() =
    generateHistory(verifyTransactions = false, ADState = true, PoPoWBootstrap = false, blocksToKeep = 0, epochLength = 1000)
      .ensuring(_.bestFullBlockOpt.isEmpty)

  private lazy val popowHistory = ensureMinimalHeight(genHistory(), 100)

  property("missedModifiersForFullChain") {
    var history = genHistory()
    val chain = genChain(BlocksToKeep, Seq())
    history = applyHeaderChain(history, HeaderChain(chain.map(_.header)))

    history.missedModifiersForFullChain().isEmpty shouldBe true
  }

  property("Should apply UTXOSnapshotChunks") {
    forAll(randomUTXOSnapshotChunkGen) { snapshot: UTXOSnapshotChunk =>
      popowHistory.applicable(snapshot) shouldBe true
      val processInfo = popowHistory.append(snapshot).get._2
      processInfo.toApply shouldEqual Some(snapshot)
      popowHistory.applicable(snapshot) shouldBe false
    }
  }

  property("Should calculate difficulty correctly") {
    val epochLength = 3
    val useLastEpochs = 3

    var history = generateHistory(verifyTransactions = false, ADState = true, PoPoWBootstrap = false, blocksToKeep = 0,
      epochLength = epochLength, useLastEpochs = useLastEpochs)
    val blocksBeforeRecalculate = epochLength * useLastEpochs + 1

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

  property("History.isInBestChain") {
    var history = genHistory()
    val common = genHeaderChain(BlocksInChain, history)
    history = applyHeaderChain(history, common)

    val fork1 = genHeaderChain(BlocksInChain, history)
    val fork2 = genHeaderChain(BlocksInChain + 1, history)

    history = applyHeaderChain(history, fork1.tail)
    history.bestHeaderOpt.get shouldBe fork1.last
    fork1.headers.foreach(h => history.isInBestChain(h.id) shouldBe true)

    history = applyHeaderChain(history, fork2.tail)
    history.bestHeaderOpt.get shouldBe fork2.last
    fork2.headers.foreach(h => history.isInBestChain(h.id) shouldBe true)
    fork1.tail.headers.foreach(h => history.isInBestChain(h.id) shouldBe false)

  }

  property("Compare headers chain") {
    var history = genHistory()

    def getInfo(c: HeaderChain) = ErgoSyncInfo(answer = true, c.headers.map(_.id))

    val common = genHeaderChain(BlocksInChain, history)
    history = applyHeaderChain(history, common)

    val fork1 = genHeaderChain(BlocksInChain, history)
    val fork2 = genHeaderChain(BlocksInChain + 1, history)

    history = applyHeaderChain(history, fork1.tail)
    history.bestHeaderOpt.get shouldBe fork1.last

    history.compare(getInfo(fork2)) shouldBe HistoryComparisonResult.Younger
    history.compare(getInfo(fork1)) shouldBe HistoryComparisonResult.Equal
    history.compare(getInfo(fork1.take(BlocksInChain - 1))) shouldBe HistoryComparisonResult.Younger
    history.compare(getInfo(fork2.take(BlocksInChain - 1))) shouldBe HistoryComparisonResult.Younger
    history.compare(getInfo(fork2.tail)) shouldBe HistoryComparisonResult.Nonsense
  }

  property("continuationIds() on forks") {
    var history1 = genHistory()
    var history2 = genHistory()
    val inChain = genHeaderChain(2, history1)


    //put genesis
    history1 = applyHeaderChain(history1, inChain)
    history2 = applyHeaderChain(history2, inChain)
    val fork1 = genHeaderChain(BlocksInChain, history1).tail
    val fork2 = genHeaderChain(BlocksInChain, history1).tail

    //apply 2 different forks
    history1 = applyHeaderChain(history1, fork1)
    history2 = applyHeaderChain(history2, fork1.take(BlocksInChain / 3))
    history2 = applyHeaderChain(history2, fork2.take(BlocksInChain / 2))
    history2.bestHeaderOpt.get shouldBe fork2.take(BlocksInChain / 2).last
    history1.bestHeaderOpt.get shouldBe fork1.last

    val si = history2.syncInfo(false)
    val continuation = history1.continuationIds(si, BlocksInChain * 100).get

    fork1.headers.foreach(h => continuation.exists(_._2 sameElements h.id) shouldBe true)

  }

  property("continuationIds() for empty ErgoSyncInfo should contain ids of all headers") {
    var history = genHistory()
    val chain = genHeaderChain(BlocksInChain, history)
    history = applyHeaderChain(history, chain)

    val smallerLimit = 2
    val ci0 = history.continuationIds(ErgoSyncInfo(answer = false, Seq()), smallerLimit).get
    chain.headers.take(smallerLimit).map(_.encodedId) shouldEqual ci0.map(c => Base58.encode(c._2))

    val biggerLimit = BlocksInChain + 2
    val ci1 = history.continuationIds(ErgoSyncInfo(answer = false, Seq()), biggerLimit).get
    chain.headers.map(_.encodedId) should contain theSameElementsAs ci1.map(c => Base58.encode(c._2))

    val ci = history.continuationIds(ErgoSyncInfo(answer = false, Seq()), BlocksInChain).get
    ci.foreach(c => c._1 shouldBe Header.modifierTypeId)
    chain.headers.map(_.encodedId) should contain theSameElementsAs ci.map(c => Base58.encode(c._2))
  }

  property("continuationIds() for light history should contain ids of next headers in our chain") {
    var history = genHistory()

    history = ensureMinimalHeight(history, BlocksInChain + 1)
    val chain = history.lastHeaders(BlocksInChain)

    forAll(smallPositiveInt) { forkLength: Int =>
      whenever(forkLength > 1 && chain.size > forkLength) {
        val si = ErgoSyncInfo(answer = true, Seq(chain.headers(chain.size - forkLength - 1).id))
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
      history.openSurfaceIds().map(Base58.encode) shouldEqual Seq(header.encodedId)
      history.heightOf(header.id).get shouldBe (inHeight + 1)
    }
  }
}
