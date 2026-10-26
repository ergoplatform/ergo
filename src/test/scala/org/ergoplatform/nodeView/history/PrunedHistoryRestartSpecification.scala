package org.ergoplatform.nodeView.history

import org.ergoplatform.modifiers.history.HeaderChain
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.utils.ErgoNodeTestConstants.initSettings

class PrunedHistoryRestartSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.HistoryTestHelpers._
  import org.ergoplatform.utils.generators.ChainGenerator._

  // A pruned digest history on its own settings, so that a test can close it and read it again, as a restart does
  private def prunedHistorySettings(votingLength: Int): ErgoSettings = initSettings.copy(
    directory = createTempDir.getAbsolutePath,
    chainSettings = initSettings.chainSettings.copy(epochLength = 100000000, useLastEpochs = 10,
      voting = initSettings.chainSettings.voting.copy(votingLength = votingLength)),
    nodeSettings = initSettings.nodeSettings.copy(
      stateType = StateType.Digest, verifyTransactions = true, blocksToKeep = BlocksToKeep, extraIndex = false))

  // Applies heights 1..`applied` and stores headers up to 29, runs `beforeRestart`, closes the history and reads it
  // again (the headers-synced flag is not stored, so it starts false), appends header 30 (fresh: it sets the flag; the
  // network tip is past the kept window), applies the rest up to 30 and checks the floor (the kept window's start,
  // rounded down to a voting epoch start) and the pruning from `prunedFrom` up.
  private def checkContinueAfterRestart(prunedFrom: Int,
                                        applied: Int = 5,
                                        votingLength: Int = initSettings.chainSettings.voting.votingLength)
                                       (beforeRestart: ErgoHistory => Unit): Unit = {
    val settings = prunedHistorySettings(votingLength)
    var history = ErgoHistory.readOrGenerate(settings)(null)
    history.writeMinimalFullBlockHeight(GenesisHeight)
    history.isHeadersChainSyncedVar = true
    val chain = genChain(30) // heights 1..30, stamped with the current time, so each header is fresh
    def at(h: Int) = chain(h - 1)
    def sections(h: Int) = at(h).header.sectionIds.map(_._2)

    (1 to applied).foreach(h => history = applyBlock(history, at(h)))
    history = applyHeaderChain(history, HeaderChain((applied + 1 to 29).map(at(_).header)))
    beforeRestart(history)
    history.closeStorage()

    history = ErgoHistory.readOrGenerate(settings)(null)
    history.isHeadersChainSynced shouldBe false
    history = applyHeaderChain(history, HeaderChain(Seq(at(30).header)))
    history.isHeadersChainSynced shouldBe true
    history.bestFullBlockOpt.map(_.header) shouldBe Some(at(applied).header)

    // the node continues from its best full block, so the next block must be applicable
    history.applicableTry(at(applied + 1).blockTransactions) shouldBe 'success
    (applied + 1 to 30).foreach(h => history = applyBlock(history, at(h)))
    history.bestFullBlockOpt.map(_.header) shouldBe Some(at(30).header)

    val floor = history.readMinimalFullBlockHeight()
    val windowStart = 30 - BlocksToKeep + 1
    floor shouldBe (if (windowStart > votingLength) windowStart - windowStart % votingLength else windowStart)
    (prunedFrom until floor).filter(h => sections(h).exists(history.contains)) shouldBe empty
    (floor to 30).filterNot(h => sections(h).forall(history.contains)) shouldBe empty
    history.closeStorage()
  }

  property("continue from the best full block after a restart, when the network tip is past the kept window") {
    checkContinueAfterRestart(prunedFrom = 2)(_.readMinimalFullBlockHeight() shouldBe GenesisHeight)
  }

  property("continue from the best full block after a restart, when an earlier start stored a floor above it") {
    // what the headers-synced latch of an earlier version writes for header 30
    checkContinueAfterRestart(prunedFrom = 6)(_.writeMinimalFullBlockHeight(30 - BlocksToKeep + 1))
  }

  property("continue from the best full block after a restart, when an earlier start stored a floor above it, " +
    "with voting epochs of 8 blocks") {
    // the earlier latch writes 20 rounded down to the voting epoch start 16; the floor is lowered to 13, the next block
    // rounds it down to 8 (below the best full block, 12), and it ends at 16 with 8..15 pruned
    checkContinueAfterRestart(prunedFrom = 8, applied = 12, votingLength = 8)(_.writeMinimalFullBlockHeight(16))
  }

}
