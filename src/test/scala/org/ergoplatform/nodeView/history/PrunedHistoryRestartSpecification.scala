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

  // A pruned digest history on its own settings, so that a test can close it and read it again, as a restart does;
  // with `utxoBootstrap`, the UTXO state history of a node bootstrapped from a UTXO set snapshot
  private def prunedHistorySettings(votingLength: Int, utxoBootstrap: Boolean): ErgoSettings = initSettings.copy(
    directory = createTempDir.getAbsolutePath,
    chainSettings = initSettings.chainSettings.copy(epochLength = 100000000, useLastEpochs = 10,
      voting = initSettings.chainSettings.voting.copy(votingLength = votingLength)),
    nodeSettings = initSettings.nodeSettings.copy(
      stateType = if (utxoBootstrap) StateType.Utxo else StateType.Digest, verifyTransactions = true,
      blocksToKeep = BlocksToKeep, extraIndex = false,
      utxoSettings = initSettings.nodeSettings.utxoSettings.copy(utxoBootstrap = utxoBootstrap)))

  // Applies heights 1..`applied` and stores headers up to 29, runs `beforeRestart`, closes the history and reads it
  // again (the headers-synced flag is not stored, so it starts false), appends header 30 (fresh: it sets the flag; the
  // network tip is past the kept window), applies the rest up to 30 and checks the floor (the kept window's start,
  // rounded down to a voting epoch start) and the pruning from `prunedFrom` up.
  private def checkContinueAfterRestart(prunedFrom: Int,
                                        applied: Int = 5,
                                        votingLength: Int = initSettings.chainSettings.voting.votingLength,
                                        utxoBootstrap: Boolean = false)
                                       (beforeRestart: ErgoHistory => Unit): Unit = {
    val settings = prunedHistorySettings(votingLength, utxoBootstrap)
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

  property("continue from the best full block after a restart, when an earlier start stored a floor above it, " +
    "on a node bootstrapped from a UTXO set snapshot") {
    // the stored floor counts as an applied snapshot; the node has a best full block, so the floor is still lowered
    checkContinueAfterRestart(prunedFrom = 6, utxoBootstrap = true)(
      _.writeMinimalFullBlockHeight(30 - BlocksToKeep + 1))
  }

  property("continue from the common ancestor after a restart, when an earlier start stored a floor above the " +
    "best full block and that block is on a fork") {
    // adapted from #2922: full blocks 1..3, then 4..5 on a fork; the best headers chain forks at 3 and reaches 30
    val settings = prunedHistorySettings(initSettings.chainSettings.voting.votingLength, utxoBootstrap = false)
    var history = ErgoHistory.readOrGenerate(settings)(null)
    history.writeMinimalFullBlockHeight(GenesisHeight)
    history.isHeadersChainSyncedVar = true
    val prefix = genChain(3)
    val fork = genChain(2, prefix.last).tail // heights 4..5
    val best = genChain(27, prefix.last).tail // heights 4..30
    def at(h: Int) = best(h - 4)
    def sections(h: Int) = at(h).header.sectionIds.map(_._2)

    (prefix ++ fork).foreach(b => history = applyBlock(history, b))
    history = applyHeaderChain(history, HeaderChain(best.init.map(_.header)))
    history.writeMinimalFullBlockHeight(30 - BlocksToKeep + 1) // what the headers-synced latch of an earlier version writes
    history.closeStorage()

    history = ErgoHistory.readOrGenerate(settings)(null)
    history.isHeadersChainSynced shouldBe false
    history = applyHeaderChain(history, HeaderChain(Seq(best.last.header)))
    history.isHeadersChainSynced shouldBe true
    history.bestFullBlockOpt.map(_.header) shouldBe Some(fork.last.header)

    // the best chain's blocks from 4 up are needed: the floor goes to the common ancestor + 1, and block 4 is requested
    history.readMinimalFullBlockHeight() shouldBe 4
    history.nextModifiersToDownload(100, (_, _) => true).values.flatten.toSet should contain allElementsOf sections(4)
    history.applicableTry(at(4).blockTransactions) shouldBe 'success
    best.foreach(b => history = applyBlock(history, b))
    history.bestFullBlockOpt.map(_.header) shouldBe Some(best.last.header)

    val floor = history.readMinimalFullBlockHeight()
    val windowStart = 30 - BlocksToKeep + 1
    val votingLength = initSettings.chainSettings.voting.votingLength
    floor shouldBe (if (windowStart > votingLength) windowStart - windowStart % votingLength else windowStart)
    (4 until floor).filter(h => sections(h).exists(history.contains)) shouldBe empty
    (floor to 30).filterNot(h => sections(h).forall(history.contains)) shouldBe empty
    history.closeStorage()
  }

}
