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

  private def prunedHistorySettings(votingLength: Int, utxoBootstrap: Boolean = false): ErgoSettings = initSettings.copy(
    directory = createTempDir.getAbsolutePath,
    chainSettings = initSettings.chainSettings.copy(
      epochLength = 100000000,
      useLastEpochs = 10,
      voting = initSettings.chainSettings.voting.copy(votingLength = votingLength)
    ),
    nodeSettings = initSettings.nodeSettings.copy(
      stateType = if (utxoBootstrap) StateType.Utxo else StateType.Digest,
      verifyTransactions = true,
      blocksToKeep = BlocksToKeep,
      extraIndex = false,
      utxoSettings = initSettings.nodeSettings.utxoSettings.copy(utxoBootstrap = utxoBootstrap)
    )
  )

  private def checkContinueAfterRestart(prunedFrom: Int,
                                        applied: Int = 5,
                                        votingLength: Int = initSettings.chainSettings.voting.votingLength,
                                        utxoBootstrap: Boolean = false)
                                       (beforeRestart: ErgoHistory => Unit): Unit = {
    val settings = prunedHistorySettings(votingLength, utxoBootstrap)
    var history = ErgoHistory.readOrGenerate(settings)(null)
    history.writeMinimalFullBlockHeight(GenesisHeight)
    history.isHeadersChainSyncedVar = true

    val chain = genChain(30)
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

  property("continue from the best full block after a restart when the network tip is past the kept window") {
    checkContinueAfterRestart(prunedFrom = 2)(_.readMinimalFullBlockHeight() shouldBe GenesisHeight)
  }

  property("continue from the best full block after a restart across voting epochs") {
    checkContinueAfterRestart(prunedFrom = 2, applied = 5, votingLength = 8)(
      _.readMinimalFullBlockHeight() shouldBe GenesisHeight
    )
  }

  property("continue from the best full block after a restart when an earlier start stored a floor above it") {
    checkContinueAfterRestart(prunedFrom = 6)(_.writeMinimalFullBlockHeight(30 - BlocksToKeep + 1))
  }

  property("continue from the best full block after a restart when an earlier start stored a floor above it across voting epochs") {
    checkContinueAfterRestart(prunedFrom = 8, applied = 12, votingLength = 8)(_.writeMinimalFullBlockHeight(16))
  }

  property("continue from the best full block after a restart on a node bootstrapped from a UTXO set snapshot") {
    checkContinueAfterRestart(prunedFrom = 6, utxoBootstrap = true)(
      _.writeMinimalFullBlockHeight(30 - BlocksToKeep + 1)
    )
  }

  property("continue from the common ancestor after a restart when the best full block is on a fork") {
    val settings = prunedHistorySettings(votingLength = initSettings.chainSettings.voting.votingLength)
    var history = ErgoHistory.readOrGenerate(settings)(null)
    history.writeMinimalFullBlockHeight(GenesisHeight)
    history.isHeadersChainSyncedVar = true

    val commonPrefix = genChain(3) // heights 1..3
    val shortFork = genChain(2, commonPrefix.last).tail // heights 4..5 on fork A
    val winningBranch = genChain(27, commonPrefix.last).tail // heights 4..30 on fork B

    (commonPrefix ++ shortFork).foreach(b => history = applyBlock(history, b))
    history.bestFullBlockOpt.map(_.header) shouldBe Some(shortFork.last.header)
    history.bestFullBlockOpt.get.height shouldBe 5

    // Apply winning headers 4..29 before restart, and simulate a previously elevated floor (20)
    history = applyHeaderChain(history, HeaderChain(winningBranch.init.map(_.header)))
    history.writeMinimalFullBlockHeight(30 - BlocksToKeep + 1)
    history.closeStorage()

    // Reopen and append fresh header 30 on the winning branch
    history = ErgoHistory.readOrGenerate(settings)(null)
    history.isHeadersChainSynced shouldBe false
    history = applyHeaderChain(history, HeaderChain(Seq(winningBranch.last.header)))
    history.isHeadersChainSynced shouldBe true

    // Because bestFullBlock (height 5 on shortFork) is not on the best header chain and shares
    // common ancestor height 3, minimalFullBlockHeight must be lowered to 4 (commonAncestor.height + 1)
    history.minimalFullBlockHeight shouldBe 4
    history.applicableTry(winningBranch.head.blockTransactions) shouldBe 'success

    winningBranch.foreach(b => history = applyBlock(history, b))
    history.bestFullBlockOpt.map(_.header) shouldBe Some(winningBranch.last.header)
    history.readMinimalFullBlockHeight() shouldBe (30 - BlocksToKeep + 1)
    history.closeStorage()
  }

}
