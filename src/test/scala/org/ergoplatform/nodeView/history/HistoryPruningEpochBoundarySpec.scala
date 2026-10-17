package org.ergoplatform.nodeView.history

import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.ErgoCorePropertyTest

import java.nio.file.Files

class HistoryPruningEpochBoundarySpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants.initSettings
  import org.ergoplatform.utils.generators.ChainGenerator.{applyChain, genChain}

  property("pruning removes every block section below an advanced voting-epoch floor") {
    val root = Files.createTempDirectory("history-pruning-epoch-boundary")
    val historySettings = initSettings.copy(
      directory = root.toString,
      chainSettings = initSettings.chainSettings.copy(
        voting = initSettings.chainSettings.voting.copy(votingLength = 4)
      ),
      nodeSettings = initSettings.nodeSettings.copy(
        stateType = StateType.Utxo,
        verifyTransactions = true,
        blocksToKeep = 2,
        extraIndex = false,
        utxoSettings = initSettings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)
      )
    )

    val history = ErgoHistory.readOrGenerate(historySettings)(null)
    val blocks = try {
      history.writeMinimalFullBlockHeight(1)
      history.isHeadersChainSyncedVar = true
      val generated = genChain(9, history)
      generated.map(_.blockTransactions.id).distinct.size shouldBe 9
      applyChain(history, generated)
      history.bestFullBlockIdOpt shouldBe Some(generated.last.id)
      generated
    } finally history.closeStorage()

    val reopened = ErgoHistory.readOrGenerate(historySettings)(null)
    try {
      // At height 9, blocksToKeep = 2 advances the persisted floor from 4 to 8.
      // The cleanup cursor must cover heights 4-7 when the floor advances an epoch.
      reopened.minimalFullBlockHeight shouldBe 8
      reopened.bestFullBlockIdOpt shouldBe Some(blocks.last.id)
      blocks.slice(3, 7).foreach { block =>
        withClue(s"height ${block.header.height}: ") {
          reopened.contains(block.blockTransactions.id) shouldBe false
          reopened.contains(block.extension.id) shouldBe false
        }
      }
      blocks.drop(7).foreach { block =>
        withClue(s"retained height ${block.header.height}: ") {
          reopened.contains(block.blockTransactions.id) shouldBe true
          reopened.contains(block.extension.id) shouldBe true
        }
      }
    } finally reopened.closeStorage()
  }
}
