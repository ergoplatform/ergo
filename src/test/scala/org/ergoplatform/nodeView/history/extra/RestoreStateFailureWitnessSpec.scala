package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.consensus.ModifierSemanticValidity.Invalid
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.{ErgoState, StateType, UtxoState}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, RandomWrapper}
import org.ergoplatform.utils.fixtures.NodeViewFixture

import java.nio.file.Files
import scala.util.Try

class RestoreStateFailureWitnessSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
  import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlockWithBoxHolder}

  property("failed startup validity repair retains the persisted UTXO state") {
    val defaults = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false)
      .toSettings
    val settings = defaults.copy(nodeSettings = defaults.nodeSettings.copy(extraIndex = true))

    new NodeViewFixture(settings, parameters).apply { fixture =>
      import fixture._

      // Establish that first launch and an ordinary restart preserve a real applied state.
      getCurrentState.version shouldBe ErgoState.genesisStateVersion
      val (generatedState, boxes) = createUtxoState(fixture.settings)
      try {
        val (ancestor, remainingBoxes) = validFullBlockWithBoxHolder(None, generatedState, boxes, new RandomWrapper)
        applyBlock(ancestor).get
        val (tip, _) = validFullBlockWithBoxHolder(
          Some(ancestor), getCurrentState.asInstanceOf[UtxoState], remainingBoxes, new RandomWrapper)
        applyBlock(tip).get
        val appliedVersion = idToVersion(tip.id)
        getCurrentState.version shouldBe appliedVersion
        getHistory.bestFullBlockIdOpt shouldBe Some(tip.id)

        stopNodeViewHolder()
        startNodeViewHolder()
        getCurrentState.version shouldBe appliedVersion
        stopNodeViewHolder()

        // Keep the selected tip readable, but invalidate an ancestor section.
        // The startup validity repair must walk that ancestor and return Failure.
        val history = ErgoHistory.readOrGenerate(fixture.settings)(null)
        try {
          history.historyStorage.insert(
            Array(history.validityKey(ancestor.blockTransactions.id) -> Array(0.toByte)),
            BlockSection.emptyArray
          ).get
          history.bestFullBlockIdOpt shouldBe Some(tip.id)
          history.bestFullBlockOpt.map(_.id) shouldBe Some(tip.id)
          history.isSemanticallyValid(ancestor.blockTransactions.id) shouldBe Invalid
          history.repairAppliedFullChainValidity(tip.id).isFailure shouldBe true
        } finally {
          history.closeStorage()
        }

        val reopenedHistory = ErgoHistory.readOrGenerate(fixture.settings)(null)
        try {
          reopenedHistory.bestFullBlockIdOpt shouldBe Some(tip.id)
          reopenedHistory.bestFullBlockOpt.map(_.id) shouldBe Some(tip.id)
          reopenedHistory.isSemanticallyValid(ancestor.blockTransactions.id) shouldBe Invalid
          reopenedHistory.repairAppliedFullChainValidity(tip.id).isFailure shouldBe true
        } finally {
          reopenedHistory.closeStorage()
        }

        val stateDirectory = ErgoState.stateDir(fixture.settings)
        val marker = new java.io.File(stateDirectory, "restore-failure-must-retain")
        Files.write(marker.toPath, Array[Byte](42))
        val stateBeforeFailure = UtxoState.create(stateDirectory, fixture.settings)
        try stateBeforeFailure.version shouldBe appliedVersion
        finally stateBeforeFailure.closeStorage()

        startNodeViewHolder()
        // The test ActorSystem disables termination during coordinated shutdown.
        // Observe what the Holder publishes, then stop it to reopen the state.
        val publishedVersion = Try(getCurrentState.version)
        stopNodeViewHolder()

        val markerSurvived = Files.exists(marker.toPath)
        val versionAfterFailure = Try {
          val reopened = UtxoState.create(stateDirectory, fixture.settings)
          try reopened.version
          finally reopened.closeStorage()
        }
        withClue(s"markerSurvived=$markerSurvived, publishedVersion=$publishedVersion, " +
          s"versionAfterFailure=$versionAfterFailure: ") {
          versionAfterFailure.get shouldBe appliedVersion
          markerSurvived shouldBe true
          publishedVersion.isFailure shouldBe true
        }
      } finally {
        generatedState.closeStorage()
      }
    }
  }
}
