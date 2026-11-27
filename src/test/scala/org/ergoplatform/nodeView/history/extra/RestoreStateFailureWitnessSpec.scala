package org.ergoplatform.nodeView.history.extra

import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.{ErgoState, StateType, UtxoState}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, RandomWrapper}
import org.ergoplatform.utils.fixtures.NodeViewFixture

import java.nio.file.Files
import scala.util.Try

class RestoreStateFailureWitnessSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
  import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlockWithBoxHolder}

  property("failed startup state replay retains the persisted UTXO state") {
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
        val (tip, tipBoxes) = validFullBlockWithBoxHolder(
          Some(ancestor), getCurrentState.asInstanceOf[UtxoState], remainingBoxes, new RandomWrapper)
        applyBlock(tip).get
        val appliedVersion = idToVersion(tip.id)
        getCurrentState.version shouldBe appliedVersion
        getHistory.bestFullBlockIdOpt shouldBe Some(tip.id)

        stopNodeViewHolder()
        startNodeViewHolder()
        getCurrentState.version shouldBe appliedVersion

        // Persist a selected successor before state application, as at an interrupted startup.
        // Its sections are complete, but its declared state root cannot follow these transactions.
        val (validSuccessor, _) = validFullBlockWithBoxHolder(
          Some(tip), getCurrentState.asInstanceOf[UtxoState], tipBoxes, new RandomWrapper)
        validSuccessor.header.stateRoot.toSeq should not be tip.header.stateRoot.toSeq
        val rejectedHeader = validSuccessor.header.copy(stateRoot = tip.header.stateRoot)
        val rejectedSuccessor = ErgoFullBlock(
          rejectedHeader,
          validSuccessor.blockTransactions.copy(headerId = rejectedHeader.id),
          validSuccessor.extension.copy(headerId = rejectedHeader.id),
          validSuccessor.adProofs.map(_.copy(headerId = rejectedHeader.id)))
        stopNodeViewHolder()

        val history = ErgoHistory.readOrGenerate(fixture.settings)(null)
        try {
          rejectedSuccessor.toSeq.foreach(section => history.append(section).get)
          history.bestFullBlockIdOpt shouldBe Some(rejectedSuccessor.id)
          history.bestFullBlockOpt.map(_.id) shouldBe Some(rejectedSuccessor.id)
        } finally {
          history.closeStorage()
        }

        val reopenedHistory = ErgoHistory.readOrGenerate(fixture.settings)(null)
        try {
          reopenedHistory.bestFullBlockIdOpt shouldBe Some(rejectedSuccessor.id)
          val persistedSuccessor = reopenedHistory.bestFullBlockOpt.get
          persistedSuccessor.id shouldBe rejectedSuccessor.id
          val replayState = UtxoState.create(ErgoState.stateDir(fixture.settings), fixture.settings)
          try {
            replayState.version shouldBe appliedVersion
            replayState.applyModifier(persistedSuccessor, reopenedHistory.estimatedTip())(_ => ()).isFailure shouldBe true
            replayState.rootDigest.toSeq shouldBe tip.header.stateRoot.toSeq
          } finally replayState.closeStorage()
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
        }
      } finally {
        generatedState.closeStorage()
      }
    }
  }
}
