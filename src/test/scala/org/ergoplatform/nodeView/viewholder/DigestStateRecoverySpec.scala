package org.ergoplatform.nodeView.viewholder

import org.ergoplatform.core.{idToVersion, versionToBytes}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, StateType}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps}
import org.ergoplatform.utils.fixtures.NodeViewFixture

class DigestStateRecoverySpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
  import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock}

  property("startup recovery persists the epoch checkpoint root before replaying later headers") {
    val defaults = NodeViewTestConfig(StateType.Digest, verifyTransactions = true, popowBootstrap = false).toSettings
    val votingLength = 10
    val recoverySettings = defaults.copy(chainSettings = defaults.chainSettings.copy(
      voting = defaults.chainSettings.voting.copy(votingLength = votingLength)))

    new NodeViewFixture(recoverySettings, parameters).apply { fixture =>
      import fixture._
      val (utxoState, boxes) = createUtxoState(fixture.settings)
      try {
        var state = WrappedUtxoState(utxoState, boxes, fixture.settings)
        var parent: Option[ErgoFullBlock] = None
        val chain = (1 to votingLength + 2).map { _ =>
          val block = validFullBlock(parent, state)
          state = state.applyModifier(block)(_ => ()).get
          applyBlock(block).get
          parent = Some(block)
          block
        }
        val checkpoint = chain(votingLength - 1).header
        val tip = chain.last.header
        checkpoint.stateRoot.toSeq should not equal tip.stateRoot.toSeq
        getCurrentState.version shouldBe idToVersion(tip.id)

        // Leave history at the tip and a valid, older state on disk.
        stopNodeViewHolder()
        val stateDirectory = ErgoState.stateDir(fixture.settings)
        val olderState = DigestState.create(None, None, stateDirectory, fixture.settings)
        try {
          olderState.rollbackTo(idToVersion(chain.head.id)).get.version shouldBe idToVersion(chain.head.id)
        } finally {
          olderState.close()
        }

        // Run the actual startup caller, including epoch selection and header replay.
        startNodeViewHolder()
        val recovered = getCurrentState.asInstanceOf[DigestState]
        recovered.version shouldBe idToVersion(tip.id)
        recovered.rootDigest shouldEqual tip.stateRoot
        val checkpointKey = versionToBytes(idToVersion(checkpoint.id))
        recovered.store.get(checkpointKey).get shouldEqual checkpoint.stateRoot

        stopNodeViewHolder()
        val reopened = DigestState.create(None, None, stateDirectory, fixture.settings)
        try {
          reopened.version shouldBe idToVersion(tip.id)
          val rolledBack = reopened.rollbackTo(idToVersion(checkpoint.id)).get
          rolledBack.rootDigest shouldEqual checkpoint.stateRoot
          rolledBack.stateContext.lastHeaderOpt.map(_.id) shouldBe Some(checkpoint.id)
        } finally {
          reopened.close()
        }
      } finally {
        utxoState.closeStorage()
      }
    }
  }
}
