package org.ergoplatform.nodeView.viewholder

import akka.pattern.ask
import akka.actor.ActorSystem
import akka.util.Timeout
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, ResetBlockchainTo}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.CurrentView
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.state.{ErgoState, StateType}
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages.RollbackForReset
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps, RandomWrapper}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlockWithBoxHolder}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

class BlockchainResetIntegrationSpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("wallet reset acknowledgement reports an unavailable registry version as failure") {
    val proto = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    val config = proto.copy(walletSettings = proto.walletSettings.copy(
      testMnemonic = Some("ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic")
    ))
    NodeViewFixture(config, parameters) { fixture =>
      implicit val ctx: org.ergoplatform.utils.NodeViewTestContext = fixture
      implicit val timeout: Timeout = Timeout(30.seconds)
      val walletActor = getCurrentView.vault.walletActor
      val unavailable = idToVersion(scorex.util.bytesToId(Array.fill(32)(1.toByte)))
      val result = Await.result((walletActor ? RollbackForReset(unavailable)).mapTo[Try[Unit]], 30.seconds)
      result.isFailure shouldBe true
    }
  }

  property("reset coordinates the real UTXO node and wallet, and rejects invalid targets without mutation") {
    val proto = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    val config = proto.copy(walletSettings = proto.walletSettings.copy(
      testMnemonic = Some("ozone drill grab fiber curtain grace pudding thank cruise elder eight picnic")
    ))
    NodeViewFixture(config, parameters) { fixture =>
      implicit val ctx: org.ergoplatform.utils.NodeViewTestContext = fixture
      implicit val timeout: Timeout = Timeout(30.seconds)
      var (generatedState, boxes) = createUtxoState(fixture.settings)
      var parent: Option[ErgoFullBlock] = None
      var blocks = Vector.empty[ErgoFullBlock]
      try {
        for (height <- 1 to 3) {
          val (block, nextBoxes) = validFullBlockWithBoxHolder(parent, generatedState, boxes, new RandomWrapper(Some(height)))
          generatedState = generatedState.applyModifier(block, None)(_ => ()).get
          applyBlock(block).isSuccess shouldBe true
          boxes = nextBoxes
          parent = Some(block)
          blocks :+= block
        }
        val before = getCurrentView
        for (height <- Seq(0, -1, 100)) {
          val rejected = Await.result((fixture.nodeViewHolderRef ? ResetBlockchainTo(height)).mapTo[Try[String]], 30.seconds)
          rejected.isFailure shouldBe true
          val unchanged = getCurrentView
          unchanged.history.headersHeight shouldBe 3
          unchanged.state.version shouldBe before.state.version
        }
        val result = Await.result((fixture.nodeViewHolderRef ? ResetBlockchainTo(2)).mapTo[Try[String]], 30.seconds)
        result.isSuccess shouldBe true
        val after = getCurrentView
        after.history.headersHeight shouldBe 2
        after.history.fullBlockHeight shouldBe 2
        after.state.version shouldBe idToVersion(blocks(1).id)
        after.history.contains(blocks(2).id) shouldBe false
        after.pool.size shouldBe 0
        Await.result(after.vault.confirmedBalances, 30.seconds).height shouldBe 2
      } finally {
        generatedState.closeStorage()
      }
    }
  }

  property("wallet timeout quarantines the real node without pruning recovery history, and restart recovers the tip") {
    val proto = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    val config = proto.copy(walletSettings = proto.walletSettings.copy(testMnemonic = None))
    NodeViewFixture(config, parameters) { fixture =>
      implicit val ctx: org.ergoplatform.utils.NodeViewTestContext = fixture
      implicit val timeout: Timeout = Timeout(30.seconds)
      var (generatedState, boxes) = createUtxoState(fixture.settings)
      var parent: Option[ErgoFullBlock] = None
      try {
        for (height <- 1 to 3) {
          val (block, nextBoxes) = validFullBlockWithBoxHolder(parent, generatedState, boxes, new RandomWrapper(Some(height)))
          generatedState = generatedState.applyModifier(block, None)(_ => ()).get
          applyBlock(block).isSuccess shouldBe true
          boxes = nextBoxes
          parent = Some(block)
        }
        val view = getCurrentView
        fixture.actorSystem.stop(view.vault.walletActor)
        val failure = Await.result((fixture.nodeViewHolderRef ? ResetBlockchainTo(2)).mapTo[Try[String]], 30.seconds)
        failure.isFailure shouldBe true
        // Test configuration disables automatic CoordinatedShutdown termination. Check that
        // the quarantined actor rejects further mutations, then complete test teardown manually.
        val retry = Await.result((fixture.nodeViewHolderRef ? ResetBlockchainTo(1)).mapTo[Try[String]], 30.seconds)
        retry.isFailure shouldBe true
        retry.failed.get.getMessage should include("stopping")
        view.history.headersHeight shouldBe 3
        fixture.stopNodeViewHolder()
        Await.result(fixture.actorSystem.terminate(), 20.seconds)

        val restartedSystem = ActorSystem()
        val restarted = ErgoNodeViewRef(fixture.settings)(restartedSystem)
        try {
          val request = GetDataFromCurrentView[ErgoState[_], CurrentView[ErgoState[_]]](v => v)
          val recovered = Await.result((restarted ? request).mapTo[CurrentView[ErgoState[_]]], 30.seconds)
          recovered.history.headersHeight shouldBe 3
          recovered.history.fullBlockHeight shouldBe 3
          recovered.state.version shouldBe idToVersion(parent.get.id)
        } finally {
          restartedSystem.stop(restarted)
          Await.result(restartedSystem.terminate(), 20.seconds)
        }
      } finally {
        generatedState.closeStorage()
      }
    }
  }
}
