package org.ergoplatform.nodeView.viewholder

import akka.testkit.TestProbe
import org.ergoplatform.core.{idToBytes, idToVersion, versionToBytes}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.{ADProofs, HistoryModifierSerializer}
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, StateType}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps}
import org.ergoplatform.utils.fixtures.NodeViewFixture

import java.io.File
import java.nio.file.Files
import scala.concurrent.duration._

class DigestStateRecoverySpec extends ErgoCorePropertyTest with NodeViewTestOps {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
  import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock}

  private def withCertifiedChain(check: (NodeViewFixture, Seq[ErgoFullBlock]) => Unit): Unit =
    withCertifiedChain(12)(check)

  private def withCertifiedChain(length: Int)(check: (NodeViewFixture, Seq[ErgoFullBlock]) => Unit): Unit = {
    val defaults = NodeViewTestConfig(StateType.Digest, verifyTransactions = true, popowBootstrap = false).toSettings
    val indexedSettings = defaults.copy(nodeSettings = defaults.nodeSettings.copy(extraIndex = true))
    new NodeViewFixture(indexedSettings, parameters).apply { fixture =>
      import fixture._
      val (sourceState, boxes) = createUtxoState(fixture.settings)
      try {
        var state = WrappedUtxoState(sourceState, boxes, fixture.settings)
        var parent: Option[ErgoFullBlock] = None
        val chain = (1 to length).map { _ =>
          val block = validFullBlock(parent, state)
          state = state.applyModifier(block)(_ => ()).get
          applyBlock(block).get
          parent = Some(block)
          block
        }
        getCurrentState.fullBlockBodyFrontier shouldBe Some(chain.last.id)
        check(fixture, chain)
      } finally sourceState.closeStorage()
    }
  }

  private def assertCertifiedTip(state: DigestState, block: ErgoFullBlock): Unit = {
    state.version shouldBe idToVersion(block.id)
    state.fullBlockBodyFrontier shouldBe Some(block.id)
    state.rootDigest shouldEqual block.header.stateRoot
    state.stateContext.lastHeaderOpt.map(_.id) shouldBe Some(block.id)
  }

  property("startup preservation stops on missing history even when Digest indexing and transaction verification are disabled") {
    withCertifiedChain(1) { (fixture, chain) =>
      import fixture._
      stopNodeViewHolder()
      val directory = ErgoState.stateDir(fixture.settings)
      val sentinel = new File(directory, "recovery-sentinel")
      val bytes = Array[Byte](51, 52, 53)
      Files.write(sentinel.toPath, bytes)
      val historyDirectory = org.ergoplatform.nodeView.history.ErgoHistory.historyDir(fixture.settings)
      val retainedHistory = new File(nodeViewDir, "retained-history")
      Files.move(historyDirectory.toPath, retainedHistory.toPath)
      fixture.settings = fixture.settings.copy(nodeSettings = fixture.settings.nodeSettings.copy(
        extraIndex = false, verifyTransactions = false))

      val lifecycle = TestProbe()(actorSystem)
      startNodeViewHolder()
      lifecycle.watch(nodeViewHolderRef)
      lifecycle.expectTerminated(nodeViewHolderRef, 10.seconds)
      Files.readAllBytes(sentinel.toPath) shouldEqual bytes
      retainedHistory.exists() shouldBe true
      val reopened = DigestState.create(None, None, directory, fixture.settings)
      try assertCertifiedTip(reopened, chain.head) finally reopened.close()
    }
  }

  property("startup preservation stops on an unavailable best full block when Digest indexing flags are disabled") {
    withCertifiedChain(1) { (fixture, chain) =>
      import fixture._
      getHistory.dumpToDb(idToBytes(chain.head.header.ADProofsId), Array[Byte](ADProofs.modifierTypeId)).get
      stopNodeViewHolder()
      val directory = ErgoState.stateDir(fixture.settings)
      val sentinel = new File(directory, "recovery-sentinel")
      val bytes = Array[Byte](61, 62, 63)
      Files.write(sentinel.toPath, bytes)
      fixture.settings = fixture.settings.copy(nodeSettings = fixture.settings.nodeSettings.copy(
        extraIndex = false, verifyTransactions = false))

      val lifecycle = TestProbe()(actorSystem)
      startNodeViewHolder()
      lifecycle.watch(nodeViewHolderRef)
      lifecycle.expectTerminated(nodeViewHolderRef, 10.seconds)
      Files.readAllBytes(sentinel.toPath) shouldEqual bytes
      val reopened = DigestState.create(None, None, directory, fixture.settings)
      try assertCertifiedTip(reopened, chain.head) finally reopened.close()
    }
  }

  property("startup preservation still permits fresh empty Digest bootstrap") {
    val defaults = NodeViewTestConfig(StateType.Digest, verifyTransactions = false, popowBootstrap = false).toSettings
    val unindexedSettings = defaults.copy(nodeSettings = defaults.nodeSettings.copy(extraIndex = false))
    new NodeViewFixture(unindexedSettings, parameters).apply { fixture =>
      import fixture._
      getCurrentState.version shouldBe ErgoState.genesisStateVersion
      getCurrentState.rootDigest shouldEqual fixture.settings.chainSettings.genesisStateDigest
      getCurrentState.stateContext.lastHeaderOpt shouldBe None
      getCurrentState.fullBlockBodyFrontier shouldBe None
    }
  }

  property("startup configuration preserves and advances B11 certification with extra indexing disabled") {
    withCertifiedChain { (fixture, chain) =>
      import fixture._
      stopNodeViewHolder()
      val directory = ErgoState.stateDir(fixture.settings)
      val persisted = DigestState.create(None, None, directory, fixture.settings)
      try assertCertifiedTip(persisted.rollbackTo(idToVersion(chain(10).id)).get, chain(10))
      finally persisted.close()
      fixture.settings = fixture.settings.copy(nodeSettings = fixture.settings.nodeSettings.copy(extraIndex = false))

      startNodeViewHolder()
      assertCertifiedTip(getCurrentState.asInstanceOf[DigestState], chain.last)
    }
  }

  property("startup configuration fails closed on a certified B11 gap when transaction verification is disabled") {
    withCertifiedChain { (fixture, chain) =>
      import fixture._
      stopNodeViewHolder()
      val directory = ErgoState.stateDir(fixture.settings)
      val persisted = DigestState.create(None, None, directory, fixture.settings)
      try assertCertifiedTip(persisted.rollbackTo(idToVersion(chain(10).id)).get, chain(10))
      finally persisted.close()
      val sentinel = new File(directory, "recovery-sentinel")
      val bytes = Array[Byte](71, 72, 73)
      Files.write(sentinel.toPath, bytes)
      fixture.settings = fixture.settings.copy(nodeSettings = fixture.settings.nodeSettings.copy(
        extraIndex = false, verifyTransactions = false))

      val lifecycle = TestProbe()(actorSystem)
      startNodeViewHolder()
      lifecycle.watch(nodeViewHolderRef)
      lifecycle.expectTerminated(nodeViewHolderRef, 10.seconds)
      Files.readAllBytes(sentinel.toPath) shouldEqual bytes
      val reopened = DigestState.create(None, None, directory, fixture.settings)
      try assertCertifiedTip(reopened, chain(10)) finally reopened.close()
    }
  }

  property("startup configuration retains an aligned certified state when transaction verification is disabled") {
    withCertifiedChain(1) { (fixture, chain) =>
      import fixture._
      stopNodeViewHolder()
      fixture.settings = fixture.settings.copy(nodeSettings = fixture.settings.nodeSettings.copy(
        extraIndex = false, verifyTransactions = false))
      startNodeViewHolder()
      assertCertifiedTip(getCurrentState.asInstanceOf[DigestState], chain.head)
    }
  }

  property("startup configuration retains a selected header-only B12 and its B11 certificate without verification") {
    withCertifiedChain { (fixture, chain) =>
      import fixture._
      stopNodeViewHolder()
      val directory = ErgoState.stateDir(fixture.settings)
      val persisted = DigestState.create(None, None, directory, fixture.settings)
      try {
        val older = persisted.rollbackTo(idToVersion(chain(10).id)).get
        older.applyModifier(chain.last.header, None)(_ => ()).get.fullBlockBodyFrontier shouldBe Some(chain(10).id)
      } finally persisted.close()
      fixture.settings = fixture.settings.copy(nodeSettings = fixture.settings.nodeSettings.copy(
        extraIndex = false, verifyTransactions = false))
      startNodeViewHolder()
      val state = getCurrentState.asInstanceOf[DigestState]
      state.version shouldBe idToVersion(chain.last.id)
      state.rootDigest shouldEqual chain.last.header.stateRoot
      state.stateContext.lastHeaderOpt.map(_.id) shouldBe Some(chain.last.id)
      state.fullBlockBodyFrontier shouldBe Some(chain(10).id)
    }
  }

  property("startup configuration rejects a present invalid proof after rollback without forging B12 certification") {
    withCertifiedChain { (fixture, chain) =>
      import fixture._
      val invalidProof = chain.last.adProofs.get.copy(proofBytes = chain(10).adProofs.get.proofBytes)
      val serialized = HistoryModifierSerializer.toBytes(invalidProof)
      HistoryModifierSerializer.parseBytesTry(serialized) shouldBe 'success
      getHistory.dumpToDb(idToBytes(chain.last.header.ADProofsId), serialized).get
      stopNodeViewHolder()
      val directory = ErgoState.stateDir(fixture.settings)
      val persisted = DigestState.create(None, None, directory, fixture.settings)
      try {
        val older = persisted.rollbackTo(idToVersion(chain(10).id)).get
        older.applyModifier(chain.last.header, None)(_ => ()).get.fullBlockBodyFrontier shouldBe Some(chain(10).id)
      } finally persisted.close()
      val sentinel = new File(directory, "recovery-sentinel")
      val bytes = Array[Byte](81, 82, 83)
      Files.write(sentinel.toPath, bytes)
      val lifecycle = TestProbe()(actorSystem)
      startNodeViewHolder()
      lifecycle.watch(nodeViewHolderRef)
      lifecycle.expectTerminated(nodeViewHolderRef, 10.seconds)
      Files.readAllBytes(sentinel.toPath) shouldEqual bytes
      val reopened = DigestState.create(None, None, directory, fixture.settings)
      try assertCertifiedTip(reopened, chain(10)) finally reopened.close()
    }
  }

  property("certified startup replays the retained B12 body from persisted B11") {
    withCertifiedChain { (fixture, chain) =>
      import fixture._
      stopNodeViewHolder()
      val directory = ErgoState.stateDir(fixture.settings)
      val persisted = DigestState.create(None, None, directory, fixture.settings)
      try assertCertifiedTip(persisted.rollbackTo(idToVersion(chain(10).id)).get, chain(10))
      finally persisted.close()

      startNodeViewHolder()
      assertCertifiedTip(getCurrentState.asInstanceOf[DigestState], chain.last)
      stopNodeViewHolder()
      val reopened = DigestState.create(None, None, directory, fixture.settings)
      try assertCertifiedTip(reopened, chain.last) finally reopened.close()
    }
  }

  property("certified startup backfills B12 when the equal state version was reached by its header only") {
    withCertifiedChain { (fixture, chain) =>
      import fixture._
      stopNodeViewHolder()
      val directory = ErgoState.stateDir(fixture.settings)
      val persisted = DigestState.create(None, None, directory, fixture.settings)
      try {
        val older = persisted.rollbackTo(idToVersion(chain(10).id)).get
        val headerOnly = older.applyModifier(chain.last.header, None)(_ => ()).get
        headerOnly.version shouldBe idToVersion(chain.last.id)
        headerOnly.fullBlockBodyFrontier shouldBe Some(chain(10).id)
      } finally persisted.close()

      startNodeViewHolder()
      assertCertifiedTip(getCurrentState.asInstanceOf[DigestState], chain.last)
    }
  }

  property("certified startup fails closed on an unreadable suffix proof and preserves persisted state") {
    withCertifiedChain { (fixture, chain) =>
      import fixture._
      getHistory.dumpToDb(idToBytes(chain(10).header.ADProofsId), Array[Byte](ADProofs.modifierTypeId)).get
      getHistory.bestFullBlockOpt.map(_.id) shouldBe Some(chain.last.id)
      getHistory.modifierBytesById(chain(10).header.ADProofsId).get shouldEqual Array.emptyByteArray
      stopNodeViewHolder()
      val directory = ErgoState.stateDir(fixture.settings)
      val persisted = DigestState.create(None, None, directory, fixture.settings)
      try persisted.rollbackTo(idToVersion(chain(9).id)).get
      finally persisted.close()
      val sentinel = new File(directory, "recovery-sentinel")
      val bytes = Array[Byte](11, 12, 13)
      Files.write(sentinel.toPath, bytes)

      val lifecycle = TestProbe()(actorSystem)
      startNodeViewHolder()
      lifecycle.watch(nodeViewHolderRef)
      lifecycle.expectTerminated(nodeViewHolderRef, 10.seconds)
      Files.readAllBytes(sentinel.toPath) shouldEqual bytes
      val reopened = DigestState.create(None, None, directory, fixture.settings)
      try assertCertifiedTip(reopened, chain(9)) finally reopened.close()
    }
  }

  property("certified startup fails closed when its header-only state no longer retains the certified rollback anchor") {
    withCertifiedChain { (fixture, chain) =>
      import fixture._
      stopNodeViewHolder()
      val directory = ErgoState.stateDir(fixture.settings)
      val persisted = DigestState.create(None, None, directory, fixture.settings)
      try {
        val older = persisted.rollbackTo(idToVersion(chain(10).id)).get
        val headerOnly = older.applyModifier(chain.last.header, None)(_ => ()).get
        headerOnly.store.clean(0)
        headerOnly.rollbackVersions.exists(_ == idToVersion(chain(10).id)) shouldBe false
      } finally persisted.close()
      val sentinel = new File(directory, "recovery-sentinel")
      val bytes = Array[Byte](21, 22, 23)
      Files.write(sentinel.toPath, bytes)

      val lifecycle = TestProbe()(actorSystem)
      startNodeViewHolder()
      lifecycle.watch(nodeViewHolderRef)
      lifecycle.expectTerminated(nodeViewHolderRef, 10.seconds)
      Files.readAllBytes(sentinel.toPath) shouldEqual bytes
      val reopened = DigestState.create(None, None, directory, fixture.settings)
      try {
        reopened.version shouldBe idToVersion(chain.last.id)
        reopened.fullBlockBodyFrontier shouldBe Some(chain(10).id)
      } finally reopened.close()
    }
  }

  property("equal-version legacy Digest startup does not manufacture a body certificate") {
    withCertifiedChain { (fixture, chain) =>
      import fixture._
      val context = getCurrentState.stateContext
      stopNodeViewHolder()
      val directory = ErgoState.stateDir(fixture.settings)
      val legacy = DigestState.recover(idToVersion(chain.last.id), chain.last.header.stateRoot,
        context, directory, fixture.settings).get
      try legacy.fullBlockBodyFrontier shouldBe None finally legacy.close()

      startNodeViewHolder()
      getCurrentState.version shouldBe idToVersion(chain.last.id)
      getCurrentState.fullBlockBodyFrontier shouldBe None
    }
  }

  property("certified startup replays a selected fork at genesis from the retained genesis state") {
    val defaults = NodeViewTestConfig(StateType.Digest, verifyTransactions = true, popowBootstrap = false).toSettings
    val indexedSettings = defaults.copy(nodeSettings = defaults.nodeSettings.copy(extraIndex = true))
    new NodeViewFixture(indexedSettings, parameters).apply { fixture =>
      import fixture._
      val (sourceState, boxes) = createUtxoState(fixture.settings)
      try {
        val original = validFullBlock(None, sourceState, boxes)
        val forkGenesis = validFullBlock(None, sourceState, boxes)
        original.id should not equal forkGenesis.id
        val genesis = WrappedUtxoState(sourceState, boxes, fixture.settings)
        val forkState = genesis.applyModifier(forkGenesis)(_ => ()).get
        val forkTip = validFullBlock(Some(forkGenesis), forkState)
        applyBlock(original).get
        getCurrentState.fullBlockBodyFrontier shouldBe Some(original.id)

        // Persist the alternate selected full chain without applying it to the certified state.
        val history = getHistory
        Seq(forkGenesis, forkTip).foreach { block =>
          history.append(block.header).get
          block.blockSections.foreach(section => history.append(section).get)
        }
        history.bestFullBlockOpt.map(_.id) shouldBe Some(forkTip.id)
        getCurrentState.version shouldBe idToVersion(original.id)
        stopNodeViewHolder()
        startNodeViewHolder()
        assertCertifiedTip(getCurrentState.asInstanceOf[DigestState], forkTip)
      } finally sourceState.closeStorage()
    }
  }

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
