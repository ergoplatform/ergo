package org.ergoplatform.nodeView.history.extra

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.core.idToVersion
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{DigestFullBlockApplied, RemoteBlockApplied, Rollback}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.extra.ExtraIndexer.ReceivableMessages.GetSegmentThreshold
import org.ergoplatform.nodeView.ErgoNodeViewHolder.CurrentView
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.state.{DigestState, ErgoStateContext, ErgoStateReader, StateType}
import org.ergoplatform.settings.{Algos, CacheSettings, ErgoSettings, NetworkType}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig, NodeViewTestOps}
import org.ergoplatform.utils.fixtures.NodeViewFixture
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock}
import org.ergoplatform.wallet.utils.TestFileUtils
import scorex.util.{ModifierId, bytesToId}

import java.nio.ByteBuffer
import scala.concurrent.duration._

/** A supervised restart must recover the one-time history initialization. */
class ExtraIndexerRestartSpec extends ErgoCorePropertyTest with NodeViewTestOps with TestFileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants.{settings => baseSettings}
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  private case object IsInitialized
  private case object FailOnce
  private case class SimulateBufferedTip(header: Header)

  private def contextAt(header: Header, settings: ErgoSettings): ErgoStateContext = {
    val empty = ErgoStateContext.empty(settings.chainSettings, settings.launchParameters)
    new ErgoStateContext(Seq(header), None, empty.genesisStateDigest, empty.currentParameters,
      empty.validationSettings, empty.votingData)(settings.chainSettings)
  }

  private def digestStateAt(header: Header,
                            settings: ErgoSettings,
                            frontier: Option[ModifierId] = None): DigestState = {
    val recovered = DigestState.recover(idToVersion(header.id), header.stateRoot,
      contextAt(header, settings), createTempDir, settings).get
    if (frontier.isEmpty) recovered
    else new DigestState(recovered.version, recovered.rootDigest, recovered.store, settings) {
      override val fullBlockBodyFrontier: Option[ModifierId] = frontier
    }
  }

  private class RestartProbeIndexer(cacheSettings: CacheSettings,
                                    addressEncoder: ErgoAddressEncoder,
                                    nodeViewHolderRef: ActorRef)
    extends ExtraIndexer(cacheSettings, addressEncoder, Some(nodeViewHolderRef), true) {
    override def receive: Receive = super.receive.orElse {
      case IsInitialized => sender() ! (_history ne null)
      case FailOnce => throw new IllegalStateException("injected actor failure")
      case SimulateBufferedTip(header) =>
        val buffered = IndexerState.fromHistory(_history).copy(
          indexedHeight = header.height, indexedHeaderId = Some(header.id), caughtUp = true)
        lastAppliedHeaderOpt = Some(header)
        context.become(receive.orElse(loaded(buffered)))
        sender() ! true
    }
  }

  property("startup and supervised restart do not certify a Digest header-only state version") {
    new TestKit(ActorSystem()) {
      val dir = createTempDir
      val settings = baseSettings.copy(
        directory = dir.getAbsolutePath,
        networkType = NetworkType.TestNet,
        nodeSettings = baseSettings.nodeSettings.copy(
          stateType = StateType.Digest, extraIndex = true, headerChainDiff = 5000))
      val history = ErgoHistory.readOrGenerate(settings)(null)
      val chainState = ChainGenerator.generate(5, dir, history, None)
      val viewHolderProbe = TestProbe()
      val indexer = system.actorOf(Props(new RestartProbeIndexer(
        settings.cacheSettings, settings.chainSettings.addressEncoder, viewHolderProbe.ref)))
      val probe = TestProbe()

      // The synthetic Digest state is at this header version but has no body certificate.
      def replyWithStateAt(height: Int): Header = {
        val request = viewHolderProbe.expectMsgType[GetDataFromCurrentView[_, _]]
          .asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        val applied = history.bestHeaderAtHeight(height).get
        val state = digestStateAt(applied, settings)
        try viewHolderProbe.send(indexer, request.f(CurrentView[ErgoStateReader](history, state, null, null)))
        finally state.close()
        applied
      }

      def awaitUnindexed(): Unit = {
        Thread.sleep(1000)
        IndexerState.fromHistory(history).indexedHeight shouldBe 0
        IndexerState.fromHistory(history).indexedHeaderId shouldBe None
      }

      try {
        replyWithStateAt(2)
        probe.send(indexer, IsInitialized)
        probe.expectMsg(true)
        awaitUnindexed()

        probe.send(indexer, FailOnce)
        replyWithStateAt(4)
        probe.awaitAssert({
          probe.send(indexer, IsInitialized)
          probe.expectMsg(1.second, true)
        }, 5.seconds, 100.millis)
        awaitUnindexed()
      } finally {
        TestKit.shutdownActorSystem(system)
        history.closeStorage()
        chainState.closeStorage()
      }
    }
  }

  property("Digest body gap event stays at B10 and an uncertified restart quarantines prior rows") {
    new TestKit(ActorSystem()) {
      val dir = createTempDir
      val settings = baseSettings.copy(
        directory = dir.getAbsolutePath,
        networkType = NetworkType.TestNet,
        nodeSettings = baseSettings.nodeSettings.copy(
          stateType = StateType.Digest, extraIndex = true, headerChainDiff = 5000))
      val history = ErgoHistory.readOrGenerate(settings)(null)
      val chainState = ChainGenerator.generate(13, dir, history, None)
      val b10 = history.bestFullBlockAt(10).get
      val b13 = history.bestFullBlockAt(13).get
      val holder = TestProbe()
      val probe = TestProbe()

      def startWithUncertifiedB13(): ActorRef = {
        val indexer = system.actorOf(Props(new ExtraIndexer(
          settings.cacheSettings, settings.chainSettings.addressEncoder, Some(holder.ref), true)))
        val request = holder.expectMsgType[GetDataFromCurrentView[_, _]]
          .asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        val state = digestStateAt(b13.header, settings)
        try holder.send(indexer, request.f(CurrentView[ErgoStateReader](history, state, null, null)))
        finally state.close()
        indexer
      }

      def replyToRefresh(indexer: ActorRef, frontier: Option[ModifierId]): Unit = {
        val request = holder.expectMsgType[GetDataFromCurrentView[_, _]]
          .asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        val state = digestStateAt(b13.header, settings, frontier)
        try holder.send(indexer, request.f(CurrentView[ErgoStateReader](history, state, null, null)))
        finally state.close()
      }

      def stopAndWait(indexer: ActorRef): Unit = {
        probe.watch(indexer)
        system.stop(indexer)
        probe.expectTerminated(indexer)
      }

      try {
        val indexer = startWithUncertifiedB13()
        probe.send(indexer, GetSegmentThreshold)
        probe.expectMsg(512)
        IndexerState.fromHistory(history).indexedHeight shouldBe 0

        // Neither the header-only startup version nor the ordinary block event is evidence.
        probe.send(indexer, RemoteBlockApplied(b13.header, b13.transactions.map(_.id)))
        probe.send(indexer, GetSegmentThreshold)
        probe.expectMsg(512)
        Thread.sleep(1000)
        IndexerState.fromHistory(history).indexedHeight shouldBe 0

        probe.send(indexer, DigestFullBlockApplied(b10.header, Some(b10.id)))
        replyToRefresh(indexer, Some(b10.id))
        awaitAssert(IndexerState.fromHistory(history).indexedHeaderId shouldBe Some(b10.id),
          10.seconds, 50.millis)
        ExtraIndexer.checkpointOnSelectedFullChain(history) shouldBe true

        // B11 and B12 are still missing, so B13's actual certificate remains B10.
        probe.send(indexer, RemoteBlockApplied(b13.header, b13.transactions.map(_.id)))
        probe.send(indexer, DigestFullBlockApplied(b13.header, Some(b10.id)))
        replyToRefresh(indexer, Some(b10.id))
        probe.send(indexer, GetSegmentThreshold)
        probe.expectMsg(512)
        Thread.sleep(1000)
        IndexerState.fromHistory(history).indexedHeaderId shouldBe Some(b10.id)
        IndexerState.fromHistory(history).indexedHeight shouldBe 10
        stopAndWait(indexer)

        val restarted = startWithUncertifiedB13()
        probe.watch(restarted)
        probe.expectTerminated(restarted)
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe true
        ExtraIndexer.checkpointOnSelectedFullChain(history) shouldBe false
        IndexerState.fromHistory(history).indexedHeight shouldBe 10

        val restartedAgain = startWithUncertifiedB13()
        probe.watch(restartedAgain)
        probe.expectTerminated(restartedAgain)
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe true
        ExtraIndexer.checkpointOnSelectedFullChain(history) shouldBe false
      } finally {
        TestKit.shutdownActorSystem(system)
        history.closeStorage()
        chainState.closeStorage()
      }
    }
  }

  property("stale Digest event preserves a newer persisted certificate until a genuine rollback") {
    new TestKit(ActorSystem()) {
      val dir = createTempDir
      val settings = baseSettings.copy(
        directory = dir.getAbsolutePath,
        networkType = NetworkType.TestNet,
        nodeSettings = baseSettings.nodeSettings.copy(
          stateType = StateType.Digest, extraIndex = true, headerChainDiff = 5000))
      val history = ErgoHistory.readOrGenerate(settings)(null)
      val chainState = ChainGenerator.generate(12, dir, history, None)
      val b11 = history.bestFullBlockAt(11).get
      val b12 = history.bestFullBlockAt(12).get
      history.historyStorage.insertExtraTry(Array(
        ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(12).array,
        ExtraIndexer.IndexedHeaderIdKey -> ExtraIndexer.fastIdToBytes(b12.id)
      ), Array.empty).get
      val holder = TestProbe()
      val probe = TestProbe()
      val indexer = system.actorOf(Props(new RestartProbeIndexer(
        settings.cacheSettings, settings.chainSettings.addressEncoder, holder.ref)))

      def reply(header: Header, frontier: ModifierId): Unit = {
        val request = holder.expectMsgType[GetDataFromCurrentView[_, _]]
          .asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        val state = digestStateAt(header, settings, Some(frontier))
        try holder.send(indexer, request.f(CurrentView[ErgoStateReader](history, state, null, null)))
        finally state.close()
      }

      try {
        // A queued E11 can arrive after startup has already read paired B12 authority.
        holder.send(indexer, DigestFullBlockApplied(b11.header, Some(b11.id)))
        reply(b12.header, b12.id)
        reply(b12.header, b12.id)
        holder.send(indexer, GetSegmentThreshold)
        holder.expectMsg(512)
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe false
        IndexerState.fromHistory(history).indexedHeaderId shouldBe Some(b12.id)
        val readable = digestStateAt(b12.header, settings, Some(b12.id))
        try ExtraIndexer.checkpointOnCertifiedDigestFullChain(history, readable,
          verifyTransactions = true) shouldBe true
        finally readable.close()

        // The same lower certificate is unsafe only when the Holder now confirms it.
        probe.watch(indexer)
        probe.send(indexer, Rollback(b11.id))
        reply(b11.header, b11.id)
        probe.expectTerminated(indexer)
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe true
        IndexerState.fromHistory(history).indexedHeaderId shouldBe Some(b12.id)
      } finally {
        TestKit.shutdownActorSystem(system)
        history.closeStorage()
        chainState.closeStorage()
      }
    }
  }

  property("old Digest event after rollback cannot flush a buffered block") {
    new TestKit(ActorSystem()) {
      val dir = createTempDir
      val settings = baseSettings.copy(
        directory = dir.getAbsolutePath,
        networkType = NetworkType.TestNet,
        nodeSettings = baseSettings.nodeSettings.copy(
          stateType = StateType.Digest, extraIndex = true, headerChainDiff = 5000))
      val history = ErgoHistory.readOrGenerate(settings)(null)
      val chainState = ChainGenerator.generate(12, dir, history, None)
      val b11 = history.bestFullBlockAt(11).get
      val b12 = history.bestFullBlockAt(12).get
      history.historyStorage.insertExtraTry(Array(
        ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(11).array,
        ExtraIndexer.IndexedHeaderIdKey -> ExtraIndexer.fastIdToBytes(b11.id)
      ), Array.empty).get
      val holder = TestProbe()
      val probe = TestProbe()
      val indexer = system.actorOf(Props(new RestartProbeIndexer(
        settings.cacheSettings, settings.chainSettings.addressEncoder, holder.ref)))

      def replyAtB11(): Unit = {
        val request = holder.expectMsgType[GetDataFromCurrentView[_, _]]
          .asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        val state = digestStateAt(b11.header, settings, Some(b11.id))
        try holder.send(indexer, request.f(CurrentView[ErgoStateReader](history, state, null, null)))
        finally state.close()
      }

      try {
        replyAtB11()
        probe.send(indexer, DigestFullBlockApplied(b12.header, Some(b12.id)))
        replyAtB11()
        holder.send(indexer, GetSegmentThreshold)
        holder.expectMsg(512)
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe false
        IndexerState.fromHistory(history).indexedHeaderId shouldBe Some(b11.id)

        // Model B12 in the actor's transient batch while disk remains at B11.
        probe.send(indexer, SimulateBufferedTip(b12.header))
        probe.expectMsg(true)
        probe.watch(indexer)
        probe.send(indexer, DigestFullBlockApplied(b12.header, Some(b12.id)))
        replyAtB11()
        probe.expectTerminated(indexer)
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe true
        IndexerState.fromHistory(history).indexedHeaderId shouldBe Some(b11.id)
      } finally {
        TestKit.shutdownActorSystem(system)
        history.closeStorage()
        chainState.closeStorage()
      }
    }
  }

  property("Digest startup retries a stale paired view without quarantining empty index rows") {
    new TestKit(ActorSystem()) {
      val dir = createTempDir
      val settings = baseSettings.copy(
        directory = dir.getAbsolutePath,
        networkType = NetworkType.TestNet,
        nodeSettings = baseSettings.nodeSettings.copy(
          stateType = StateType.Digest, extraIndex = true, headerChainDiff = 5000))
      val history = ErgoHistory.readOrGenerate(settings)(null)
      val chainState = ChainGenerator.generate(5, dir, history, None)
      val tip = history.bestFullBlockOpt.get.header
      val prior = history.bestHeaderAtHeight(tip.height - 1).get
      val staleState = DigestState.recover(idToVersion(tip.id), tip.stateRoot,
        contextAt(prior, settings), createTempDir, settings).get
      val coherentState = digestStateAt(tip, settings)
      val holder = TestProbe()
      val probe = TestProbe()

      def reply(indexer: ActorRef, state: DigestState): Unit = {
        val request = holder.expectMsgType[GetDataFromCurrentView[_, _]]
          .asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        holder.send(indexer, request.f(CurrentView[ErgoStateReader](history, state, null, null)))
      }

      try {
        val rejected = system.actorOf(Props(new RestartProbeIndexer(
          settings.cacheSettings, settings.chainSettings.addressEncoder, holder.ref)))
        probe.watch(rejected)
        reply(rejected, staleState)
        reply(rejected, staleState)
        probe.expectTerminated(rejected)
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe false
        IndexerState.fromHistory(history).indexedHeight shouldBe 0

        val retried = system.actorOf(Props(new RestartProbeIndexer(
          settings.cacheSettings, settings.chainSettings.addressEncoder, holder.ref)))
        reply(retried, staleState)
        reply(retried, coherentState)
        probe.awaitAssert({
          probe.send(retried, IsInitialized)
          probe.expectMsg(1.second, true)
        }, 5.seconds, 100.millis)
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe false
        system.stop(retried)
      } finally {
        staleState.close()
        coherentState.close()
        TestKit.shutdownActorSystem(system)
        history.closeStorage()
        chainState.closeStorage()
      }
    }
  }

  property("Digest extra index cannot start with transaction verification disabled") {
    new TestKit(ActorSystem()) {
      val settings = baseSettings.copy(
        directory = createTempDir.getAbsolutePath,
        networkType = NetworkType.TestNet,
        nodeSettings = baseSettings.nodeSettings.copy(
          stateType = StateType.Digest, extraIndex = true, verifyTransactions = false))
      val history = ErgoHistory.readOrGenerate(settings)(null)
      val state = DigestState.create(None, None, createTempDir, settings)
      val holder = TestProbe()
      val probe = TestProbe()
      val indexer = system.actorOf(Props(new ExtraIndexer(
        settings.cacheSettings, settings.chainSettings.addressEncoder, Some(holder.ref), true)))
      try {
        val request = holder.expectMsgType[GetDataFromCurrentView[_, _]]
          .asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        probe.watch(indexer)
        holder.send(indexer, request.f(CurrentView[ErgoStateReader](history, state, null, null)))
        probe.expectTerminated(indexer)
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe false
        IndexerState.fromHistory(history).indexedHeight shouldBe 0
      } finally {
        state.close()
        TestKit.shutdownActorSystem(system)
        history.closeStorage()
      }
    }
  }

  property("verification-off Digest indexer preserves a certified checkpoint for verified restart") {
    new TestKit(ActorSystem()) {
      val dir = createTempDir
      val settings = baseSettings.copy(
        directory = dir.getAbsolutePath,
        networkType = NetworkType.TestNet,
        nodeSettings = baseSettings.nodeSettings.copy(
          stateType = StateType.Digest, extraIndex = true, verifyTransactions = true,
          headerChainDiff = 5000))
      val disabledSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(verifyTransactions = false))
      val history = ErgoHistory.readOrGenerate(settings)(null)
      val chainState = ChainGenerator.generate(4, dir, history, None)
      val tip = history.bestFullBlockAt(4).get
      history.historyStorage.insertExtraTry(Array(
        ExtraIndexer.IndexedHeightKey -> ByteBuffer.allocate(4).putInt(tip.header.height).array,
        ExtraIndexer.IndexedHeaderIdKey -> ExtraIndexer.fastIdToBytes(tip.id)
      ), Array.empty).get
      val holder = TestProbe()
      val probe = TestProbe()

      def start(verify: Boolean): ActorRef = {
        val indexer = system.actorOf(Props(new RestartProbeIndexer(
          settings.cacheSettings, settings.chainSettings.addressEncoder, holder.ref)))
        val request = holder.expectMsgType[GetDataFromCurrentView[_, _]]
          .asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        val state = digestStateAt(tip.header, if (verify) settings else disabledSettings, Some(tip.id))
        try holder.send(indexer, request.f(CurrentView[ErgoStateReader](history, state, null, null)))
        finally state.close()
        indexer
      }

      try {
        val disabled = start(verify = false)
        probe.watch(disabled)
        probe.expectTerminated(disabled)
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe false
        IndexerState.fromHistory(history).indexedHeaderId shouldBe Some(tip.id)

        val enabled = start(verify = true)
        probe.awaitAssert({
          probe.send(enabled, IsInitialized)
          probe.expectMsg(1.second, true)
        }, 5.seconds, 100.millis)
        probe.watch(enabled)
        probe.send(enabled, DigestFullBlockApplied(tip.header, Some(tip.id)))
        val refresh = holder.expectMsgType[GetDataFromCurrentView[_, _]]
          .asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        val disabledState = digestStateAt(tip.header, disabledSettings, Some(tip.id))
        try holder.send(enabled, refresh.f(CurrentView[ErgoStateReader](history, disabledState, null, null)))
        finally disabledState.close()
        probe.expectTerminated(enabled)
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe false
        IndexerState.fromHistory(history).indexedHeaderId shouldBe Some(tip.id)

        val reenabled = start(verify = true)
        probe.awaitAssert({
          probe.send(reenabled, IsInitialized)
          probe.expectMsg(1.second, true)
        }, 5.seconds, 100.millis)
        val readable = digestStateAt(tip.header, settings, Some(tip.id))
        try ExtraIndexer.checkpointOnCertifiedDigestFullChain(history, readable,
          verifyTransactions = true) shouldBe true
        finally readable.close()
        system.stop(reenabled)
      } finally {
        TestKit.shutdownActorSystem(system)
        history.closeStorage()
        chainState.closeStorage()
      }
    }
  }

  property("a runtime UTXO state cannot index under Digest mode provenance") {
    new TestKit(ActorSystem()) {
      val dir = createTempDir
      val settings = baseSettings.copy(
        directory = dir.getAbsolutePath,
        networkType = NetworkType.TestNet,
        nodeSettings = baseSettings.nodeSettings.copy(
          stateType = StateType.Digest, extraIndex = true, headerChainDiff = 5000))
      val history = ErgoHistory.readOrGenerate(settings)(null)
      val utxoState = ChainGenerator.generate(3, dir, history, None)
      val holder = TestProbe()
      val probe = TestProbe()
      val indexer = system.actorOf(Props(new ExtraIndexer(
        settings.cacheSettings, settings.chainSettings.addressEncoder, Some(holder.ref), true)))
      try {
        val request = holder.expectMsgType[GetDataFromCurrentView[_, _]]
          .asInstanceOf[GetDataFromCurrentView[ErgoStateReader, Any]]
        ExtraIndexer.digestIndexHasProvenance(history) shouldBe true
        probe.watch(indexer)
        holder.send(indexer, request.f(CurrentView[ErgoStateReader](history, utxoState, null, null)))
        probe.expectTerminated(indexer)
        IndexerState.fromHistory(history).indexedHeight shouldBe 0
        ExtraIndexer.digestFrontierQuarantined(history) shouldBe false
      } finally {
        TestKit.shutdownActorSystem(system)
        history.closeStorage()
        utxoState.closeStorage()
      }
    }
  }

  property("real Digest state application publishes the certificate consumed by the indexer") {
    val defaults = NodeViewTestConfig(StateType.Digest, verifyTransactions = true, popowBootstrap = false).toSettings
    val digestSettings = defaults.copy(nodeSettings = defaults.nodeSettings.copy(extraIndex = true))
    new NodeViewFixture(digestSettings, parameters).apply { fixture =>
      import fixture._
      val (sourceState, boxes) = createUtxoState(settings)
      val appliedEvents = TestProbe()(actorSystem)
      actorSystem.eventStream.subscribe(appliedEvents.ref, classOf[DigestFullBlockApplied])
      val indexer = actorSystem.actorOf(Props(new ExtraIndexer(
        settings.cacheSettings, settings.chainSettings.addressEncoder, Some(nodeViewHolderRef), true)))
      try {
        val block = validFullBlock(None, sourceState, boxes)
        applyBlock(block).get
        appliedEvents.expectMsg(DigestFullBlockApplied(block.header, Some(block.id)))
        appliedEvents.awaitAssert({
          getCurrentState.fullBlockBodyFrontier shouldBe Some(block.id)
          IndexerState.fromHistory(getHistory).indexedHeaderId shouldBe Some(block.id)
        }, 10.seconds, 50.millis)
      } finally {
        actorSystem.stop(indexer)
        sourceState.closeStorage()
      }
    }
  }

  property("Digest extra index provenance and quarantine rebuild only on fresh database reopen") {
    val dir = createTempDir
    val digestSettings = baseSettings.copy(
      directory = dir.getAbsolutePath,
      networkType = NetworkType.TestNet,
      nodeSettings = baseSettings.nodeSettings.copy(stateType = StateType.Digest, extraIndex = true))
    val utxoSettings = digestSettings.copy(nodeSettings = digestSettings.nodeSettings.copy(stateType = StateType.Utxo))
    val sentinelKey = Algos.hash("digest extra index reopen sentinel")
    val sentinelId = bytesToId(sentinelKey)

    def open(settings: org.ergoplatform.settings.ErgoSettings): ErgoHistory =
      ErgoHistory.readOrGenerate(settings)(null)
    def hasSentinel(history: ErgoHistory): Boolean =
      history.historyStorage.modifierBytesById(sentinelId).isDefined
    def hasMode(history: ErgoHistory, mode: Array[Byte]): Boolean =
      history.historyStorage.modifierBytesById(bytesToId(ExtraIndexer.IndexModeProvenanceKey))
        .exists(_.sameElements(mode))
    def addSentinel(history: ErgoHistory): Unit =
      history.historyStorage.insertExtraTry(Array(sentinelKey -> Array(7.toByte)), Array.empty).get

    val first = open(digestSettings)
    ExtraIndexer.digestIndexHasProvenance(first) shouldBe true
    addSentinel(first)
    first.closeStorage()

    val second = open(digestSettings)
    hasSentinel(second) shouldBe true // stable Digest reopen did not rebuild
    second.historyStorage.insertExtraTry(
      Array(ExtraIndexer.DigestFrontierQuarantineKey -> Array(1.toByte)), Array.empty).get
    second.closeStorage()

    val afterQuarantine = open(digestSettings)
    ExtraIndexer.digestFrontierQuarantined(afterQuarantine) shouldBe false
    ExtraIndexer.digestIndexHasProvenance(afterQuarantine) shouldBe true
    hasSentinel(afterQuarantine) shouldBe false
    addSentinel(afterQuarantine)
    afterQuarantine.closeStorage()

    val inUtxoMode = open(utxoSettings)
    ExtraIndexer.digestIndexHasProvenance(inUtxoMode) shouldBe false
    hasMode(inUtxoMode, ExtraIndexer.UtxoIndexProvenanceBytes) shouldBe true
    hasSentinel(inUtxoMode) shouldBe false
    addSentinel(inUtxoMode)
    inUtxoMode.closeStorage()

    val backInDigestMode = open(digestSettings)
    ExtraIndexer.digestIndexHasProvenance(backInDigestMode) shouldBe true
    hasSentinel(backInDigestMode) shouldBe false
    addSentinel(backInDigestMode)
    backInDigestMode.historyStorage.insertExtraTry(
      Array(ExtraIndexer.DigestFrontierQuarantineKey -> Array(1.toByte)), Array.empty).get
    backInDigestMode.closeStorage()

    val utxoAfterQuarantine = open(utxoSettings)
    ExtraIndexer.digestFrontierQuarantined(utxoAfterQuarantine) shouldBe false
    ExtraIndexer.digestIndexHasProvenance(utxoAfterQuarantine) shouldBe false
    hasMode(utxoAfterQuarantine, ExtraIndexer.UtxoIndexProvenanceBytes) shouldBe true
    hasSentinel(utxoAfterQuarantine) shouldBe false
    utxoAfterQuarantine.closeStorage()
  }
}
