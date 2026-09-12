package org.ergoplatform.nodeView.viewholder

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.TestProbe
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import java.io.{File, IOException}
import java.nio.file.Files
import java.util.concurrent.atomic.AtomicBoolean
import org.ergoplatform.core.{idToVersion, versionToBytes}
import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.modifiers.{ErgoFullBlock, SnapshotsInfoTypeId}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedState, CurrentWalletView, InitStateFromSnapshot, RequestCurrentWalletView, UtxoSnapshotAppliedToState, UtxoSnapshotStateRestorationFailed}
import org.ergoplatform.nodeView.{ErgoNodeViewHolder, ErgoNodeViewRef}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, StateType, UtxoState, UtxoStateReader}
import org.ergoplatform.nodeView.state.wrapped.WrappedUtxoState
import org.ergoplatform.serialization.ManifestSerializer
import org.ergoplatform.settings.{Algos, ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestContext, NodeViewTestOps}
import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
import org.ergoplatform.utils.generators.ValidBlocksGenerators.validFullBlock
import org.ergoplatform.wallet.utils.FileUtils
import scorex.db.{LDBFactory, LDBVersionedStore, StoreRegistry}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/** Exercises the snapshot producer and closes the actual stores before reopening the configured state mode. */
class DigestSnapshotBootstrapSpecification extends ErgoCorePropertyTest with NodeViewTestOps with FileUtils {

  private def parsedSettings(directory: File, mode: StateType): ErgoSettings = {
    Files.createDirectories(directory.toPath.resolve("wallet/keystore"))
    val config = ConfigFactory.parseString(
      s"""ergo.node.stateType = "${mode.stateTypeName}"
         |ergo.node.utxo.utxoBootstrap = true
         |ergo.node.nipopow.nipopowBootstrap = false
         |ergo.node.verifyTransactions = true
         |ergo.node.mining = false
         |ergo.node.extraIndex = false
         |ergo.chain.voting.votingLength = 20
         |""".stripMargin)
      .withValue("ergo.directory", ConfigValueFactory.fromAnyRef(directory.getAbsolutePath))
      .withValue("ergo.wallet.secretStorage.secretDir",
        ConfigValueFactory.fromAnyRef(new File(directory, "wallet/keystore").getAbsolutePath))
      .withFallback(ConfigFactory.load())
      .resolve()
    val parsed = ErgoSettingsReader.fromConfig(config)
    parsed.nodeSettings.stateType shouldBe mode
    parsed.nodeSettings.utxoSettings.utxoBootstrap shouldBe true
    parsed.nodeSettings.nipopowSettings.nipopowBootstrap shouldBe false
    parsed.nodeSettings.verifyTransactions shouldBe true
    parsed.nodeSettings.mining shouldBe false
    parsed.copy(chainSettings = parsed.chainSettings.copy(
      powScheme = new DefaultFakePowScheme(parsed.chainSettings.powScheme.k, parsed.chainSettings.powScheme.n)))
  }

  // The registry reuses unclosed DB handles, so actor recreation alone cannot establish a disk reopen.
  private def closeOwnedStores(directory: File): Unit = {
    val root = directory.getCanonicalFile.toPath
    val registry = LDBFactory.factory.asInstanceOf[StoreRegistry]
    registry.lock.writeLock().lock()
    try {
      registry.map.toVector.collect {
        case (path, db) if path.getCanonicalFile.toPath.startsWith(root) => db
      }.foreach(_.close())
      registry.map.keys.exists(_.getCanonicalFile.toPath.startsWith(root)) shouldBe false
    } finally registry.lock.writeLock().unlock()
  }

  private class Session(override val settings: ErgoSettings,
                        selfShutdown: Boolean = false,
                        injectedFailure: Option[(String, AtomicBoolean)] = None) extends NodeViewTestContext {
    override val actorSystem: ActorSystem = if (selfShutdown) {
      ActorSystem("snapshot-failure", ConfigFactory.parseString(
        """akka.coordinated-shutdown.terminate-actor-system = on
          |akka.coordinated-shutdown.exit-jvm = off
          |""".stripMargin).withFallback(ConfigFactory.load()))
    } else ActorSystem()
    override val testProbe: TestProbe = TestProbe()(actorSystem)
    override val nodeViewHolderRef: ActorRef = injectedFailure match {
      case None => ErgoNodeViewRef(settings)(actorSystem)
      case Some((fault, injected)) => actorSystem.actorOf(Props(new ErgoNodeViewHolder[DigestState](settings) {
        override protected def genesisState = {
          val (history, initialState, wallet, pool) = super.genesisState
          initialState.closeStorage()
          val faultStore = new LDBVersionedStore(new File(settings.directory, "state"),
            initialKeepVersions = settings.nodeSettings.keepVersions) {
            override def update(versionID: Array[Byte],
                                toRemove: TraversableOnce[Array[Byte]],
                                toUpdate: TraversableOnce[(Array[Byte], Array[Byte])]): Try[Unit] = {
              if (fault == "conversion" && versionID.length == 32 &&
                  lastVersionID.exists(_.length == 33) && injected.compareAndSet(false, true)) {
                Failure(new IOException("injected Digest checkpoint conversion failure"))
              } else {
                super.update(versionID, toRemove, toUpdate).flatMap { _ =>
                  if (fault.startsWith("write-") && versionID.length == 33 &&
                      injected.compareAndSet(false, true)) {
                    Failure(new IOException("injected failure after AVL reconstruction write"))
                  } else Success(())
                }
              }
            }

            override def rollbackTo(versionID: Array[Byte]): Try[Unit] =
              if (fault == "write-rollback-failed" && injected.get()) {
                Failure(new IOException("injected reconstruction rollback failure"))
              } else super.rollbackTo(versionID)
          }
          val faultState = new DigestState(initialState.version, initialState.rootDigest, faultStore, settings) {}
          (history, faultState, wallet, pool)
        }
      }))
    }

    def stop(): Unit = {
      // Termination also completes when the system has already stopped after an actor failure.
      // A probe in that stopped system cannot receive a new Terminated acknowledgement.
      Await.result(actorSystem.terminate(), 30.seconds)
      closeOwnedStores(new File(settings.directory))
    }
  }

  Seq((StateType.Utxo, "", false), (StateType.Digest, "", false),
    (StateType.Digest, "stale-anchor", false), (StateType.Digest, "", true),
    (StateType.Digest, "conversion", false), (StateType.Digest, "write-recovered", false),
    (StateType.Digest, "write-rollback-failed", false)).foreach { case (mode, fault, legacyFormat) =>
    val terminalFailure = fault == "conversion" || fault == "write-rollback-failed"
    val testName = fault match {
      case "stale-anchor" => "ignore a stale snapshot initialization without changing state or download plan"
      case "conversion" => "failed Digest conversion shuts down after valid snapshot reconstruction without finalizing"
      case "write-recovered" => "recover the previous state after a snapshot write failure and report its plan identity"
      case "write-rollback-failed" => "shut down after snapshot reconstruction and rollback both fail"
      case _ if legacyFormat => "convert a legacy UTXO-format snapshot checkpoint on Digest reopen and retain rollback"
      case _ => s"preserve real snapshot root and version after store reopen in ${mode.stateTypeName} mode"
    }
    property(testName) {
      val root = Files.createTempDirectory("snapshot-mode-reopen-").toFile
      val nodeSettings = parsedSettings(new File(root, "node"), mode)
      // The real UTXO producer writes the legacy physical format; no old binary is executed.
      val importMode = if (legacyFormat) StateType.Utxo else mode
      val importSettings = if (legacyFormat) parsedSettings(new File(root, "node"), importMode) else nodeSettings
      val sourceSettings = parsedSettings(new File(root, "source"), StateType.Utxo)
      var session: Option[Session] = None
      var observerSystem: Option[ActorSystem] = None
      try {
        val sourceDir = new File(sourceSettings.directory, "state")
        Files.createDirectories(sourceDir.toPath)
        val (genesis, boxes) = ErgoState.generateGenesisUtxoState(sourceDir, sourceSettings, Some(parameters))
        var source = WrappedUtxoState(genesis, boxes, sourceSettings)
        var parent: Option[ErgoFullBlock] = None
        val startTime = System.currentTimeMillis() - 20000L
        val blocks = (1 to 19).map { height =>
          val block = validFullBlock(parent, source, startTime + height * 1000L)
          source = source.applyModifier(block)(_ => ()).get
          parent = Some(block)
          block
        }
        val snapshotHeader = blocks.last.header
        val successor = validFullBlock(parent, source, startTime + 20000L)
        snapshotHeader.height shouldBe 19
        Algos.encode(source.rootDigest) shouldBe Algos.encode(snapshotHeader.stateRoot)
        source.dumpSnapshot(snapshotHeader.height, source.rootDigest.dropRight(1)).get
        val manifestId = source.snapshotsDb.readSnapshotsInfo.availableManifests(snapshotHeader.height)
        val manifestBytes = source.snapshotsDb.readManifestBytes(manifestId).get
        val manifest = ManifestSerializer.defaultSerializer.parseBytes(manifestBytes)
        // Small trees can be fully embedded in the production-depth manifest. Such a fixture
        // exercises real snapshot import, but does not establish nonzero chunk-transfer coverage.
        info(s"${mode.stateTypeName} snapshot subtree count: ${manifest.subtreesIds.size}")

        val injectedWrite = new AtomicBoolean(false)
        val first = new Session(importSettings, selfShutdown = terminalFailure,
          injectedFailure = if (fault.nonEmpty && fault != "stale-anchor") Some(fault -> injectedWrite) else None)
        session = Some(first)
        blocks.foreach(block => applyHeader(block.header)(first).get)
        val history = getHistory(first)
        history.bestFullBlockOpt shouldBe None
        history.isUtxoSnapshotApplied shouldBe false
        history.registerManifestToDownload(manifest, manifestBytes, snapshotHeader.height, Seq.empty)
        history.getChunkIdsToDownload(manifest.subtreesIds.size).foreach { id =>
          history.registerDownloadedChunk(id, source.snapshotsDb.readSubtreeBytes(id).get).get
        }
        if (fault.nonEmpty) {
          val observer = ActorSystem("snapshot-failure-observer")
          observerSystem = Some(observer)
          val stateEvents = TestProbe()(observer)
          val snapshotEvents = TestProbe()(observer)
          val restorationEvents = TestProbe()(observer)
          first.actorSystem.eventStream.subscribe(stateEvents.ref, classOf[ChangedState]) shouldBe true
          first.actorSystem.eventStream.subscribe(snapshotEvents.ref, classOf[UtxoSnapshotAppliedToState]) shouldBe true
          first.actorSystem.eventStream.subscribe(restorationEvents.ref, classOf[UtxoSnapshotStateRestorationFailed]) shouldBe true
          val initialFloor = history.minimalFullBlockHeight
          val initialState = getCurrentState(first)
          val initialVersion = initialState.version
          val initialRoot = Algos.encode(initialState.rootDigest)
          val initialRows = initialState.store.getAll
            .map { case (key, value) => key.toIndexedSeq -> value.toIndexedSeq }.toSet
          val initialPlan = history.utxoSetSnapshotDownloadPlan().get
          val requestedAnchor = if (fault == "stale-anchor") Header.GenesisParentId else snapshotHeader.id
          send(InitStateFromSnapshot(snapshotHeader.height, requestedAnchor))(first)
          if (terminalFailure) {
            Await.result(first.actorSystem.whenTerminated, 30.seconds)
            injectedWrite.get() shouldBe true
            closeOwnedStores(new File(nodeSettings.directory))
            session = None
            val reconstructedStore = new LDBVersionedStore(new File(nodeSettings.directory, "state"),
              initialKeepVersions = nodeSettings.nodeSettings.keepVersions)
            try {
              reconstructedStore.lastVersionID.map(Algos.encode(_)) shouldBe Some(Algos.encode(snapshotHeader.stateRoot))
            } finally reconstructedStore.close()
            restorationEvents.expectNoMessage(300.millis)
          } else {
            val current = getCurrentState(first)
            current.version shouldBe initialVersion
            Algos.encode(current.rootDigest) shouldBe initialRoot
            current.store.getAll
              .map { case (key, value) => key.toIndexedSeq -> value.toIndexedSeq }.toSet shouldBe initialRows
            getHistory(first).utxoSetSnapshotDownloadPlan().get shouldBe initialPlan
            injectedWrite.get() shouldBe (fault == "write-recovered")
            if (fault == "write-recovered") {
              val failed = restorationEvents.expectMsgType[UtxoSnapshotStateRestorationFailed](10.seconds)
              failed.blockHeight shouldBe snapshotHeader.height
              failed.blockId shouldBe snapshotHeader.id
              failed.snapshotId shouldBe initialPlan.id
              failed.planCreatedTime shouldBe initialPlan.createdTime
            } else restorationEvents.expectNoMessage(300.millis)
            first.stop()
            session = None
          }
          stateEvents.expectNoMessage(300.millis)
          snapshotEvents.expectNoMessage(300.millis)
          val reopenedHistory = ErgoHistory.readOrGenerate(nodeSettings)(null)
          try {
            reopenedHistory.isUtxoSnapshotApplied shouldBe false
            reopenedHistory.minimalFullBlockHeight shouldBe initialFloor
            reopenedHistory.bestFullBlockOpt shouldBe None
            reopenedHistory.readUtxoSnapshotScanSource().failed.get.getMessage shouldBe
              "No persisted UTXO snapshot scan source"
          } finally reopenedHistory.closeStorage()
        } else {
        val appliedEvents = TestProbe()(first.actorSystem)
        first.actorSystem.eventStream.subscribe(appliedEvents.ref, classOf[UtxoSnapshotAppliedToState]) shouldBe true
        send(InitStateFromSnapshot(snapshotHeader.height, snapshotHeader.id))(first)
        val importedStateClass = if (importMode == StateType.Digest) classOf[DigestState] else classOf[UtxoState]
        val expectedStateClass = if (mode == StateType.Digest) classOf[DigestState] else classOf[UtxoState]
        // send and query both originate from the probe, preserving mailbox ordering.
        first.testProbe.send(first.nodeViewHolderRef,
          org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView[ErgoState[_], String](
            view => view.state.getClass.getName))
        first.testProbe.expectMsg(20.seconds, importedStateClass.getName)
        getCurrentState(first).version shouldBe idToVersion(snapshotHeader.id)
        getRootHash(first) shouldBe Algos.encode(snapshotHeader.stateRoot)
        val snapshotContext = Algos.encode(getCurrentState(first).stateContext.bytes)
        if (importMode == StateType.Digest) {
          val versions = getCurrentState(first).rollbackVersions.toSeq
          versions.map(version => versionToBytes(version).length) shouldBe Seq(32)
          versions shouldBe Seq(idToVersion(snapshotHeader.id))
        }
        getHistory(first).isUtxoSnapshotApplied shouldBe true
        getHistory(first).minimalFullBlockHeight shouldBe 20
        getHistory(first).bestFullBlockOpt shouldBe None
        val retainedSource = getHistory(first).readUtxoSnapshotScanSource(snapshotHeader.id).get
        retainedSource.snapshotHeight shouldBe snapshotHeader.height
        retainedSource.snapshotBlockId shouldBe snapshotHeader.id
        retainedSource.manifestBytes shouldBe manifestBytes
        getHistory(first).readUtxoSnapshotScanPart(retainedSource, 0).isSuccess shouldBe true
        val applied = appliedEvents.expectMsgType[UtxoSnapshotAppliedToState](10.seconds)
        applied.blockHeight shouldBe snapshotHeader.height
        applied.blockId shouldBe snapshotHeader.id
        applied.stateReader.version shouldBe idToVersion(snapshotHeader.id)
        applied.stateReader.getClass.getName shouldBe importedStateClass.getName
        applied.stateReader.isInstanceOf[UtxoStateReader] shouldBe (importMode == StateType.Utxo)
        Algos.encode(applied.stateReader.rootDigest) shouldBe Algos.encode(snapshotHeader.stateRoot)

        first.stop()
        session = None
        val reopened = new Session(nodeSettings)
        session = Some(reopened)
        val reopenedView = getCurrentView(reopened)
        val requestId = java.util.UUID.randomUUID()
        reopened.testProbe.send(reopened.nodeViewHolderRef, RequestCurrentWalletView(requestId, reopened.testProbe.ref))
        val walletView = reopened.testProbe.expectMsgType[CurrentWalletView](10.seconds)
        walletView.requestId shouldBe requestId
        val offeredSnapshot = walletView.appliedSnapshot.get
        offeredSnapshot.blockHeight shouldBe snapshotHeader.height
        offeredSnapshot.blockId shouldBe snapshotHeader.id
        offeredSnapshot.stateReader.version shouldBe idToVersion(snapshotHeader.id)
        offeredSnapshot.stateReader.getClass.getName shouldBe expectedStateClass.getName
        offeredSnapshot.stateReader.isInstanceOf[UtxoStateReader] shouldBe (mode == StateType.Utxo)
        (walletView.state eq offeredSnapshot.stateReader) shouldBe true
        Algos.encode(offeredSnapshot.stateReader.rootDigest) shouldBe Algos.encode(snapshotHeader.stateRoot)
        val requests = reopenedView.history.nextModifiersToDownload(10, (_, id) => !reopenedView.history.contains(id))
        val observation = s"mode=$mode, runtime=${reopenedView.state.getClass.getSimpleName}, " +
          s"version=${reopenedView.state.version}, root=${Algos.encode(reopenedView.state.rootDigest)}, " +
          s"applied=${reopenedView.history.isUtxoSnapshotApplied}, floor=${reopenedView.history.minimalFullBlockHeight}, " +
          s"fullBlock=${reopenedView.history.bestFullBlockOpt.map(_.id)}, requestTypes=${requests.keySet}"
        withClue(observation) {
          reopenedView.history.bestFullBlockOpt shouldBe None
          reopenedView.history.isUtxoSnapshotApplied shouldBe true
          reopenedView.history.minimalFullBlockHeight shouldBe 20
          Algos.encode(reopenedView.state.rootDigest) shouldBe Algos.encode(snapshotHeader.stateRoot)
          reopenedView.state.version shouldBe idToVersion(snapshotHeader.id)
          reopenedView.state.getClass.getName shouldBe expectedStateClass.getName
          Algos.encode(reopenedView.state.stateContext.bytes) shouldBe snapshotContext
          if (mode == StateType.Digest) {
            reopenedView.state.rollbackVersions.toSeq shouldBe Seq(idToVersion(snapshotHeader.id))
          }
          reopenedView.history.isHeadersChainSynced shouldBe false
          requests shouldBe Map.empty
        }
        // Ordinary header sync is session-local: fresh header arrival must rearm it naturally.
        applyHeader(successor.header)(reopened).get
        val rearmedHistory = getHistory(reopened)
        rearmedHistory.isHeadersChainSynced shouldBe true
        val rearmedRequests = rearmedHistory.nextModifiersToDownload(10, (_, id) => !rearmedHistory.contains(id))
        withClue(s"$observation, rearmedRequestTypes=${rearmedRequests.keySet}") {
          rearmedRequests.contains(SnapshotsInfoTypeId.value) shouldBe false
          rearmedRequests.values.flatten.toSet shouldBe successor.header.sectionIds
            .filter { case (typeId, _) => mode.requireProofs || typeId != org.ergoplatform.modifiers.history.ADProofs.modifierTypeId }
            .map(_._2).toSet
        }
        applyPayload(successor)(reopened).get
        val afterBlock = getCurrentView(reopened)
        afterBlock.state.version shouldBe idToVersion(successor.id)
        Algos.encode(afterBlock.state.rootDigest) shouldBe Algos.encode(successor.header.stateRoot)
        afterBlock.history.bestFullBlockOpt.map(_.id) shouldBe Some(successor.id)
        val successorContext = Algos.encode(afterBlock.state.stateContext.bytes)

        reopened.stop()
        session = None
        val afterBlockRestart = new Session(nodeSettings)
        session = Some(afterBlockRestart)
        val afterRestart = getCurrentView(afterBlockRestart)
        afterRestart.state.getClass.getName shouldBe expectedStateClass.getName
        afterRestart.state.version shouldBe idToVersion(successor.id)
        Algos.encode(afterRestart.state.rootDigest) shouldBe Algos.encode(successor.header.stateRoot)
        Algos.encode(afterRestart.state.stateContext.bytes) shouldBe successorContext
        afterRestart.history.bestFullBlockOpt.map(_.id) shouldBe Some(successor.id)

        if (mode == StateType.Digest) {
          afterRestart.state.rollbackVersions.foreach(version => versionToBytes(version).length shouldBe 32)
          afterBlockRestart.stop()
          session = None
          // State-only rollback control: no actor/history reconciliation is invoked after moving
          // this isolated state back to the anchor while history still records the successor.
          val persisted = ErgoState.readOrGenerate(nodeSettings).asInstanceOf[DigestState]
          val rolledBack = persisted.rollbackTo(idToVersion(snapshotHeader.id)).get
          rolledBack.version shouldBe idToVersion(snapshotHeader.id)
          Algos.encode(rolledBack.rootDigest) shouldBe Algos.encode(snapshotHeader.stateRoot)
          Algos.encode(rolledBack.stateContext.bytes) shouldBe snapshotContext
          rolledBack.closeStorage()
          closeOwnedStores(new File(nodeSettings.directory))
          val reopenedAnchor = ErgoState.readOrGenerate(nodeSettings)
          reopenedAnchor.version shouldBe idToVersion(snapshotHeader.id)
          Algos.encode(reopenedAnchor.rootDigest) shouldBe Algos.encode(snapshotHeader.stateRoot)
          Algos.encode(reopenedAnchor.stateContext.bytes) shouldBe snapshotContext
          reopenedAnchor.closeStorage()
        }
        }
      } finally {
        try session.foreach(_.stop())
        finally {
          try observerSystem.foreach(system => Await.result(system.terminate(), 30.seconds))
          finally {
            closeOwnedStores(root)
            deleteRecursive(root)
          }
        }
      }
    }
  }
}
