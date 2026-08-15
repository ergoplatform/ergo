package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorRef, ActorSystem, Props, Status}
import akka.pattern.ask
import akka.testkit.TestProbe
import akka.util.{ByteString, Timeout}
import org.ergoplatform._
import org.ergoplatform.ErgoBox.R1
import org.ergoplatform.core.{idToVersion, versionToId}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedState, UtxoSnapshotAppliedToState}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader, UtxoStateReader, VotingData}
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.ErgoWalletServiceUtils.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.nodeView.wallet.WalletScanLogic.ScanResults
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, UtxoSnapshotScanInvalidation, UtxoSnapshotScanStatus, UtxoSnapshotScanStatusSerializer, UtxoSnapshotWalletOrigin, UtxoSnapshotWalletOriginSerializer, WalletDigest, WalletDigestSerializer, WalletRegistry, WalletStorage}
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, BurnTokensRequest, ExternalSecret, PaymentRequest}
import org.ergoplatform.nodeView.wallet.scanning.{ContainsAssetPredicate, EqualsScanningPredicate, Scan, ScanRequest, ScanWalletInteraction}
import org.ergoplatform.sdk.wallet.secrets.PrimitiveSecretKey
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.serialization.ManifestSerializer
import org.ergoplatform.settings.{Algos, Parameters}
import org.ergoplatform.utils._
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.wallet.boxes.BoxSelector.MinBoxValue
import org.ergoplatform.wallet.boxes.{ChainStatus, ErgoBoxSerializer, ReplaceCompactCollectBoxSelector, TrackedBoxSerializer}
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.interpreter.{ErgoInterpreter, TransactionHintsBag}
import org.scalacheck.Gen
import org.scalatest.concurrent.Eventually
import scorex.crypto.authds.ADDigest
import scorex.db.{LDBFactory, LDBVersionedStore}
import scorex.util.ModifierId
import scorex.util.encode.Base16
import sigma.Extensions.ArrayOps
import sigma.ast.{ByteArrayConstant, ErgoTree}
import sigma.data.{CAND, CTHRESHOLD}
import sigmastate.crypto.DLogProtocol.DLogProverInput
import sigmastate.eval.Extensions._
import org.ergoplatform.settings.Constants.TrueTree

import java.io.File
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.nio.file.Files
import java.util.UUID
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong, AtomicReference}
import scala.collection.compat.immutable.ArraySeq
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class ErgoWalletSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import ErgoWalletService._
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._
  import org.ergoplatform.wallet.utils.WalletGenerators._
  import org.ergoplatform.utils.generators.ErgoCoreTransactionGenerators._

  private implicit val verifier: ErgoInterpreter = ErgoInterpreter(parameters)
  private implicit val timeout: Timeout = Timeout(5.seconds)

  private val DefaultSnapshotWalletVars = WalletVars(
    proverOpt = None,
    externalScans = Seq.empty,
    stateCacheProvided = Some(WalletCache(defaultProver.hdPubKeys, settings)))(settings)

  private val DefaultSnapshotScanDefinition = UtxoSnapshotScanDefinition.calculate(
    DefaultSnapshotWalletVars,
    settings.walletSettings.dustLimit).get

  private val ActorDefinitionBasePredicate =
    ContainsAssetPredicate(Array.tabulate(32)(_.toByte).toTokenId)
  private val ActorDefinitionOtherPredicate =
    ContainsAssetPredicate(Array.fill(32)(0x55.toByte).toTokenId)
  private val ActorDefinitionBaseScan = Scan(
    ScanId @@ 50.toShort,
    "actor-definition-base",
    ActorDefinitionBasePredicate,
    ScanWalletInteraction.Off,
    removeOffchain = true)

  private val WalletVarsExternalScanDefinition = UtxoSnapshotScanDefinition.calculate(
    DefaultSnapshotWalletVars.copy(externalScans = Seq(ActorDefinitionBaseScan)),
    settings.walletSettings.dustLimit).get

  private val AlternateSnapshotScanDefinition = UtxoSnapshotScanDefinition(
    UtxoSnapshotScanDefinition.WalletScanSemanticsVersion,
    ByteString(Array.fill(UtxoSnapshotScanDefinition.DigestLength)(0x55.toByte)))

  private def snapshotStatus(snapshotHeight: Int,
                             snapshotBlockId: ModifierId,
                             manifestDepth: Int,
                             nextSubtreeIndex: Int,
                             totalSubtrees: Int,
                             completed: Boolean): UtxoSnapshotScanStatus =
    new UtxoSnapshotScanStatus(
      snapshotHeight, snapshotBlockId, manifestDepth, nextSubtreeIndex,
      totalSubtrees, completed, DefaultSnapshotScanDefinition)

  private def snapshotStatus(snapshotHeight: Int,
                             snapshotBlockId: ModifierId,
                             manifestDepth: Int,
                             nextSubtreeIndex: Int,
                             totalSubtrees: Int,
                             completed: Boolean,
                             scanDefinition: UtxoSnapshotScanDefinition): UtxoSnapshotScanStatus =
    new UtxoSnapshotScanStatus(
      snapshotHeight, snapshotBlockId, manifestDepth, nextSubtreeIndex,
      totalSubtrees, completed, scanDefinition)

  private def snapshotOrigin(snapshotHeight: Int,
                             snapshotBlockId: ModifierId): UtxoSnapshotWalletOrigin =
    new UtxoSnapshotWalletOrigin(
      snapshotHeight, snapshotBlockId, DefaultSnapshotScanDefinition)

  private def snapshotOrigin(snapshotHeight: Int,
                             snapshotBlockId: ModifierId,
                             scanDefinition: UtxoSnapshotScanDefinition): UtxoSnapshotWalletOrigin =
    new UtxoSnapshotWalletOrigin(snapshotHeight, snapshotBlockId, scanDefinition)

  private def withProbeWalletActor[T](baseSettings: org.ergoplatform.settings.ErgoSettings,
                                      directory: File = Files.createTempDirectory("wallet-run-fence-").toFile,
                                      historyReader: ErgoHistoryReader = null,
                                       sourceIdentity: Option[ModifierId => Try[UtxoSnapshotSourceIdentity]] = None,
                                       registryTip: Option[ErgoWalletState => Try[(Int, Option[ModifierId])]] = None,
                                       bestHeaderId: Option[Int => Try[Option[ModifierId]]] = None,
                                       bestHeaderState: Option[Int => Try[Option[(ModifierId, ADDigest)]]] = None,
                                       snapshotFullHeight: Option[ErgoWalletState => Int] = None,
                                       catchUpReady: Option[Int => Boolean] = None,
                                       catchUpScan: Option[(ErgoWalletState, Int) => Try[ErgoWalletState]] = None,
                                       statusRemoval: Option[ErgoWalletState => Try[Unit]] = None,
                                       sourceRemoval: Option[ModifierId => Try[Unit]] = None,
                                       walletOriginRead: Option[(ErgoWalletState,
                                         () => Try[Option[UtxoSnapshotWalletOrigin]]) =>
                                         Try[Option[UtxoSnapshotWalletOrigin]]] = None,
                                       rescanRegistryRecreation: Option[(ErgoWalletState,
                                         () => Try[ErgoWalletState]) => Try[ErgoWalletState]] = None,
                                       snapshotChunkScan: Option[() => Unit] = None,
                                       recoveryFenceRead: Option[(ErgoWalletState,
                                         () => Try[Option[UtxoSnapshotScanInvalidation]]) =>
                                         Try[Option[UtxoSnapshotScanInvalidation]]] = None,
                                       recoveryWalletVarsUpdate: Option[(ErgoWalletState, Parameters,
                                         () => Try[WalletVars]) => Try[WalletVars]] = None,
                                       recoveryStateContextPersistence: Option[(ErgoWalletState,
                                         ErgoStateContext, () => Try[Unit]) => Try[Unit]] = None,
                                       registryRecreation: Option[(ErgoWalletState, () => RegistryResetOutcome) =>
                                         RegistryResetOutcome] = None,
                                       recoveryStorageClose: Option[(ErgoWalletState, () => Try[Unit]) =>
                                         Try[Unit]] = None,
                                       recoveryRestart: Option[(ErgoWalletState, UtxoSnapshotScanInvalidation,
                                         UtxoSnapshotScanStatus, () => Try[Boolean]) => Try[Boolean]] = None,
                                       snapshotDefinitionCalculation: Option[(ErgoWalletState,
                                         () => Try[UtxoSnapshotScanDefinition]) =>
                                         Try[UtxoSnapshotScanDefinition]] = None,
                                       actorPreStart: Option[() => Unit] = None)
                                     (test: (ActorRef, TestProbe, TestProbe) => T): T = {
    implicit val actorSystem: ActorSystem =
      ActorSystem(s"wallet-run-fence-${UUID.randomUUID().toString}")
    val scanner = TestProbe()
    val client = TestProbe()
    val isolatedSettings = baseSettings.copy(directory = directory.getAbsolutePath)
    val boxSelector = new ReplaceCompactCollectBoxSelector(
      isolatedSettings.walletSettings.maxInputs,
      isolatedSettings.walletSettings.optimalInputs,
      None)
    val walletService = new ErgoWalletServiceImpl(isolatedSettings) {
      override def scanUtxoSnapshotChunk(
        state: ErgoWalletState,
        boxes: Seq[ErgoBox],
        snapshotBlockId: ModifierId,
        snapshotHeight: Int,
        subtreeIndex: Int,
        nextSubtreeIndex: Int,
        finalChunk: Boolean,
        dustLimit: Option[Long]): Try[ErgoWalletState] = {
        snapshotChunkScan.foreach(_())
        super.scanUtxoSnapshotChunk(
          state,
          boxes,
          snapshotBlockId,
          snapshotHeight,
          subtreeIndex,
          nextSubtreeIndex,
          finalChunk,
          dustLimit)
      }

      override def recreateRegistry(
        state: ErgoWalletState,
        actorSettings: org.ergoplatform.settings.ErgoSettings): Try[ErgoWalletState] =
        rescanRegistryRecreation.fold(super.recreateRegistry(state, actorSettings)) { recreate =>
          recreate(state, () => super.recreateRegistry(state, actorSettings))
        }
    }
    val actor = actorSystem.actorOf(Props(new ErgoWalletActor(
      isolatedSettings,
      parameters,
      walletService,
      boxSelector,
      historyReader) {
      override def preStart(): Unit = {
        actorPreStart.foreach(_())
        super.preStart()
      }

      override protected[wallet] def createUtxoSnapshotScanner(): ActorRef = scanner.ref

      override protected[wallet] def readUtxoSnapshotSourceIdentity(
        expectedBlockId: ModifierId): Try[UtxoSnapshotSourceIdentity] =
        sourceIdentity.fold(super.readUtxoSnapshotSourceIdentity(expectedBlockId))(_(expectedBlockId))

      override protected[wallet] def readWalletRegistryTip(
        state: ErgoWalletState): Try[(Int, Option[ModifierId])] =
        registryTip.fold(super.readWalletRegistryTip(state))(_(state))

      override protected[wallet] def readBestHeaderIdAtHeight(
        height: Int): Try[Option[ModifierId]] =
        bestHeaderId.fold(super.readBestHeaderIdAtHeight(height))(_(height))

      override protected[wallet] def readBestHeaderStateAtHeight(
        height: Int): Try[Option[(ModifierId, ADDigest)]] =
        bestHeaderState.fold(super.readBestHeaderStateAtHeight(height))(_(height))

      override protected[wallet] def utxoSnapshotFullHeight(state: ErgoWalletState): Int =
        snapshotFullHeight.fold(super.utxoSnapshotFullHeight(state))(_(state))

      override protected[wallet] def isUtxoSnapshotCatchUpReady(height: Int): Boolean =
        catchUpReady.fold(super.isUtxoSnapshotCatchUpReady(height))(_(height))

      override protected[wallet] def scanUtxoSnapshotCatchUpHeight(
        state: ErgoWalletState,
        height: Int): Try[ErgoWalletState] =
        catchUpScan.fold(super.scanUtxoSnapshotCatchUpHeight(state, height))(_(state, height))

      override protected[wallet] def removeUtxoSnapshotScanStatus(
        state: ErgoWalletState): Try[Unit] =
        statusRemoval.fold(super.removeUtxoSnapshotScanStatus(state))(_(state))

      override protected[wallet] def removeUtxoSnapshotScanSource(
        snapshotBlockId: ModifierId): Try[Unit] =
        sourceRemoval.fold(super.removeUtxoSnapshotScanSource(snapshotBlockId))(_(snapshotBlockId))

      override protected[wallet] def readUtxoSnapshotWalletOrigin(
        state: ErgoWalletState): Try[Option[UtxoSnapshotWalletOrigin]] =
        walletOriginRead.fold(super.readUtxoSnapshotWalletOrigin(state)) { read =>
          read(state, () => super.readUtxoSnapshotWalletOrigin(state))
        }

      override protected[wallet] def calculateUtxoSnapshotScanDefinition(
        state: ErgoWalletState): Try[UtxoSnapshotScanDefinition] =
        snapshotDefinitionCalculation.fold(
          super.calculateUtxoSnapshotScanDefinition(state)) { calculate =>
          calculate(state, () => super.calculateUtxoSnapshotScanDefinition(state))
        }

      override protected[wallet] def readUtxoSnapshotRecoveryFence(
        state: ErgoWalletState): Try[Option[UtxoSnapshotScanInvalidation]] =
        recoveryFenceRead.fold(super.readUtxoSnapshotRecoveryFence(state)) { read =>
          read(state, () => super.readUtxoSnapshotRecoveryFence(state))
        }

      override protected[wallet] def updateUtxoSnapshotRecoveryWalletVars(
        state: ErgoWalletState,
        currentParameters: Parameters): Try[WalletVars] =
        recoveryWalletVarsUpdate.fold(
          super.updateUtxoSnapshotRecoveryWalletVars(state, currentParameters)) { update =>
          update(state, currentParameters,
            () => super.updateUtxoSnapshotRecoveryWalletVars(state, currentParameters))
        }

      override protected[wallet] def persistUtxoSnapshotRecoveryStateContext(
        state: ErgoWalletState,
        stateContext: ErgoStateContext): Try[Unit] =
        recoveryStateContextPersistence.fold(
          super.persistUtxoSnapshotRecoveryStateContext(state, stateContext)) { persist =>
          persist(state, stateContext,
            () => super.persistUtxoSnapshotRecoveryStateContext(state, stateContext))
        }

      override protected[wallet] def recreateRegistryForUtxoSnapshotRecovery(
        state: ErgoWalletState): RegistryResetOutcome = registryRecreation match {
        case Some(recreate) =>
          recreate(state, () => super.recreateRegistryForUtxoSnapshotRecovery(state))
        case None =>
          super.recreateRegistryForUtxoSnapshotRecovery(state)
      }

      override protected[wallet] def closeWalletStorageForUtxoSnapshotRecovery(
        state: ErgoWalletState): Try[Unit] = recoveryStorageClose match {
        case Some(close) =>
          close(state, () => super.closeWalletStorageForUtxoSnapshotRecovery(state))
        case None =>
          super.closeWalletStorageForUtxoSnapshotRecovery(state)
      }

      override protected[wallet] def restartUtxoSnapshotScanRecovery(
        state: ErgoWalletState,
        expected: UtxoSnapshotScanInvalidation,
        freshStatus: UtxoSnapshotScanStatus): Try[Boolean] = recoveryRestart match {
        case Some(restart) =>
          restart(state, expected, freshStatus,
            () => super.restartUtxoSnapshotScanRecovery(state, expected, freshStatus))
        case None =>
          super.restartUtxoSnapshotScanRecovery(state, expected, freshStatus)
      }
    }))
    client.send(actor, GetWalletStatus)
    client.expectMsgType[WalletStatus](10.seconds).initialized shouldBe true
    try {
      test(actor, scanner, client)
    } finally {
      client.watch(actor)
      client.send(actor, CloseWallet)
      client.expectTerminated(actor, 10.seconds)
      actorSystem.terminate()
    }
  }

  private def withSeededWalletStorage[T](baseSettings: org.ergoplatform.settings.ErgoSettings,
                                         directory: File)
                                        (seed: WalletStorage => T): T = {
    val isolatedSettings = baseSettings.copy(directory = directory.getAbsolutePath)
    val storage = WalletStorage.readOrCreate(isolatedSettings)
    try seed(storage)
    finally storage.close()
  }

  private def strictHistoryReader(bestFullBlockReads: AtomicInteger = new AtomicInteger(0)):
    ErgoHistoryReader = {
    Proxy.newProxyInstance(
      classOf[ErgoHistoryReader].getClassLoader,
      Array(classOf[ErgoHistoryReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = method.getName match {
          case "heightOf" => None
          case "bestFullBlockAt" =>
            bestFullBlockReads.incrementAndGet()
            None
          case "removeUtxoSnapshotScanSource" => Success(())
          case "toString" => "strict-history-reader"
          case "hashCode" => Int.box(System.identityHashCode(proxy))
          case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq args(0))
          case other => throw new UnsupportedOperationException(s"Unexpected history read: $other")
        }
      }).asInstanceOf[ErgoHistoryReader]
  }

  private def snapshotRecoveryStateReader(baseSettings: org.ergoplatform.settings.ErgoSettings,
                                          snapshotBlockId: ModifierId,
                                          stateRoot: ADDigest = startDigest,
                                          versionId: Option[ModifierId] = None,
                                          stateHeight: Int = 0,
                                          stateContextOverride: Option[ErgoStateContext] = None): UtxoStateReader = {
    val defaultStateContext = if (stateHeight == 0) {
      ErgoStateContext.empty(baseSettings.chainSettings, parameters)
    } else {
      val header = defaultHeaderGen.sample.get.copy(height = stateHeight)
      new ErgoStateContext(
        Seq(header),
        None,
        startDigest,
        parameters,
        validationSettingsNoIl,
        VotingData.empty)(baseSettings.chainSettings)
    }
    val stateContext = stateContextOverride.getOrElse(defaultStateContext)
    Proxy.newProxyInstance(
      classOf[UtxoStateReader].getClassLoader,
      Array(classOf[UtxoStateReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "stateContext" => stateContext
            case "version" => idToVersion(versionId.getOrElse(snapshotBlockId))
            case "rootDigest" => stateRoot
            case "toString" => "snapshot-recovery-state-reader"
            case "hashCode" => Int.box(System.identityHashCode(proxy))
            case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq args(0))
            case other => throw new UnsupportedOperationException(s"Unexpected UTXO state read: $other")
          }
      }).asInstanceOf[UtxoStateReader]
  }

  private final class SnapshotRecoveryPreflightCase(
    val label: String,
    val actorSettings: org.ergoplatform.settings.ErgoSettings,
    val eventHeight: Int,
    val eventBlockId: ModifierId,
    val stateReader: UtxoStateReader,
    val bestHeaderState: Try[Option[(ModifierId, ADDigest)]],
    val sourceIdentity: Try[UtxoSnapshotSourceIdentity],
    val expectedError: String)

  private final class SnapshotRecoveryStrictPreflightCase(
    val label: String,
    val recoveryFenceRead: Try[Option[UtxoSnapshotScanInvalidation]],
    val stateReader: UtxoStateReader,
    val bestHeaderState: Try[Option[(ModifierId, ADDigest)]],
    val parameterFailure: Option[Throwable],
    val contextPersistenceFailure: Option[Throwable],
    val expectedFenceReads: Int,
    val expectedParameterUpdates: Int,
    val expectedContextWrites: Int,
    val expectedError: String)

  private def overwriteWalletStorageEntry(
    baseSettings: org.ergoplatform.settings.ErgoSettings,
    directory: File,
    key: Array[Byte],
    bytes: Array[Byte]): Unit = {
    val isolatedSettings = baseSettings.copy(directory = directory.getAbsolutePath)
    val store = LDBFactory.createKvDb(WalletStorage.storageFolder(isolatedSettings).getPath)
    try store.insert(key, bytes).get
    finally store.close()
  }

  private def readWalletStorageEntry(
    baseSettings: org.ergoplatform.settings.ErgoSettings,
    directory: File,
    key: Array[Byte]): Option[Array[Byte]] = {
    val isolatedSettings = baseSettings.copy(directory = directory.getAbsolutePath)
    val store = LDBFactory.createKvDb(WalletStorage.storageFolder(isolatedSettings).getPath)
    try store.get(key).map(_.clone())
    finally store.close()
  }

  private def seedNonemptyWalletRegistry(
    baseSettings: org.ergoplatform.settings.ErgoSettings,
    directory: File,
    versionId: ModifierId): (Array[Byte], Option[ModifierId]) = {
    val isolatedSettings = baseSettings.copy(directory = directory.getAbsolutePath)
    val registry = WalletRegistry(isolatedSettings).get
    try {
      registry.updateOnBlock(
        ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty),
        versionId,
        blockHeight = 7).get
      WalletDigestSerializer.toBytes(registry.fetchDigest()) -> registry.lastVersionId
    } finally registry.close()
  }

  private def openObservedWalletRegistry(
    baseSettings: org.ergoplatform.settings.ErgoSettings,
    directory: File,
    consumed: AtomicBoolean,
    readsAfterConsumption: AtomicInteger,
    closesAfterConsumption: AtomicInteger): WalletRegistry = {
    val isolatedSettings = baseSettings.copy(directory = directory.getAbsolutePath)
    val registryFolder = WalletRegistry.registryFolder(isolatedSettings)
    registryFolder.mkdirs()
    val versionedStore = new LDBVersionedStore(
      registryFolder,
      isolatedSettings.nodeSettings.keepVersions)
    if (!versionedStore.versionIdExists(WalletRegistry.PreGenesisStateVersion)) {
      versionedStore.update(WalletRegistry.PreGenesisStateVersion, Seq.empty, Seq.empty).get
    }
    new WalletRegistry(versionedStore)(isolatedSettings.walletSettings) {
      override def fetchDigest(): WalletDigest = {
        if (consumed.get()) readsAfterConsumption.incrementAndGet()
        super.fetchDigest()
      }

      override def close(): Unit = {
        if (consumed.get()) closesAfterConsumption.incrementAndGet()
        super.close()
      }
    }
  }

  private def startProbeRun(actor: ActorRef,
                            scanner: TestProbe,
                            client: TestProbe,
                            snapshotHeight: Int,
                            snapshotBlockId: ModifierId): UtxoSnapshotScanRun = {
    client.send(actor, UtxoSnapshotAppliedToState(
      snapshotHeight, snapshotBlockId, null))
    val observedRun = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds).get.get
    val start = scanner.expectMsgType[StartUtxoSnapshotScan](5.seconds)
    start.forceRestart shouldBe false
    start.run.hasSnapshot(snapshotHeight, snapshotBlockId) shouldBe true
    observedRun shouldBe start.run
    start.run
  }

  private def stopFixtureUtxoSnapshotScanner(walletActor: ActorRef,
                                              actorSystem: ActorSystem): Unit = {
    val scanner = await(actorSystem.actorSelection(
      walletActor.path / "utxo-snapshot-wallet-scanner").resolveOne(5.seconds))
    val watcher = TestProbe()(actorSystem)
    watcher.watch(scanner)
    actorSystem.stop(scanner)
    watcher.expectTerminated(scanner, 5.seconds)
  }

  property("assets in WalletDigest are deterministic against serialization") {
    forAll(Gen.listOfN(5, assetGen)) { preAssets =>
      val assets = preAssets.map { case (id, amt) => ModifierId @@ Algos.encode(id) -> amt }
      val wd0 = WalletDigest(1, 0, assets)
      val bs = WalletDigestSerializer.toBytes(wd0)
      WalletDigestSerializer.parseBytes(bs).walletAssetBalances shouldBe wd0.walletAssetBalances
    }
  }

  property("reuse one actor-minted run for duplicate snapshot events and fence stale lifecycle messages") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(63: Byte))

    withProbeWalletActor(bootstrapSettings) { (actor, scanner, client) =>
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)

      client.send(actor, UtxoSnapshotAppliedToState(0, snapshotId, null))
      client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]].get shouldBe Some(run)
      scanner.expectNoMessage(300.millis)

      val staleRun = run.copy(token = UtxoSnapshotRunToken(UUID.randomUUID()))
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        staleRun, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 33))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].isFailure shouldBe true

      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 33))
      val status = client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      val digestBefore = client.expectMsgType[WalletDigest]

      client.send(actor, ApplyUtxoSnapshotScanBatch(
        staleRun, subtreeIndex = 0, nextSubtreeIndex = 32,
        completed = false, boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].isFailure shouldBe true
      client.send(actor, UtxoSnapshotScanTerminated(staleRun, "stale termination"))
      client.send(actor, FinalizeUtxoSnapshotScan(staleRun, status.copy(completed = true)))
      client.send(actor, UtxoSnapshotCatchUpFailed(staleRun, status, "stale catch-up"))
      client.send(actor, UtxoSnapshotCleanupFailed(staleRun, "stale cleanup"))
      client.send(actor, ContinueUtxoSnapshotCatchUp(staleRun, blockHeight = 1))

      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest] shouldBe digestBefore
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 33))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get shouldBe status
    }
  }

  property("mint unique UTXO snapshot run tokens across wallet actor incarnations") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(64: Byte))
    var firstToken: Option[UtxoSnapshotRunToken] = None

    withProbeWalletActor(bootstrapSettings) { (actor, scanner, client) =>
      firstToken = Some(startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId).token)
    }
    withProbeWalletActor(bootstrapSettings) { (actor, scanner, client) =>
      val secondToken = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId).token
      Some(secondToken) should not equal firstToken
    }
  }

  property("persist the current scan definition with a new cursor-zero status") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-definition-new-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(105: Byte))
    val definitionCalls = new AtomicInteger(0)
    var expectedStatus: Option[UtxoSnapshotScanStatus] = None

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      snapshotDefinitionCalculation = Some((_, _) => {
        definitionCalls.incrementAndGet()
        Success(DefaultSnapshotScanDefinition)
      })) { (actor, scanner, client) =>
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 33))
      val status = client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      status.nextSubtreeIndex shouldBe 0
      status.completed shouldBe false
      status.scanDefinition shouldBe DefaultSnapshotScanDefinition
      definitionCalls.get() should be > 0
      expectedStatus = Some(status)
    }

    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
      contain theSameElementsInOrderAs UtxoSnapshotScanStatusSerializer.toBytes(expectedStatus.get)
  }

  property("validate active-owned progress once on ChangedState without rereading its source") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-active-owned-definition-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(118: Byte))
    val stateReader = snapshotRecoveryStateReader(bootstrapSettings, snapshotId)
    val activeSourceReads = new AtomicInteger(0)
    val activeDefinitionCalls = new AtomicInteger(0)
    var persistedStatus: Option[UtxoSnapshotScanStatus] = None
    var statusBytesBefore = Array.emptyByteArray

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => {
        activeSourceReads.incrementAndGet()
        Failure(new IllegalStateException("active-owned progress must not reread its source"))
      }),
      snapshotDefinitionCalculation = Some((_, fallback) => {
        activeDefinitionCalls.incrementAndGet()
        fallback()
      })) { (actor, scanner, client) =>
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 33))
      val status = client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      persistedStatus = Some(status)
      statusBytesBefore = UtxoSnapshotScanStatusSerializer.toBytes(status)

      activeSourceReads.set(0)
      activeDefinitionCalls.set(0)
      client.send(actor, ChangedState(stateReader))
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None

      activeDefinitionCalls.get() shouldBe 1
      activeSourceReads.get() shouldBe 0
      scanner.expectNoMessage(300.millis)
    }

    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
      contain theSameElementsInOrderAs statusBytesBefore

    val inactiveSourceReads = new AtomicInteger(0)
    val inactiveDefinitionCalls = new AtomicInteger(0)
    val expectedSource = UtxoSnapshotSourceIdentity(
      persistedStatus.get.snapshotHeight,
      persistedStatus.get.snapshotBlockId,
      persistedStatus.get.manifestDepth,
      persistedStatus.get.totalSubtrees)
    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => {
        inactiveSourceReads.incrementAndGet()
        Success(expectedSource)
      }),
      snapshotDefinitionCalculation = Some((_, fallback) => {
        inactiveDefinitionCalls.incrementAndGet()
        fallback()
      })) { (actor, scanner, client) =>
      val start = scanner.expectMsgType[StartUtxoSnapshotScan](5.seconds)
      start.run.hasSnapshot(
        persistedStatus.get.snapshotHeight, persistedStatus.get.snapshotBlockId) shouldBe true
      inactiveDefinitionCalls.get() shouldBe 1
      inactiveSourceReads.get() shouldBe 1
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
    }

    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
      contain theSameElementsInOrderAs statusBytesBefore
  }

  property("definition drift fences GetOrInit and Apply before snapshot mutation") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-definition-operational-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(106: Byte))
    val liveDefinition = new AtomicReference[UtxoSnapshotScanDefinition](
      DefaultSnapshotScanDefinition)
    val chunkScans = new AtomicInteger(0)
    var statusBytesBefore: Array[Byte] = Array.emptyByteArray

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      snapshotChunkScan = Some(() => chunkScans.incrementAndGet()),
      snapshotDefinitionCalculation = Some((_, _) => Success(liveDefinition.get()))) {
      (actor, scanner, client) =>
        val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
        client.send(actor, GetOrInitUtxoSnapshotScanStatus(
          run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 33))
        val initialStatus = client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
        initialStatus.scanDefinition shouldBe DefaultSnapshotScanDefinition
        statusBytesBefore = UtxoSnapshotScanStatusSerializer.toBytes(initialStatus)

        client.send(actor, ReadBalances(ChainStatus.OnChain))
        val digestBefore = client.expectMsgType[WalletDigest]
        liveDefinition.set(WalletVarsExternalScanDefinition)

        client.send(actor, GetOrInitUtxoSnapshotScanStatus(
          run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 33))
        val getResult = client.expectMsgType[Try[UtxoSnapshotScanStatus]]
        client.send(actor, ApplyUtxoSnapshotScanBatch(
          run,
          subtreeIndex = 0,
          nextSubtreeIndex = UtxoSnapshotWalletScanner.SnapshotScanBatchSize,
          completed = false,
          boxes = IndexedSeq.empty))
        val applyResult = client.expectMsgType[Try[UtxoSnapshotScanStatus]]
        client.send(actor, ReadBalances(ChainStatus.OnChain))
        val digestAfter = client.expectMsgType[WalletDigest]

        getResult.isFailure shouldBe true
        getResult.failed.get.getMessage.toLowerCase should include("definition")
        applyResult.isFailure shouldBe true
        applyResult.failed.get.getMessage.toLowerCase should include("definition")
        chunkScans.get() shouldBe 0
        digestAfter shouldBe digestBefore
    }

    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
      contain theSameElementsInOrderAs statusBytesBefore
    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotWalletOriginKey) shouldBe None
  }

  property("definition drift quarantines startup before resuming persisted progress") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-definition-startup-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(107: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 0,
      totalSubtrees = 33,
      completed = false,
      scanDefinition = DefaultSnapshotScanDefinition)
    val persistedScan = withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.writeUtxoSnapshotScanStatus(status).get
      storage.addScan(ScanRequest(
        "wallet-vars-definition-drift",
        ActorDefinitionBasePredicate,
        Some(ScanWalletInteraction.Off),
        Some(true))).get
    }
    val expectedLiveDefinition = UtxoSnapshotScanDefinition.calculate(
      DefaultSnapshotWalletVars.copy(externalScans = Seq(persistedScan)),
      bootstrapSettings.walletSettings.dustLimit).get
    expectedLiveDefinition should not be status.scanDefinition
    val sourceReads = new AtomicInteger(0)
    val statusBytesBefore = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => {
        sourceReads.incrementAndGet()
        Failure(new IllegalStateException("definition drift must precede source validation"))
      })) { (actor, scanner, client) =>
      val unexpectedStart = scanner.receiveOne(500.millis)
      client.send(actor, GetWalletStatus)
      val walletStatus = client.expectMsgType[WalletStatus]

      unexpectedStart shouldBe null
      sourceReads.get() shouldBe 0
      walletStatus.error.get.toLowerCase should include("definition")
      walletStatus.error.get should include(s"live=$expectedLiveDefinition")
    }

    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
      contain theSameElementsInOrderAs statusBytesBefore
  }

  property("each result-affecting definition field fences startup independently") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))

    def definition(
      tracked: Seq[Array[Byte]] = Seq(Array[Byte](1)),
      mining: Seq[Array[Byte]] = Seq(Array[Byte](2)),
      rewardDelay: Int = 720,
      scan: Scan = ActorDefinitionBaseScan,
      dustLimit: Option[Long] = Some(1L)): UtxoSnapshotScanDefinition =
      UtxoSnapshotScanDefinition.calculate(
        tracked, mining, rewardDelay, Seq(scan), dustLimit).get

    val baseDefinition = definition()
    val variants = Seq(
      "tracked script" -> definition(tracked = Seq(Array[Byte](3))),
      "mining script" -> definition(mining = Seq(Array[Byte](3))),
      "reward-delay branch" -> definition(rewardDelay = 0),
      "scan id" -> definition(scan = ActorDefinitionBaseScan.copy(
        scanId = ScanId @@ 51.toShort)),
      "interaction" -> definition(scan = ActorDefinitionBaseScan.copy(
        walletInteraction = ScanWalletInteraction.Shared)),
      "predicate" -> definition(scan = ActorDefinitionBaseScan.copy(
        trackingRule = ActorDefinitionOtherPredicate)),
      "dust presence" -> definition(dustLimit = None),
      "dust value" -> definition(dustLimit = Some(2L)))

    variants.zipWithIndex.foreach { case ((label, changedDefinition), index) =>
      val directory = Files.createTempDirectory(
        s"wallet-snapshot-definition-field-${label.replaceAll("[^a-z0-9]+", "-")}-").toFile
      val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)((109 + index).toByte))
      val status = snapshotStatus(
        snapshotHeight = 0,
        snapshotBlockId = snapshotId,
        manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
        nextSubtreeIndex = 0,
        totalSubtrees = 33,
        completed = false,
        scanDefinition = baseDefinition)
      val source = UtxoSnapshotSourceIdentity(
        status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
      val definitionCalls = new AtomicInteger(0)
      changedDefinition should not be baseDefinition
      withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)
      val statusBytesBefore = readWalletStorageEntry(
        bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get

      withProbeWalletActor(
        bootstrapSettings,
        directory,
        strictHistoryReader(),
        sourceIdentity = Some(_ => Success(source)),
        snapshotDefinitionCalculation = Some((_, _) => {
          definitionCalls.incrementAndGet()
          Success(changedDefinition)
        })) { (actor, scanner, client) =>
        val unexpectedStart = scanner.receiveOne(300.millis)
        client.send(actor, GetWalletStatus)
        val walletStatus = client.expectMsgType[WalletStatus]

        withClue(label) {
          unexpectedStart shouldBe null
          definitionCalls.get() shouldBe 1
          walletStatus.error.get.toLowerCase should include("definition")
        }
      }

      withClue(label) {
        readWalletStorageEntry(
          bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
          contain theSameElementsInOrderAs statusBytesBefore
      }
    }
  }

  property("definition drift rejects an applied snapshot event before another start") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-definition-event-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(108: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 0,
      totalSubtrees = 33,
      completed = false,
      scanDefinition = DefaultSnapshotScanDefinition)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    val liveDefinition = new AtomicReference[UtxoSnapshotScanDefinition](
      DefaultSnapshotScanDefinition)
    val definitionCalls = new AtomicInteger(0)
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)
    val statusBytesBefore = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      snapshotDefinitionCalculation = Some((_, _) => {
        definitionCalls.incrementAndGet()
        Success(liveDefinition.get())
      })) { (actor, scanner, client) =>
      val start = scanner.expectMsgType[StartUtxoSnapshotScan](5.seconds)
      start.run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId) shouldBe true
      liveDefinition.set(WalletVarsExternalScanDefinition)

      client.send(actor, UtxoSnapshotAppliedToState(
        status.snapshotHeight, status.snapshotBlockId, null))
      val result = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)

      result.isFailure shouldBe true
      result.failed.get.getMessage.toLowerCase should include("definition")
      definitionCalls.get() should be >= 2
      val abort = scanner.expectMsgType[AbortUtxoSnapshotScan](5.seconds)
      abort.run shouldBe start.run
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get.toLowerCase should include("definition")
    }

    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
      contain theSameElementsInOrderAs statusBytesBefore
  }

  property("definition drift fences finalization retries before durable cleanup") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-definition-finalization-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(117: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 1,
      totalSubtrees = 1,
      completed = true,
      scanDefinition = DefaultSnapshotScanDefinition)
    val origin = snapshotOrigin(
      status.snapshotHeight, status.snapshotBlockId, DefaultSnapshotScanDefinition)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    val liveDefinition = new AtomicReference[UtxoSnapshotScanDefinition](
      DefaultSnapshotScanDefinition)
    val definitionCalls = new AtomicInteger(0)
    val statusRemovals = new AtomicInteger(0)
    val sourceRemovals = new AtomicInteger(0)

    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)
    overwriteWalletStorageEntry(
      bootstrapSettings,
      directory,
      WalletStorage.UtxoSnapshotWalletOriginKey,
      UtxoSnapshotWalletOriginSerializer.toBytes(origin))
    val statusBytesBefore = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get
    val originBytesBefore = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotWalletOriginKey).get

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => {
        liveDefinition.set(WalletVarsExternalScanDefinition)
        Success(source)
      }),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      statusRemoval = Some(_ => {
        statusRemovals.incrementAndGet()
        Failure(new IllegalStateException("unexpected status cleanup after definition drift"))
      }),
      sourceRemoval = Some(_ => {
        sourceRemovals.incrementAndGet()
        Success(())
      }),
      snapshotDefinitionCalculation = Some((_, _) => {
        definitionCalls.incrementAndGet()
        Success(liveDefinition.get())
      })) { (actor, scanner, client) =>
      val abort = scanner.expectMsgType[AbortUtxoSnapshotScan](5.seconds)
      abort.run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId) shouldBe true
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get.toLowerCase should include("definition")
        definitionCalls.get() should be >= 2
        statusRemovals.get() shouldBe 0
        sourceRemovals.get() shouldBe 0
      }, 5.seconds, 100.millis)
    }

    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
      contain theSameElementsInOrderAs statusBytesBefore
    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotWalletOriginKey).get should
      contain theSameElementsInOrderAs originBytesBefore
  }

  property("quarantine a preseeded UTXO snapshot invalidation before reading resumable progress") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-quarantine-marker-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(65: Byte))
    val invalidation = UtxoSnapshotScanInvalidation(0, snapshotId)
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanInvalidation(invalidation).get)

    withProbeWalletActor(bootstrapSettings, directory) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get.toLowerCase should include("quarantine")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure].cause.getMessage.toLowerCase should include("quarantine")
    }
  }

  property("quarantine an incomplete persisted scan when one immutable source field differs") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-quarantine-source-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(66: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 0,
      totalSubtrees = 33,
      completed = false,
      scanDefinition = DefaultSnapshotScanDefinition)
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)
    val mismatchedSource = UtxoSnapshotSourceIdentity(
      status.snapshotHeight,
      status.snapshotBlockId,
      status.manifestDepth,
      status.totalSubtrees + 1)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(mismatchedSource))) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get should include("immutable source")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe
        Some(UtxoSnapshotScanInvalidation(status.snapshotHeight, status.snapshotBlockId))
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
    }
  }

  property("fresh snapshot recovery installs each deferred registry and preserves the durable fence") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-recovery-reset-failure-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(83: Byte))
    val invalidation = UtxoSnapshotScanInvalidation(0, snapshotId)
    val oldStatus = snapshotStatus(
      invalidation.snapshotHeight,
      invalidation.snapshotBlockId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 7,
      totalSubtrees = 33,
      completed = false)
    val source = UtxoSnapshotSourceIdentity(
      invalidation.snapshotHeight,
      invalidation.snapshotBlockId,
      oldStatus.manifestDepth,
      oldStatus.totalSubtrees)
    val stateReader = snapshotRecoveryStateReader(bootstrapSettings, snapshotId)
    val resetFailure = new IllegalStateException("injected deferred registry reset")
    val resetCalls = new AtomicInteger(0)
    val restartCalls = new AtomicInteger(0)
    val resetInputs = scala.collection.mutable.ArrayBuffer.empty[WalletRegistry]
    val resetOutputs = scala.collection.mutable.ArrayBuffer.empty[WalletRegistry]

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.writeUtxoSnapshotScanStatus(oldStatus).get
      storage.writeUtxoSnapshotScanInvalidation(invalidation).get
    }
    val statusBytesBefore = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get
    val fenceBytesBefore = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanInvalidationKey).get

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      bestHeaderState = Some(_ => Success(Some(snapshotId -> startDigest))),
      registryRecreation = Some((state, fallback) => {
        resetCalls.incrementAndGet()
        resetInputs.synchronized(resetInputs += state.registry)
        fallback() match {
          case RegistryResetReady(fresh, _) =>
            (fresh.registry eq state.registry) shouldBe false
            fresh.registry.fetchDigest()
            resetOutputs.synchronized(resetOutputs += fresh.registry)
            RegistryResetDeferred(fresh, resetFailure)
          case other =>
            fail(s"Expected a ready registry fixture, got $other")
        }
      }),
      recoveryRestart = Some((_, _, _, _) => {
        restartCalls.incrementAndGet()
        Success(true)
      })) { (actor, scanner, client) =>
      (1 to 2).foreach { _ =>
        client.send(actor, UtxoSnapshotAppliedToState(0, snapshotId, stateReader))
        val result = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)
        (result.failed.get eq resetFailure) shouldBe true
        scanner.expectNoMessage(300.millis)
      }
      resetCalls.get() shouldBe 2
      restartCalls.get() shouldBe 0
      resetInputs.synchronized(resetInputs.size) shouldBe 2
      resetOutputs.synchronized(resetOutputs.size) shouldBe 2
      resetInputs.synchronized(resetOutputs.synchronized(
        resetInputs(1) eq resetOutputs(0))) shouldBe true
      resetOutputs.synchronized(resetOutputs(1).fetchDigest()) shouldBe WalletDigest.empty

      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get.toLowerCase should include("quarantine")
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe Some(invalidation)
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(oldStatus)
    }
    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
      contain theSameElementsInOrderAs statusBytesBefore
    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanInvalidationKey).get should
      contain theSameElementsInOrderAs fenceBytesBefore
  }

  property("fresh snapshot recovery keeps each recreated registry after restart refusal") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val outcomes = Seq[(String, Try[Boolean])](
      "failure" -> Failure(new IllegalStateException("injected recovery restart failure")),
      "false" -> Success(false))

    outcomes.zipWithIndex.foreach { case ((label, outcome), index) =>
      val directory = Files.createTempDirectory(s"wallet-snapshot-recovery-$label-").toFile
      val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)((84 + index).toByte))
      val invalidation = UtxoSnapshotScanInvalidation(0, snapshotId)
      val oldStatus = snapshotStatus(
        invalidation.snapshotHeight,
        invalidation.snapshotBlockId,
        ManifestSerializer.MainnetManifestDepth.toInt,
        nextSubtreeIndex = 9,
        totalSubtrees = 33,
        completed = false)
      val source = UtxoSnapshotSourceIdentity(
        invalidation.snapshotHeight,
        invalidation.snapshotBlockId,
        oldStatus.manifestDepth,
        oldStatus.totalSubtrees)
      val stateReader = snapshotRecoveryStateReader(bootstrapSettings, snapshotId)
      val resetInputs = scala.collection.mutable.ArrayBuffer.empty[WalletRegistry]
      val resetOutputs = scala.collection.mutable.ArrayBuffer.empty[WalletRegistry]
      val restartRegistries = scala.collection.mutable.ArrayBuffer.empty[WalletRegistry]

      withSeededWalletStorage(bootstrapSettings, directory) { storage =>
        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        storage.writeUtxoSnapshotScanInvalidation(invalidation).get
      }

      withProbeWalletActor(
        bootstrapSettings,
        directory,
        strictHistoryReader(),
        sourceIdentity = Some(_ => Success(source)),
        bestHeaderState = Some(_ => Success(Some(snapshotId -> startDigest))),
        registryRecreation = Some((state, fallback) => {
          resetInputs.synchronized(resetInputs += state.registry)
          fallback() match {
            case ready@RegistryResetReady(fresh, _) =>
              resetOutputs.synchronized(resetOutputs += fresh.registry)
              ready
            case other =>
              fail(s"Expected a ready registry fixture, got $other")
          }
        }),
        recoveryRestart = Some((state, _, _, _) => {
          restartRegistries.synchronized(restartRegistries += state.registry)
          outcome
        })) { (actor, scanner, client) =>
        (1 to 2).foreach { _ =>
          client.send(actor, UtxoSnapshotAppliedToState(0, snapshotId, stateReader))
          client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds).isFailure shouldBe true
          scanner.expectNoMessage(300.millis)
        }

        resetInputs.synchronized(resetInputs.size) shouldBe 2
        resetOutputs.synchronized(resetOutputs.size) shouldBe 2
        restartRegistries.synchronized(restartRegistries.size) shouldBe 2
        resetInputs.synchronized(resetOutputs.synchronized(
          resetInputs(1) eq resetOutputs(0))) shouldBe true
        restartRegistries.synchronized(resetOutputs.synchronized(
          restartRegistries(0) eq resetOutputs(0))) shouldBe true
        restartRegistries.synchronized(resetOutputs.synchronized(
          restartRegistries(1) eq resetOutputs(1))) shouldBe true
      }

      withSeededWalletStorage(bootstrapSettings, directory) { storage =>
        storage.readUtxoSnapshotScanInvalidationTry().get shouldBe Some(invalidation)
        storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(oldStatus)
      }
    }
  }

  property("fresh snapshot recovery installs durable cursor zero before one forced start") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-recovery-success-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(86: Byte))
    val invalidation = UtxoSnapshotScanInvalidation(0, snapshotId)
    val origin = snapshotOrigin(
      invalidation.snapshotHeight, invalidation.snapshotBlockId)
    val oldStatus = snapshotStatus(
      invalidation.snapshotHeight,
      invalidation.snapshotBlockId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 11,
      totalSubtrees = 33,
      completed = false)
    val source = UtxoSnapshotSourceIdentity(
      invalidation.snapshotHeight,
      invalidation.snapshotBlockId,
      oldStatus.manifestDepth,
      oldStatus.totalSubtrees)
    val stateReader = snapshotRecoveryStateReader(bootstrapSettings, snapshotId)
    val events = scala.collection.mutable.ArrayBuffer.empty[String]
    val headerReads = new AtomicInteger(0)
    val sourceReads = new AtomicInteger(0)
    val resetOutput = new AtomicReference[WalletRegistry](null)
    val restartState = new AtomicReference[ErgoWalletState](null)
    val restartStatus = new AtomicReference[UtxoSnapshotScanStatus](null)
    val durableFenceAtRestart =
      new AtomicReference[Option[UtxoSnapshotScanInvalidation]](None)
    val durableStatusAtRestart =
      new AtomicReference[Option[UtxoSnapshotScanStatus]](None)
    val fallbackRecovery = new IllegalStateException("injected fallback recovery evidence")
    val restartEntered = new CountDownLatch(1)
    val releaseRestart = new CountDownLatch(1)

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.writeUtxoSnapshotScanStatus(oldStatus).get
      storage.writeUtxoSnapshotScanInvalidation(invalidation).get
    }
    overwriteWalletStorageEntry(
      bootstrapSettings,
      directory,
      WalletStorage.UtxoSnapshotWalletOriginKey,
      UtxoSnapshotWalletOriginSerializer.toBytes(origin))

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => {
        sourceReads.incrementAndGet()
        Success(source)
      }),
      bestHeaderState = Some(_ => {
        headerReads.incrementAndGet()
        Success(Some(snapshotId -> startDigest))
      }),
      registryRecreation = Some((_, fallback) => {
        events.synchronized(events += "reset")
        fallback() match {
          case RegistryResetReady(state, _) =>
            resetOutput.set(state.registry)
            RegistryResetReady(state, Some(fallbackRecovery))
          case other =>
            fail(s"Expected a ready registry fixture, got $other")
        }
      }),
      recoveryRestart = Some((state, expected, freshStatus, fallback) => {
        events.synchronized(events += "restart")
        restartEntered.countDown()
        if (!releaseRestart.await(5, TimeUnit.SECONDS)) {
          throw new IllegalStateException("timed out waiting to release durable recovery restart")
        }
        expected shouldBe invalidation
        restartState.set(state)
        restartStatus.set(freshStatus)
        val restarted = fallback()
        durableFenceAtRestart.set(state.storage.readUtxoSnapshotScanInvalidationTry().get)
        durableStatusAtRestart.set(state.storage.readUtxoSnapshotScanStatusTry().get)
        restarted
      })) { (actor, scanner, client) =>
      client.send(actor, UtxoSnapshotAppliedToState(0, snapshotId, stateReader))
      restartEntered.await(5, TimeUnit.SECONDS) shouldBe true
      val prematureStartFailure = try {
        scanner.expectNoMessage(300.millis)
        None
      } catch {
        case failure: AssertionError => Some(failure)
      } finally {
        releaseRestart.countDown()
      }

      val recoveryResult = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)
      prematureStartFailure.foreach(failure => throw failure)
      val recoveredRun = recoveryResult.get.get
      val start = scanner.expectMsgType[StartUtxoSnapshotScan](5.seconds)
      events.synchronized(events += "start")

      start.run shouldBe recoveredRun
      start.forceRestart shouldBe true
      scanner.expectNoMessage(300.millis)
      client.expectNoMessage(300.millis)
      events.synchronized(events.toSeq) shouldBe Seq("reset", "restart", "start")
      headerReads.get() shouldBe 1
      sourceReads.get() shouldBe 1

      val freshStatus = restartStatus.get()
      freshStatus shouldBe oldStatus.copy(nextSubtreeIndex = 0, completed = false)
      durableFenceAtRestart.get() shouldBe None
      durableStatusAtRestart.get() shouldBe Some(freshStatus)

      val normalized = restartState.get()
      (normalized.registry eq resetOutput.get()) shouldBe true
      normalized.outputsFilter shouldBe None
      normalized.stateReaderOpt shouldBe Some(stateReader)
      normalized.utxoStateReaderOpt shouldBe Some(stateReader)
      normalized.parameters shouldBe stateReader.stateContext.currentParameters
      normalized.offChainRegistry shouldBe OffChainRegistry.init(normalized.registry)

      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None

      client.send(actor, UtxoSnapshotAppliedToState(0, snapshotId, stateReader))
      client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds).get shouldBe None
      scanner.expectNoMessage(300.millis)
      headerReads.get() shouldBe 1
      sourceReads.get() shouldBe 1
      events.synchronized(events.toSeq) shouldBe Seq("reset", "restart", "start")
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(restartStatus.get())
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(origin)
    }
  }

  property("unavailable snapshot recovery restarts the same actor without touching the consumed registry") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-recovery-unavailable-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(96: Byte))
    val invalidation = UtxoSnapshotScanInvalidation(0, snapshotId)
    val oldStatus = snapshotStatus(
      invalidation.snapshotHeight,
      invalidation.snapshotBlockId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 13,
      totalSubtrees = 33,
      completed = false)
    val source = UtxoSnapshotSourceIdentity(
      invalidation.snapshotHeight,
      invalidation.snapshotBlockId,
      oldStatus.manifestDepth,
      oldStatus.totalSubtrees)
    val stateReader = snapshotRecoveryStateReader(bootstrapSettings, snapshotId)
    val deferredCause = new IllegalStateException("injected deferred setup")
    val resetCause = new IllegalStateException("injected unavailable registry reset")
    val resetCalls = new AtomicInteger(0)
    val preStarts = new AtomicInteger(0)
    val consumed = new AtomicBoolean(false)
    val readsAfterConsumption = new AtomicInteger(0)
    val closesAfterConsumption = new AtomicInteger(0)
    val observedRegistry = new AtomicReference[WalletRegistry](null)

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.writeUtxoSnapshotScanStatus(oldStatus).get
      storage.writeUtxoSnapshotScanInvalidation(invalidation).get
    }
    val statusBytesBefore = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get
    val fenceBytesBefore = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanInvalidationKey).get

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      bestHeaderState = Some(_ => Success(Some(snapshotId -> startDigest))),
      registryRecreation = Some((state, fallback) => resetCalls.incrementAndGet() match {
        case 1 =>
          fallback() match {
            case RegistryResetReady(fresh, _) =>
              fresh.registry.close()
              val observed = openObservedWalletRegistry(
                bootstrapSettings,
                directory,
                consumed,
                readsAfterConsumption,
                closesAfterConsumption)
              observed.fetchDigest() shouldBe WalletDigest.empty
              observedRegistry.set(observed)
              RegistryResetDeferred(fresh.copy(registry = observed), deferredCause)
            case other =>
              fail(s"Expected a ready registry fixture, got $other")
          }
        case 2 =>
          (state.registry eq observedRegistry.get()) shouldBe true
          consumed.set(true)
          state.registry.close()
          RegistryResetUnavailable(resetCause)
        case count =>
          fail(s"Unexpected registry reset call $count")
      }),
      actorPreStart = Some(() => preStarts.incrementAndGet())) { (actor, scanner, client) =>
      client.send(actor, UtxoSnapshotAppliedToState(0, snapshotId, stateReader))
      val deferred = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)
      (deferred.failed.get eq deferredCause) shouldBe true
      scanner.expectNoMessage(300.millis)

      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus](5.seconds).error.get.toLowerCase should include("quarantine")

      client.watch(actor)
      client.send(actor, UtxoSnapshotAppliedToState(0, snapshotId, stateReader))
      val unavailable = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)
      (unavailable.failed.get eq resetCause) shouldBe true
      scanner.expectNoMessage(300.millis)

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        preStarts.get() shouldBe 2
      }
      client.send(actor, GetWalletStatus)
      val restartedStatus = client.expectMsgType[WalletStatus](5.seconds)
      restartedStatus.initialized shouldBe true
      restartedStatus.error.get.toLowerCase should include("quarantine")
      client.expectNoMessage(300.millis)

      resetCalls.get() shouldBe 2
      readsAfterConsumption.get() shouldBe 0
      closesAfterConsumption.get() shouldBe 1
    }

    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
      contain theSameElementsInOrderAs statusBytesBefore
    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanInvalidationKey).get should
      contain theSameElementsInOrderAs fenceBytesBefore
  }

  property("unavailable snapshot recovery suppresses a distinct storage-close failure and still restarts") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-recovery-close-failure-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(97: Byte))
    val invalidation = UtxoSnapshotScanInvalidation(0, snapshotId)
    val oldStatus = snapshotStatus(
      invalidation.snapshotHeight,
      invalidation.snapshotBlockId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 15,
      totalSubtrees = 33,
      completed = false)
    val source = UtxoSnapshotSourceIdentity(
      invalidation.snapshotHeight,
      invalidation.snapshotBlockId,
      oldStatus.manifestDepth,
      oldStatus.totalSubtrees)
    val stateReader = snapshotRecoveryStateReader(bootstrapSettings, snapshotId)
    val resetCause = new IllegalStateException("injected unavailable reset cause")
    val storageCloseFailure = new IllegalStateException("injected storage close failure")
    val closeCalls = new AtomicInteger(0)
    val preStarts = new AtomicInteger(0)

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.writeUtxoSnapshotScanStatus(oldStatus).get
      storage.writeUtxoSnapshotScanInvalidation(invalidation).get
    }
    val statusBytesBefore = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get
    val fenceBytesBefore = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanInvalidationKey).get

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      bestHeaderState = Some(_ => Success(Some(snapshotId -> startDigest))),
      registryRecreation = Some((state, _) => {
        state.registry.close()
        RegistryResetUnavailable(resetCause)
      }),
      recoveryStorageClose = Some((_, fallback) => {
        closeCalls.incrementAndGet()
        fallback().get
        Failure(storageCloseFailure)
      }),
      actorPreStart = Some(() => preStarts.incrementAndGet())) { (actor, scanner, client) =>
      client.watch(actor)
      client.send(actor, UtxoSnapshotAppliedToState(0, snapshotId, stateReader))
      val unavailable = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)
      (unavailable.failed.get eq resetCause) shouldBe true
      resetCause.getSuppressed.toSeq should contain theSameElementsInOrderAs Seq(storageCloseFailure)
      scanner.expectNoMessage(300.millis)

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        preStarts.get() shouldBe 2
      }
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus](5.seconds).error.get.toLowerCase should include("quarantine")
      client.expectNoMessage(300.millis)
      closeCalls.get() shouldBe 1
    }

    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
      contain theSameElementsInOrderAs statusBytesBefore
    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanInvalidationKey).get should
      contain theSameElementsInOrderAs fenceBytesBefore
  }

  property("fresh snapshot recovery preflight rejects every isolated evidence mismatch") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val disabledBootstrapSettings = bootstrapSettings.copy(
      nodeSettings = bootstrapSettings.nodeSettings.copy(
        utxoSettings = bootstrapSettings.nodeSettings.utxoSettings.copy(
          utxoBootstrap = false)))
    val noScanSettings = bootstrapSettings.copy(
      walletSettings = bootstrapSettings.walletSettings.copy(
        testMnemonic = None,
        testKeysQty = None))
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(87: Byte))
    val otherId = ModifierId @@ Algos.encode(Array.fill(32)(88: Byte))
    val otherRoot = ADDigest @@ Array.fill(startDigest.length)(89: Byte)
    val manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt
    val validSource = UtxoSnapshotSourceIdentity(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = manifestDepth,
      partCount = 33)
    val validReader = snapshotRecoveryStateReader(bootstrapSettings, snapshotId)
    val validHeader = Success(Some(snapshotId -> startDigest))

    def preflightCase(
      label: String,
      expectedError: String,
      actorSettings: org.ergoplatform.settings.ErgoSettings = bootstrapSettings,
      eventHeight: Int = 0,
      eventBlockId: ModifierId = snapshotId,
      stateReader: UtxoStateReader = validReader,
      bestHeaderState: Try[Option[(ModifierId, ADDigest)]] = validHeader,
      sourceIdentity: Try[UtxoSnapshotSourceIdentity] = Success(validSource)):
      SnapshotRecoveryPreflightCase =
      new SnapshotRecoveryPreflightCase(
        label,
        actorSettings,
        eventHeight,
        eventBlockId,
        stateReader,
        bestHeaderState,
        sourceIdentity,
        expectedError)

    val cases = Seq(
      preflightCase(
        "utxo bootstrap disabled",
        "utxobootstrap",
        actorSettings = disabledBootstrapSettings,
        stateReader = snapshotRecoveryStateReader(disabledBootstrapSettings, snapshotId)),
      preflightCase(
        "wallet scan variables unavailable",
        "initialized wallet scan variables",
        actorSettings = noScanSettings,
        stateReader = snapshotRecoveryStateReader(noScanSettings, snapshotId)),
      preflightCase(
        "event height mismatch",
        "durable invalidation fence",
        eventHeight = 1),
      preflightCase(
        "event block id mismatch",
        "durable invalidation fence",
        eventBlockId = otherId),
      preflightCase(
        "null state reader",
        "state reader is unavailable",
        stateReader = null),
      preflightCase(
        "state reader height mismatch",
        "state reader identity",
        stateReader = snapshotRecoveryStateReader(
          bootstrapSettings, snapshotId, stateHeight = 1)),
      preflightCase(
        "state reader version mismatch",
        "state reader identity",
        stateReader = snapshotRecoveryStateReader(
          bootstrapSettings, snapshotId, versionId = Some(otherId))),
      preflightCase(
        "state reader root unavailable",
        "state root is unavailable",
        stateReader = snapshotRecoveryStateReader(
          bootstrapSettings, snapshotId, stateRoot = null)),
      preflightCase(
        "state reader root mismatch",
        "state root does not match the best header",
        stateReader = snapshotRecoveryStateReader(
          bootstrapSettings, snapshotId, stateRoot = otherRoot)),
      preflightCase(
        "best header absent",
        "best header",
        bestHeaderState = Success(None)),
      preflightCase(
        "best header id mismatch",
        "best header identity",
        bestHeaderState = Success(Some(otherId -> startDigest))),
      preflightCase(
        "best header root mismatch",
        "state root does not match the best header",
        bestHeaderState = Success(Some(snapshotId -> otherRoot))),
      preflightCase(
        "snapshot source read failure",
        "injected source read failure",
        sourceIdentity = Failure(new IllegalStateException("injected source read failure"))),
      preflightCase(
        "snapshot source height mismatch",
        "immutable source identity",
        sourceIdentity = Success(validSource.copy(snapshotHeight = 1))),
      preflightCase(
        "snapshot source id mismatch",
        "immutable source identity",
        sourceIdentity = Success(validSource.copy(snapshotBlockId = otherId))),
      preflightCase(
        "snapshot source depth invalid",
        "invalid immutable utxo snapshot source dimensions",
        sourceIdentity = Success(validSource.copy(manifestDepth = -1))),
      preflightCase(
        "snapshot source part count invalid",
        "invalid immutable utxo snapshot source dimensions",
        sourceIdentity = Success(validSource.copy(partCount = 0))))

    cases.foreach { testCase =>
      val directory = Files.createTempDirectory(
        s"wallet-snapshot-preflight-${testCase.label.replaceAll("[^a-z0-9]+", "-")}-")
        .toFile
      val invalidation = UtxoSnapshotScanInvalidation(0, snapshotId)
      val oldStatus = snapshotStatus(
        invalidation.snapshotHeight,
        invalidation.snapshotBlockId,
        manifestDepth,
        nextSubtreeIndex = 5,
        totalSubtrees = 33,
        completed = false)
      val resetCalls = new AtomicInteger(0)
      val restartCalls = new AtomicInteger(0)

      withSeededWalletStorage(testCase.actorSettings, directory) { storage =>
        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        storage.writeUtxoSnapshotScanInvalidation(invalidation).get
      }
      val statusBytesBefore = readWalletStorageEntry(
        testCase.actorSettings,
        directory,
        WalletStorage.UtxoSnapshotScanStatusKey).get
      val fenceBytesBefore = readWalletStorageEntry(
        testCase.actorSettings,
        directory,
        WalletStorage.UtxoSnapshotScanInvalidationKey).get

      withProbeWalletActor(
        testCase.actorSettings,
        directory,
        strictHistoryReader(),
        sourceIdentity = Some(_ => testCase.sourceIdentity),
        bestHeaderState = Some(_ => testCase.bestHeaderState),
        registryRecreation = Some((_, _) => {
          resetCalls.incrementAndGet()
          RegistryResetUnavailable(new IllegalStateException(
            s"unexpected registry recreation for ${testCase.label}"))
        }),
        recoveryRestart = Some((_, _, _, _) => {
          restartCalls.incrementAndGet()
          Success(false)
        })) { (actor, scanner, client) =>
        client.send(actor, UtxoSnapshotAppliedToState(
          testCase.eventHeight,
          testCase.eventBlockId,
          testCase.stateReader))
        val result = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)
        withClue(testCase.label) {
          result.isFailure shouldBe true
          result.failed.get.getMessage.toLowerCase should include(testCase.expectedError)
          resetCalls.get() shouldBe 0
          restartCalls.get() shouldBe 0
        }
        scanner.expectNoMessage(100.millis)

        client.send(actor, GetWalletStatus)
        withClue(testCase.label) {
          client.expectMsgType[WalletStatus].error.get.toLowerCase should
            include(testCase.expectedError)
        }
      }

      withSeededWalletStorage(testCase.actorSettings, directory) { storage =>
        withClue(testCase.label) {
          storage.readUtxoSnapshotScanInvalidationTry().get shouldBe Some(invalidation)
          storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(oldStatus)
        }
      }
      withClue(testCase.label) {
        readWalletStorageEntry(
          testCase.actorSettings,
          directory,
          WalletStorage.UtxoSnapshotScanStatusKey).get should contain theSameElementsInOrderAs statusBytesBefore
        readWalletStorageEntry(
          testCase.actorSettings,
          directory,
          WalletStorage.UtxoSnapshotScanInvalidationKey).get should
          contain theSameElementsInOrderAs fenceBytesBefore
      }
    }
  }

  property("fresh snapshot recovery preflight rejects missing and corrupt fence identities") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(90: Byte))
    val manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt
    val oldStatus = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = manifestDepth,
      nextSubtreeIndex = 3,
      totalSubtrees = 33,
      completed = false)
    val validReader = snapshotRecoveryStateReader(bootstrapSettings, snapshotId)
    val validSource = UtxoSnapshotSourceIdentity(
      oldStatus.snapshotHeight,
      oldStatus.snapshotBlockId,
      oldStatus.manifestDepth,
      oldStatus.totalSubtrees)
    val corruptBytes = Array[Byte](1, 2, 3)

    Seq("missing quarantine fence" -> false, "corrupt durable fence" -> true)
      .foreach { case ((label, corruptFence)) =>
        val directory = Files.createTempDirectory(
          s"wallet-snapshot-preflight-${label.replaceAll("[^a-z0-9]+", "-")}-")
          .toFile
        val resetCalls = new AtomicInteger(0)
        val restartCalls = new AtomicInteger(0)

        if (corruptFence) {
          withSeededWalletStorage(bootstrapSettings, directory) { storage =>
            storage.writeUtxoSnapshotScanStatus(oldStatus).get
          }
          overwriteWalletStorageEntry(
            bootstrapSettings,
            directory,
            WalletStorage.UtxoSnapshotScanInvalidationKey,
            corruptBytes)
        } else {
          withSeededWalletStorage(bootstrapSettings, directory)(_ => ())
          overwriteWalletStorageEntry(
            bootstrapSettings,
            directory,
            WalletStorage.UtxoSnapshotScanStatusKey,
            corruptBytes)
        }
        val statusBytesBefore = readWalletStorageEntry(
          bootstrapSettings,
          directory,
          WalletStorage.UtxoSnapshotScanStatusKey)
        val fenceBytesBefore = readWalletStorageEntry(
          bootstrapSettings,
          directory,
          WalletStorage.UtxoSnapshotScanInvalidationKey)

        withProbeWalletActor(
          bootstrapSettings,
          directory,
          strictHistoryReader(),
          sourceIdentity = Some(_ => Success(validSource)),
          bestHeaderState = Some(_ => Success(Some(snapshotId -> startDigest))),
          registryRecreation = Some((_, _) => {
            resetCalls.incrementAndGet()
            RegistryResetUnavailable(
              new IllegalStateException(s"unexpected registry recreation for $label"))
          }),
          recoveryRestart = Some((_, _, _, _) => {
            restartCalls.incrementAndGet()
            Success(false)
          })) { (actor, scanner, client) =>
          client.send(actor, UtxoSnapshotAppliedToState(0, snapshotId, validReader))
          val result = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)
          withClue(label) {
            result.isFailure shouldBe true
            result.failed.get.getMessage.toLowerCase should include("fence identity")
            resetCalls.get() shouldBe 0
            restartCalls.get() shouldBe 0
          }
          scanner.expectNoMessage(100.millis)
          client.send(actor, GetWalletStatus)
          client.expectMsgType[WalletStatus].error.get.toLowerCase should include("fence identity")
        }

        withSeededWalletStorage(bootstrapSettings, directory) { storage =>
          if (corruptFence) {
            storage.readUtxoSnapshotScanInvalidationTry().isFailure shouldBe true
            storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(oldStatus)
          } else {
            storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
            storage.readUtxoSnapshotScanStatusTry().isFailure shouldBe true
          }
        }
        withClue(label) {
          readWalletStorageEntry(
            bootstrapSettings,
            directory,
            WalletStorage.UtxoSnapshotScanStatusKey).map(_.toSeq) shouldBe
            statusBytesBefore.map(_.toSeq)
          readWalletStorageEntry(
            bootstrapSettings,
            directory,
            WalletStorage.UtxoSnapshotScanInvalidationKey).map(_.toSeq) shouldBe
            fenceBytesBefore.map(_.toSeq)
        }
      }
  }

  property("fresh snapshot strict recovery preflight rejects late evidence and preparation failures") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(91: Byte))
    val otherId = ModifierId @@ Algos.encode(Array.fill(32)(92: Byte))
    val invalidation = UtxoSnapshotScanInvalidation(0, snapshotId)
    val manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt
    val oldStatus = snapshotStatus(
      snapshotHeight = invalidation.snapshotHeight,
      snapshotBlockId = invalidation.snapshotBlockId,
      manifestDepth = manifestDepth,
      nextSubtreeIndex = 4,
      totalSubtrees = 33,
      completed = false)
    val validReader = snapshotRecoveryStateReader(bootstrapSettings, snapshotId)
    val validHeader = Success(Some(snapshotId -> startDigest))
    val validSource = UtxoSnapshotSourceIdentity(
      invalidation.snapshotHeight,
      invalidation.snapshotBlockId,
      oldStatus.manifestDepth,
      oldStatus.totalSubtrees)

    def strictCase(
      label: String,
      expectedError: String,
      recoveryFenceRead: Try[Option[UtxoSnapshotScanInvalidation]] = Success(Some(invalidation)),
      stateReader: UtxoStateReader = validReader,
      bestHeaderState: Try[Option[(ModifierId, ADDigest)]] = validHeader,
      parameterFailure: Option[Throwable] = None,
      contextPersistenceFailure: Option[Throwable] = None,
      expectedFenceReads: Int = 0,
      expectedParameterUpdates: Int = 0,
      expectedContextWrites: Int = 0): SnapshotRecoveryStrictPreflightCase =
      new SnapshotRecoveryStrictPreflightCase(
        label,
        recoveryFenceRead,
        stateReader,
        bestHeaderState,
        parameterFailure,
        contextPersistenceFailure,
        expectedFenceReads,
        expectedParameterUpdates,
        expectedContextWrites,
        expectedError)

    val cases = Seq(
      strictCase(
        "durable fence disappears before reset",
        "fence changed before recovery",
        recoveryFenceRead = Success(None),
        expectedFenceReads = 1),
      strictCase(
        "durable fence height changes before reset",
        "fence changed before recovery",
        recoveryFenceRead = Success(Some(invalidation.copy(snapshotHeight = 1))),
        expectedFenceReads = 1),
      strictCase(
        "durable fence id changes before reset",
        "fence changed before recovery",
        recoveryFenceRead = Success(Some(invalidation.copy(snapshotBlockId = otherId))),
        expectedFenceReads = 1),
      strictCase(
        "durable fence reread fails before reset",
        "injected durable fence reread failure",
        recoveryFenceRead = Failure(
          new IllegalStateException("injected durable fence reread failure")),
        expectedFenceReads = 1),
      strictCase(
        "state reader context is null",
        "state context is unavailable",
        stateReader = snapshotRecoveryStateReader(
          bootstrapSettings,
          snapshotId,
          stateContextOverride = Some(null))),
      strictCase(
        "best header read fails",
        "injected best header read failure",
        bestHeaderState = Failure(
          new IllegalStateException("injected best header read failure"))),
      strictCase(
        "best header root is null",
        "state root does not match the best header",
        bestHeaderState = Success(Some(snapshotId -> null))),
      strictCase(
        "wallet parameter derivation fails",
        "injected wallet parameter derivation failure",
        parameterFailure = Some(
          new IllegalStateException("injected wallet parameter derivation failure")),
        expectedFenceReads = 1,
        expectedParameterUpdates = 1),
      strictCase(
        "state context persistence fails",
        "injected state context persistence failure",
        contextPersistenceFailure = Some(
          new IllegalStateException("injected state context persistence failure")),
        expectedFenceReads = 1,
        expectedParameterUpdates = 1,
        expectedContextWrites = 1))

    cases.foreach { testCase =>
      val directory = Files.createTempDirectory(
        s"wallet-snapshot-strict-preflight-${testCase.label.replaceAll("[^a-z0-9]+", "-")}-")
        .toFile
      val fenceReads = new AtomicInteger(0)
      val parameterUpdates = new AtomicInteger(0)
      val contextWrites = new AtomicInteger(0)
      val resetCalls = new AtomicInteger(0)
      val restartCalls = new AtomicInteger(0)

      withSeededWalletStorage(bootstrapSettings, directory) { storage =>
        storage.writeUtxoSnapshotScanStatus(oldStatus).get
        storage.writeUtxoSnapshotScanInvalidation(invalidation).get
      }
      val statusBytesBefore = readWalletStorageEntry(
        bootstrapSettings,
        directory,
        WalletStorage.UtxoSnapshotScanStatusKey).get
      val fenceBytesBefore = readWalletStorageEntry(
        bootstrapSettings,
        directory,
        WalletStorage.UtxoSnapshotScanInvalidationKey).get

      withProbeWalletActor(
        bootstrapSettings,
        directory,
        strictHistoryReader(),
        sourceIdentity = Some(_ => Success(validSource)),
        bestHeaderState = Some(_ => testCase.bestHeaderState),
        recoveryFenceRead = Some((_, _) => {
          fenceReads.incrementAndGet()
          testCase.recoveryFenceRead
        }),
        recoveryWalletVarsUpdate = Some((_, _, update) => {
          parameterUpdates.incrementAndGet()
          testCase.parameterFailure match {
            case Some(t) => Failure(t)
            case None => update()
          }
        }),
        recoveryStateContextPersistence = Some((_, _, persist) => {
          contextWrites.incrementAndGet()
          testCase.contextPersistenceFailure match {
            case Some(t) => Failure(t)
            case None => persist()
          }
        }),
        registryRecreation = Some((_, _) => {
          resetCalls.incrementAndGet()
          RegistryResetUnavailable(new IllegalStateException(
            s"unexpected registry recreation for ${testCase.label}"))
        }),
        recoveryRestart = Some((_, _, _, _) => {
          restartCalls.incrementAndGet()
          Success(false)
        })) { (actor, scanner, client) =>
        client.send(actor, UtxoSnapshotAppliedToState(
          invalidation.snapshotHeight,
          invalidation.snapshotBlockId,
          testCase.stateReader))
        val result = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)
        withClue(testCase.label) {
          result.isFailure shouldBe true
          result.failed.get.getMessage.toLowerCase should include(testCase.expectedError)
          fenceReads.get() shouldBe testCase.expectedFenceReads
          parameterUpdates.get() shouldBe testCase.expectedParameterUpdates
          contextWrites.get() shouldBe testCase.expectedContextWrites
          resetCalls.get() shouldBe 0
          restartCalls.get() shouldBe 0
        }
        scanner.expectNoMessage(100.millis)
        client.send(actor, GetWalletStatus)
        withClue(testCase.label) {
          client.expectMsgType[WalletStatus].error.get.toLowerCase should
            include(testCase.expectedError)
        }
      }

      withSeededWalletStorage(bootstrapSettings, directory) { storage =>
        withClue(testCase.label) {
          storage.readUtxoSnapshotScanInvalidationTry().get shouldBe Some(invalidation)
          storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(oldStatus)
        }
      }
      withClue(testCase.label) {
        readWalletStorageEntry(
          bootstrapSettings,
          directory,
          WalletStorage.UtxoSnapshotScanStatusKey).get should
          contain theSameElementsInOrderAs statusBytesBefore
        readWalletStorageEntry(
          bootstrapSettings,
          directory,
          WalletStorage.UtxoSnapshotScanInvalidationKey).get should
          contain theSameElementsInOrderAs fenceBytesBefore
      }
    }
  }

  property("reject conflicting or corrupt snapshot origin in recovery preflight before registry reset") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(101: Byte))
    val otherId = ModifierId @@ Algos.encode(Array.fill(32)(102: Byte))
    val registryVersion = ModifierId @@ Algos.encode(Array.fill(32)(103: Byte))
    val invalidation = UtxoSnapshotScanInvalidation(0, snapshotId)
    val status = snapshotStatus(
      invalidation.snapshotHeight,
      invalidation.snapshotBlockId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 7,
      totalSubtrees = 33,
      completed = false)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    val stateReader = snapshotRecoveryStateReader(bootstrapSettings, snapshotId)
    val cases = Seq(
      ("height conflict", UtxoSnapshotWalletOriginSerializer.toBytes(
        snapshotOrigin(1, snapshotId, DefaultSnapshotScanDefinition)), 1),
      ("block id conflict", UtxoSnapshotWalletOriginSerializer.toBytes(
        snapshotOrigin(0, otherId)), 1),
      ("definition conflict", UtxoSnapshotWalletOriginSerializer.toBytes(
        snapshotOrigin(0, snapshotId, AlternateSnapshotScanDefinition)), 1),
      ("corrupt bytes", Array[Byte](1, 2, 3), 0))

    cases.foreach { case (label, originBytes, expectedWalletVarsUpdates) =>
      val directory = Files.createTempDirectory(
        s"wallet-snapshot-recovery-origin-${label.replaceAll("[^a-z0-9]+", "-")}-").toFile
      val resetCalls = new AtomicInteger(0)
      val restartCalls = new AtomicInteger(0)
      val walletVarsUpdates = new AtomicInteger(0)
      val contextWrites = new AtomicInteger(0)

      withSeededWalletStorage(bootstrapSettings, directory) { storage =>
        storage.writeUtxoSnapshotScanStatus(status).get
        storage.writeUtxoSnapshotScanInvalidation(invalidation).get
      }
      overwriteWalletStorageEntry(
        bootstrapSettings, directory, WalletStorage.UtxoSnapshotWalletOriginKey, originBytes)
      val (digestBytesBefore, versionBefore) =
        seedNonemptyWalletRegistry(bootstrapSettings, directory, registryVersion)
      val statusBytesBefore = readWalletStorageEntry(
        bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get
      val fenceBytesBefore = readWalletStorageEntry(
        bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanInvalidationKey).get
      val originBytesBefore = readWalletStorageEntry(
        bootstrapSettings, directory, WalletStorage.UtxoSnapshotWalletOriginKey).get

      withProbeWalletActor(
        bootstrapSettings,
        directory,
        strictHistoryReader(),
        sourceIdentity = Some(_ => Success(source)),
        bestHeaderState = Some(_ => Success(Some(snapshotId -> startDigest))),
        recoveryWalletVarsUpdate = Some((_, _, fallback) => {
          walletVarsUpdates.incrementAndGet()
          fallback()
        }),
        recoveryStateContextPersistence = Some((_, _, _) => {
          contextWrites.incrementAndGet()
          Success(())
        }),
        registryRecreation = Some((_, _) => {
          resetCalls.incrementAndGet()
          RegistryResetUnavailable(
            new IllegalStateException(s"unexpected registry reset for $label"))
        }),
        recoveryRestart = Some((_, _, _, _) => {
          restartCalls.incrementAndGet()
          Success(false)
        })) { (actor, scanner, client) =>
        client.send(actor, UtxoSnapshotAppliedToState(
          invalidation.snapshotHeight, invalidation.snapshotBlockId, stateReader))
        val result = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)
        withClue(label) {
          result.isFailure shouldBe true
          result.failed.get.getMessage.toLowerCase should include("origin")
          walletVarsUpdates.get() shouldBe expectedWalletVarsUpdates
          contextWrites.get() shouldBe 0
          resetCalls.get() shouldBe 0
          restartCalls.get() shouldBe 0
        }
        scanner.expectNoMessage(300.millis)
      }

      val reopened = WalletRegistry(
        bootstrapSettings.copy(directory = directory.getAbsolutePath)).get
      try {
        withClue(label) {
          WalletDigestSerializer.toBytes(reopened.fetchDigest()) should
            contain theSameElementsInOrderAs digestBytesBefore
          reopened.lastVersionId shouldBe versionBefore
        }
      } finally reopened.close()
      withClue(label) {
        readWalletStorageEntry(
          bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
          contain theSameElementsInOrderAs statusBytesBefore
        readWalletStorageEntry(
          bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanInvalidationKey).get should
          contain theSameElementsInOrderAs fenceBytesBefore
        readWalletStorageEntry(
          bootstrapSettings, directory, WalletStorage.UtxoSnapshotWalletOriginKey).get should
          contain theSameElementsInOrderAs originBytesBefore
      }
    }
  }

  property("recovery definition mismatch and failure precede every state mutation") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val injectedCalculationFailure =
      new IllegalStateException("injected recovery definition calculation failure")
    val cases = Seq[(String, Try[UtxoSnapshotScanDefinition], String)](
      ("definition mismatch", Success(AlternateSnapshotScanDefinition), "definition"),
      ("definition calculation failure", Failure(injectedCalculationFailure),
        injectedCalculationFailure.getMessage))

    cases.zipWithIndex.foreach { case ((label, definitionResult, expectedError), index) =>
      val directory = Files.createTempDirectory(
        s"wallet-snapshot-recovery-definition-${label.replaceAll("[^a-z0-9]+", "-")}-").toFile
      val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)((118 + index).toByte))
      val registryVersion = ModifierId @@ Algos.encode(Array.fill(32)((120 + index).toByte))
      val invalidation = UtxoSnapshotScanInvalidation(0, snapshotId)
      val status = snapshotStatus(
        snapshotHeight = invalidation.snapshotHeight,
        snapshotBlockId = invalidation.snapshotBlockId,
        manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
        nextSubtreeIndex = 7,
        totalSubtrees = 33,
        completed = false,
        scanDefinition = DefaultSnapshotScanDefinition)
      val origin = snapshotOrigin(
        status.snapshotHeight, status.snapshotBlockId, DefaultSnapshotScanDefinition)
      val source = UtxoSnapshotSourceIdentity(
        status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
      val stateReader = snapshotRecoveryStateReader(bootstrapSettings, snapshotId)
      val events = scala.collection.mutable.ArrayBuffer.empty[String]
      val definitionCalls = new AtomicInteger(0)
      val contextWrites = new AtomicInteger(0)
      val resetCalls = new AtomicInteger(0)
      val restartCalls = new AtomicInteger(0)
      val definitionSawUpdatedWalletVars = new AtomicBoolean(false)
      val updatedWalletVars = new AtomicReference[WalletVars](null)

      withSeededWalletStorage(bootstrapSettings, directory) { storage =>
        storage.writeUtxoSnapshotScanStatus(status).get
        storage.writeUtxoSnapshotScanInvalidation(invalidation).get
      }
      overwriteWalletStorageEntry(
        bootstrapSettings,
        directory,
        WalletStorage.UtxoSnapshotWalletOriginKey,
        UtxoSnapshotWalletOriginSerializer.toBytes(origin))
      val (digestBytesBefore, versionBefore) =
        seedNonemptyWalletRegistry(bootstrapSettings, directory, registryVersion)
      val statusBytesBefore = readWalletStorageEntry(
        bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get
      val fenceBytesBefore = readWalletStorageEntry(
        bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanInvalidationKey).get
      val originBytesBefore = readWalletStorageEntry(
        bootstrapSettings, directory, WalletStorage.UtxoSnapshotWalletOriginKey).get

      withProbeWalletActor(
        bootstrapSettings,
        directory,
        strictHistoryReader(),
        sourceIdentity = Some(_ => Success(source)),
        bestHeaderState = Some(_ => Success(Some(snapshotId -> startDigest))),
        recoveryWalletVarsUpdate = Some((_, _, fallback) => fallback().map { current =>
          val updated = current.copy(externalScans = current.externalScans)
          updatedWalletVars.set(updated)
          events.synchronized(events += "vars")
          updated
        }),
        recoveryStateContextPersistence = Some((_, _, _) => {
          contextWrites.incrementAndGet()
          events.synchronized(events += "persist")
          Success(())
        }),
        registryRecreation = Some((_, fallback) => {
          resetCalls.incrementAndGet()
          events.synchronized(events += "reset")
          fallback()
        }),
        recoveryRestart = Some((_, _, _, fallback) => {
          restartCalls.incrementAndGet()
          events.synchronized(events += "restart")
          fallback()
        }),
        snapshotDefinitionCalculation = Some((state, _) => {
          definitionCalls.incrementAndGet()
          events.synchronized(events += "definition")
          val expectedWalletVars = updatedWalletVars.get()
          definitionSawUpdatedWalletVars.set(
            expectedWalletVars != null &&
              (state.walletVars.asInstanceOf[AnyRef] eq expectedWalletVars.asInstanceOf[AnyRef]))
          definitionResult
        })) { (actor, scanner, client) =>
        client.send(actor, UtxoSnapshotAppliedToState(
          invalidation.snapshotHeight, invalidation.snapshotBlockId, stateReader))
        val result = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)
        val unexpectedStart = scanner.receiveOne(300.millis)

        withClue(label) {
          result.isFailure shouldBe true
          result.failed.get.getMessage.toLowerCase should include(expectedError.toLowerCase)
          events.synchronized(events.toSeq) shouldBe Seq("vars", "definition")
          definitionCalls.get() shouldBe 1
          definitionSawUpdatedWalletVars.get() shouldBe true
          contextWrites.get() shouldBe 0
          resetCalls.get() shouldBe 0
          restartCalls.get() shouldBe 0
          unexpectedStart shouldBe null
        }
      }

      val reopened = WalletRegistry(
        bootstrapSettings.copy(directory = directory.getAbsolutePath)).get
      try {
        withClue(label) {
          WalletDigestSerializer.toBytes(reopened.fetchDigest()) should
            contain theSameElementsInOrderAs digestBytesBefore
          reopened.lastVersionId shouldBe versionBefore
        }
      } finally reopened.close()
      withClue(label) {
        readWalletStorageEntry(
          bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
          contain theSameElementsInOrderAs statusBytesBefore
        readWalletStorageEntry(
          bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanInvalidationKey).get should
          contain theSameElementsInOrderAs fenceBytesBefore
        readWalletStorageEntry(
          bootstrapSettings, directory, WalletStorage.UtxoSnapshotWalletOriginKey).get should
          contain theSameElementsInOrderAs originBytesBefore
      }
    }
  }

  property("validate a completed scan against the best header tip without requiring a full block") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(67: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 33,
      totalSubtrees = 33,
      completed = true)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    val fullBlockReads = new AtomicInteger(0)
    val cleanupEvents = scala.collection.mutable.ArrayBuffer.empty[String]
    def recordCleanup(event: String): Unit = cleanupEvents.synchronized(cleanupEvents += event)
    val acceptedDirectory = Files.createTempDirectory("wallet-snapshot-completed-header-").toFile
    withSeededWalletStorage(bootstrapSettings, acceptedDirectory)(_.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      acceptedDirectory,
      strictHistoryReader(fullBlockReads),
      sourceIdentity = Some(_ => Success(source)),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      statusRemoval = Some(state => {
        recordCleanup("status")
        state.storage.removeUtxoSnapshotScanStatus()
      }),
      sourceRemoval = Some(_ => {
        recordCleanup("source")
        Success(())
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest]
    }
    fullBlockReads.get() shouldBe 0
    cleanupEvents.synchronized(cleanupEvents.toSeq) shouldBe Seq("status", "source")

    val duplicateResumeSourceReads = new AtomicInteger(0)
    withProbeWalletActor(
      bootstrapSettings,
      acceptedDirectory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => {
        duplicateResumeSourceReads.incrementAndGet()
        scala.util.Failure(new IllegalStateException("completed status must not resume twice"))
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest]
    }
    duplicateResumeSourceReads.get() shouldBe 0

    val rejectedDirectory = Files.createTempDirectory("wallet-snapshot-completed-mismatch-").toFile
    val otherHeaderId = ModifierId @@ Algos.encode(Array.fill(32)(68: Byte))
    withSeededWalletStorage(bootstrapSettings, rejectedDirectory)(_.writeUtxoSnapshotScanStatus(status).get)
    withProbeWalletActor(
      bootstrapSettings,
      rejectedDirectory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(otherHeaderId)))) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get should include("registry tip")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }
  }

  property("retry completed status cleanup with delay and a fixed bound before touching the source") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(71: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 33,
      totalSubtrees = 33,
      completed = true)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)

    val transientDirectory = Files.createTempDirectory("wallet-snapshot-cleanup-retry-").toFile
    withSeededWalletStorage(bootstrapSettings, transientDirectory)(_.writeUtxoSnapshotScanStatus(status).get)
    val transientAttempts = new AtomicInteger(0)
    val firstAttemptAt = new AtomicLong(0L)
    val secondAttemptAt = new AtomicLong(0L)
    val transientEvents = scala.collection.mutable.ArrayBuffer.empty[String]
    def recordTransient(event: String): Unit = transientEvents.synchronized(transientEvents += event)
    withProbeWalletActor(
      bootstrapSettings,
      transientDirectory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      statusRemoval = Some(state => {
        val attempt = transientAttempts.incrementAndGet()
        recordTransient(s"status-$attempt")
        if (attempt == 1) {
          firstAttemptAt.set(System.nanoTime())
          scala.util.Failure(new IllegalStateException("injected status removal failure"))
        } else {
          secondAttemptAt.set(System.nanoTime())
          state.storage.removeUtxoSnapshotScanStatus()
        }
      }),
      sourceRemoval = Some(_ => {
        recordTransient("source")
        Success(())
      })) { (actor, _, client) =>
      client.awaitAssert({
        transientAttempts.get() shouldBe 2
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 6.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest]
    }
    transientEvents.synchronized(transientEvents.toSeq) shouldBe
      Seq("status-1", "status-2", "source")
    (secondAttemptAt.get() - firstAttemptAt.get()).nanos should be >= 500.millis

    val boundedDirectory = Files.createTempDirectory("wallet-snapshot-cleanup-bounded-").toFile
    withSeededWalletStorage(bootstrapSettings, boundedDirectory)(_.writeUtxoSnapshotScanStatus(status).get)
    val boundedAttempts = new AtomicInteger(0)
    val boundedSourceRemovals = new AtomicInteger(0)
    withProbeWalletActor(
      bootstrapSettings,
      boundedDirectory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      statusRemoval = Some(_ => {
        boundedAttempts.incrementAndGet()
        scala.util.Failure(new IllegalStateException("persistent status removal failure"))
      }),
      sourceRemoval = Some(_ => {
        boundedSourceRemovals.incrementAndGet()
        Success(())
      })) { (actor, _, client) =>
      client.awaitAssert(
        boundedAttempts.get() shouldBe ErgoWalletActor.MaxFinalizationCleanupRetries + 1,
        8.seconds,
        100.millis)
      client.expectNoMessage(ErgoWalletActor.FinalizationCleanupRetryDelay + 300.millis)
      boundedAttempts.get() shouldBe ErgoWalletActor.MaxFinalizationCleanupRetries + 1
      boundedSourceRemovals.get() shouldBe 0
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get should include("remove completed UTXO snapshot scan status")
    }
    withSeededWalletStorage(bootstrapSettings, boundedDirectory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
    }
  }

  property("recover completed finalization when the initially missing catch-up block becomes available") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-catchup-recovery-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(73: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 33,
      totalSubtrees = 33,
      completed = true)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    val catchUpAvailable = new AtomicBoolean(false)
    val catchUpScans = new AtomicInteger(0)
    val statusRemovals = new AtomicInteger(0)
    val sourceRemovals = new AtomicInteger(0)
    val stateContext = ErgoStateContext.empty(bootstrapSettings.chainSettings, parameters)
    val stateReader = Proxy.newProxyInstance(
      classOf[ErgoStateReader].getClassLoader,
      Array(classOf[ErgoStateReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "stateContext" => stateContext
            case "toString" => "snapshot-catchup-state-reader"
            case "hashCode" => Int.box(System.identityHashCode(proxy))
            case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq args(0))
            case other => throw new UnsupportedOperationException(s"Unexpected state read: $other")
          }
      }).asInstanceOf[ErgoStateReader]
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      snapshotFullHeight = Some(_ => status.snapshotHeight + 1),
      catchUpReady = Some(_ => catchUpAvailable.get()),
      catchUpScan = Some((state, _) => {
        catchUpScans.incrementAndGet()
        if (catchUpAvailable.get()) Success(state)
        else Failure(new IllegalStateException("catch-up block unavailable"))
      }),
      statusRemoval = Some(state => {
        statusRemovals.incrementAndGet()
        state.storage.removeUtxoSnapshotScanStatus()
      }),
      sourceRemoval = Some(_ => {
        sourceRemovals.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get should include("unavailable")
      }, 5.seconds, 100.millis)
      catchUpScans.get() shouldBe 0
      statusRemovals.get() shouldBe 0
      sourceRemovals.get() shouldBe 0

      catchUpAvailable.set(true)
      client.send(actor, ChangedState(stateReader))
      client.send(actor, ChangedState(stateReader))

      client.awaitAssert({
        catchUpScans.get() shouldBe 1
        statusRemovals.get() shouldBe 1
        sourceRemovals.get() shouldBe 1
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest]
      scanner.expectNoMessage(300.millis)
    }
    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
    }
  }

  property("keep the wallet usable when source cleanup fails after completed status removal") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-source-cleanup-failure-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(72: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 33,
      totalSubtrees = 33,
      completed = true)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      sourceRemoval = Some(_ =>
        scala.util.Failure(new IllegalStateException("injected source cleanup failure")))) {
      (actor, scanner, client) =>
        scanner.expectNoMessage(300.millis)
        client.awaitAssert({
          client.send(actor, GetWalletStatus)
          client.expectMsgType[WalletStatus].error.get should include("remove completed UTXO snapshot scan source")
        }, 5.seconds, 100.millis)
        client.send(actor, ReadBalances(ChainStatus.OnChain))
        client.expectMsgType[WalletDigest]
    }
    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
    }
  }

  property("persist an invalidation before abort and reject stale batches without changing durable wallet bytes") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-rollback-quarantine-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(69: Byte))
    var expectedStatusBytes = Array.emptyByteArray
    var expectedDigestBytes = Array.emptyByteArray

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader()) { (actor, scanner, client) =>
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 33))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 32, completed = false, boxes = IndexedSeq.empty))
      val applied = client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      expectedStatusBytes = UtxoSnapshotScanStatusSerializer.toBytes(applied)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      expectedDigestBytes = WalletDigestSerializer.toBytes(client.expectMsgType[WalletDigest])

      client.send(actor, Rollback(idToVersion(ModifierId @@ Algos.encode(Array.fill(32)(70: Byte)))))
      scanner.expectMsg(AbortUtxoSnapshotScan(run))
      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 32, nextSubtreeIndex = 33, completed = true, boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].isFailure shouldBe true
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get.toLowerCase should include("quarantine")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe
        Some(UtxoSnapshotScanInvalidation(0, snapshotId))
      val persisted = storage.readUtxoSnapshotScanStatusTry().get.get
      UtxoSnapshotScanStatusSerializer.toBytes(persisted) should contain theSameElementsInOrderAs expectedStatusBytes
    }
    val reopenedRegistry = WalletRegistry(bootstrapSettings.copy(directory = directory.getAbsolutePath)).get
    try {
      WalletDigestSerializer.toBytes(reopenedRegistry.fetchDigest()) should
        contain theSameElementsInOrderAs expectedDigestBytes
    } finally reopenedRegistry.close()
  }

  property("do not use inputs spent in off-chain transaction") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      val genesisTx = genesisBlock.transactions.head
      applyBlock(genesisBlock) shouldBe 'success //scan by wallet happens during apply
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val tx =
        eventually {
          val snap = getConfirmedBalances
          // prepare a lot of inputs
          val inputsToCreate = 50
          val sumToSpend = (snap.walletBalance - MinBoxValue) / (inputsToCreate + 1)
          val req = (0 until inputsToCreate).map(_ => PaymentRequest(addresses.head, sumToSpend, Array.empty, Map.empty))
          log.info(s"Confirmed balance $snap")
          log.info(s"Payment request $req")
          val tx = await(wallet.generateTransaction(req)).get
          log.info(s"Generated transaction $tx")
          val context = new ErgoStateContext(Seq(genesisBlock.header), Some(genesisBlock.extension), startDigest, parameters, validationSettingsNoIl, VotingData.empty)(settings.chainSettings)
          val boxesToSpend = tx.inputs.map(i => genesisTx.outputs.find(o => java.util.Arrays.equals(o.id, i.boxId)).get)
          tx.statefulValidity(boxesToSpend, emptyDataBoxes, context) shouldBe 'success
          val block = makeNextBlock(getUtxoState, Seq(tx))
          applyBlock(block) shouldBe 'success //scan by wallet happens during apply
          tx
        }
      val (req2, tx2) =
        eventually {
          // generate transaction spending part of inputs
          val newSumToSpend = tx.outputs.head.value
          val req2 = Seq(PaymentRequest(addresses.head, newSumToSpend, Array.empty, Map.empty))
          log.info(s"Payment requests 2 $req2")
          val tx2 = await(wallet.generateTransaction(req2)).get
          (req2, tx2)
        }
      log.info(s"Generated transaction $tx2")
      wallet.scanOffchain(tx2)

      eventually {
        tx2.inputs.size should be < tx.outputs.size
        // trying to create a new transaction
        val tx3 = await(wallet.generateTransaction(req2)).get
        // check that tx3 has inputs different from tx2
        tx3.inputs.foreach { in =>
          tx2.inputs.exists(tx2In => tx2In.boxId sameElements in.boxId) shouldBe false
        }
      }
    }
  }

  property("skip on-chain block scan while UTXO snapshot scan is pending") {
    val bootstrapSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)
      )
    )
    new WalletFixture(bootstrapSettings, parameters, getCurrentView(_).vault).apply { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey)
      val initialBoxes = boxesAvailable(genesisBlock, address.pubkey)
      val initialBalance = balanceAmount(initialBoxes)
      val run = await(wallet.walletActor ? UtxoSnapshotAppliedToState(
        genesisBlock.height, genesisBlock.id, getUtxoState))
        .asInstanceOf[Try[Option[UtxoSnapshotScanRun]]].get.get
      stopFixtureUtxoSnapshotScanner(wallet.walletActor, w.actorSystem)

      await(wallet.walletActor ? GetOrInitUtxoSnapshotScanStatus(
        run,
        ManifestSerializer.MainnetManifestDepth.toInt,
        totalSubtrees = 33
      )) shouldBe a[Success[_]]
      await(wallet.walletActor ? ApplyUtxoSnapshotScanBatch(
        run,
        subtreeIndex = 0,
        nextSubtreeIndex = 32,
        completed = false,
        boxes = initialBoxes.toIndexedSeq
      )) shouldBe a[Success[_]]

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        getConfirmedBalances.walletBalance shouldBe initialBalance
      }

      applyBlock(genesisBlock) shouldBe 'success

      val returnBalance = initialBalance / 2
      val spendingTx = makeSpendingTx(initialBoxes, address, returnBalance)
      val nextBlock = makeNextBlock(getUtxoState, Seq(spendingTx))

      wallet.scanPersistent(nextBlock)
      Thread.sleep(500)
      getConfirmedBalances.walletBalance shouldBe initialBalance

      await(wallet.walletActor ? ApplyUtxoSnapshotScanBatch(
        run,
        subtreeIndex = 32,
        nextSubtreeIndex = 33,
        completed = true,
        boxes = IndexedSeq.empty
      )) shouldBe a[Success[_]]

      wallet.scanPersistent(nextBlock)
      eventually {
        getConfirmedBalances.walletBalance shouldBe returnBalance
      }
    }
  }

  property("freeze direct box-tracking mutations while UTXO snapshot scan is unresolved") {
    val bootstrapSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    new WalletFixture(bootstrapSettings, parameters, getCurrentView(_).vault).apply { implicit w =>
      val address = getPublicKeys.head
      val snapshot = makeGenesisBlock(address.pubkey)
      val snapshotBoxes = boxesAvailable(snapshot, address.pubkey)
      val boxToStopTracking = snapshotBoxes.head
      val blockedAddBox = boxesAvailable(makeGenesisBlock(address.pubkey, randomNewAsset), address.pubkey).head
      val run = await(wallet.walletActor ? UtxoSnapshotAppliedToState(
        snapshot.height, snapshot.id, getUtxoState))
        .asInstanceOf[Try[Option[UtxoSnapshotScanRun]]].get.get

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      def trackedState: (Seq[String], WalletDigest) = (
        await(wallet.walletBoxes(unspentOnly = false, considerUnconfirmed = false))
          .map(box => Base16.encode(TrackedBoxSerializer.toBytes(box.trackedBox))),
        getConfirmedBalances)

      await(wallet.walletActor ? GetOrInitUtxoSnapshotScanStatus(
        run,
        ManifestSerializer.MainnetManifestDepth.toInt,
        totalSubtrees = 33)) shouldBe a[Success[_]]
      await(wallet.walletActor ? ApplyUtxoSnapshotScanBatch(
        run,
        subtreeIndex = 0,
        nextSubtreeIndex = 32,
        completed = false,
        boxes = snapshotBoxes.toIndexedSeq)) shouldBe a[Success[_]]
      eventually {
        getConfirmedBalances.walletBalance shouldBe balanceAmount(snapshotBoxes)
      }

      val beforeAdd = trackedState
      val addResponse = await(wallet.addBox(blockedAddBox, Set(org.ergoplatform.wallet.Constants.PaymentsScanId)))
      val afterAdd = trackedState

      val beforeStopTracking = trackedState
      val stopTrackingResponse = await(wallet.stopTracking(
        org.ergoplatform.wallet.Constants.PaymentsScanId,
        boxToStopTracking.id))
      val afterStopTracking = trackedState

      addResponse.status.isFailure shouldBe true
      afterAdd shouldBe beforeAdd
      stopTrackingResponse.status.isFailure shouldBe true
      afterStopTracking shouldBe beforeStopTracking
    }
  }

  property("enforce UTXO snapshot identity cursor completion and replay invariants") {
    val bootstrapSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    new WalletFixture(bootstrapSettings, parameters, getCurrentView(_).vault).apply { implicit w =>
      val address = getPublicKeys.head
      val snapshot = makeGenesisBlock(address.pubkey)
      val trackedBox = boxesAvailable(snapshot, address.pubkey).head
      val otherBlockId = ModifierId @@ Algos.encode(Array.fill(32)(91: Byte))
      val run = await(wallet.walletActor ? UtxoSnapshotAppliedToState(
        snapshot.height, snapshot.id, getUtxoState))
        .asInstanceOf[Try[Option[UtxoSnapshotScanRun]]].get.get
      stopFixtureUtxoSnapshotScanner(wallet.walletActor, w.actorSystem)
      val otherRun = run.copy(snapshotBlockId = otherBlockId)
      val activeStatus = snapshotStatus(
        run.snapshotHeight,
        run.snapshotBlockId,
        ManifestSerializer.MainnetManifestDepth.toInt,
        nextSubtreeIndex = 0,
        totalSubtrees = 33,
        completed = false)
      ErgoWalletActor.statusBelongsToActiveRun(run, activeStatus) shouldBe true
      ErgoWalletActor.statusBelongsToActiveRun(run,
        activeStatus.copy(snapshotBlockId = otherBlockId)) shouldBe false
      val completedStatus = activeStatus.copy(
        nextSubtreeIndex = activeStatus.totalSubtrees,
        completed = true)
      ErgoWalletActor.shouldResumeCompletedActiveRun(
        run, completedStatus, finalizingRun = None) shouldBe true
      ErgoWalletActor.shouldResumeCompletedActiveRun(
        run, completedStatus, finalizingRun = Some(run)) shouldBe false
      ErgoWalletActor.shouldResumeCompletedActiveRun(
        run, activeStatus, finalizingRun = None) shouldBe false
      ErgoWalletActor.shouldResumeCompletedActiveRun(
        run, completedStatus.copy(snapshotBlockId = otherBlockId), finalizingRun = None) shouldBe false
      applyBlock(snapshot) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)

      def askTry(message: Any): Try[UtxoSnapshotScanStatus] =
        await(wallet.walletActor ? message).asInstanceOf[Try[UtxoSnapshotScanStatus]]

      askTry(GetOrInitUtxoSnapshotScanStatus(
        run,
        ManifestSerializer.MainnetManifestDepth.toInt,
        totalSubtrees = 33
      )).isSuccess shouldBe true

      askTry(GetOrInitUtxoSnapshotScanStatus(
        otherRun,
        ManifestSerializer.MainnetManifestDepth.toInt,
        totalSubtrees = 33
      )).isFailure shouldBe true

      askTry(ApplyUtxoSnapshotScanBatch(
        run, 0, 0, completed = false, IndexedSeq.empty
      )).isFailure shouldBe true
      askTry(ApplyUtxoSnapshotScanBatch(
        run, 0, 34, completed = true, IndexedSeq.empty
      )).isFailure shouldBe true
      askTry(ApplyUtxoSnapshotScanBatch(
        run, 0, 32, completed = true, IndexedSeq.empty
      )).isFailure shouldBe true
      askTry(ApplyUtxoSnapshotScanBatch(
        run, 0, 33, completed = false, IndexedSeq.empty
      )).isFailure shouldBe true

      val first = ApplyUtxoSnapshotScanBatch(
        run, 0, 32, completed = false, IndexedSeq(trackedBox))
      askTry(first).get.nextSubtreeIndex shouldBe 32
      askTry(first).get.nextSubtreeIndex shouldBe 32

      val spendingTx = eventually {
        await(wallet.generateTransaction(Seq(
          PaymentRequest(address, trackedBox.value / 2, Array.empty, Map.empty)))).get
      }

      val divergentReplay = ApplyUtxoSnapshotScanBatch(
        run, 0, 32, completed = false, IndexedSeq.empty)
      askTry(divergentReplay).isFailure shouldBe true

      val keysBefore = await(wallet.publicKeys(0, Int.MaxValue))
      val scansBefore = await(wallet.readScans()).apps
      await(wallet.walletActor ? DeriveKey("m/1")).asInstanceOf[Try[_]].isFailure shouldBe true
      await(wallet.walletActor ? DeriveNextKey)
        .asInstanceOf[DeriveNextKeyResult].result.isFailure shouldBe true
      val scanRequest = ScanRequest(
        "snapshot-definition-mutation",
        EqualsScanningPredicate(R1, ByteArrayConstant(address.script.bytes)),
        Some(ScanWalletInteraction.Off),
        Some(false))
      await(wallet.walletActor ? AddScan(scanRequest))
        .asInstanceOf[AddScanResponse].response.isFailure shouldBe true
      await(wallet.walletActor ? RemoveScan(org.ergoplatform.wallet.Constants.ScanId @@ 11.toShort))
        .asInstanceOf[RemoveScanResponse].response.isFailure shouldBe true
      await(wallet.publicKeys(0, Int.MaxValue)) shouldBe keysBefore
      await(wallet.readScans()).apps shouldBe scansBefore

      val digestBeforeRollback = getConfirmedBalances
      w.testProbe.send(wallet.walletActor, Rollback(idToVersion(snapshot.id)))
      w.testProbe.send(wallet.walletActor, GetOrInitUtxoSnapshotScanStatus(
        run,
        ManifestSerializer.MainnetManifestDepth.toInt,
        totalSubtrees = 33))
      w.testProbe.expectMsgType[Try[UtxoSnapshotScanStatus]].get.nextSubtreeIndex shouldBe 32
      getConfirmedBalances shouldBe digestBeforeRollback

      val nextBlock = makeNextBlock(getUtxoState, Seq(spendingTx))
      applyBlock(nextBlock) shouldBe 'success

      askTry(ApplyUtxoSnapshotScanBatch(
        run, 32, 33, completed = true, IndexedSeq.empty
      )).get.completed shouldBe true
      eventually {
        getConfirmedBalances.height shouldBe nextBlock.height
      }

    }
  }

  property("replay a logically identical multi-scan snapshot batch after registry reopen") {
    val directory = Files.createTempDirectory("wallet-snapshot-multi-scan-replay-").toFile
    val isolatedSettings = settings.copy(directory = directory.getAbsolutePath)
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(105: Byte))
    val scanIds: Seq[ScanId] = Seq(
      org.ergoplatform.wallet.Constants.PaymentsScanId,
      ScanId @@ 50.toShort,
      ScanId @@ 51.toShort)
    val initialScans = scanIds.foldLeft(Set.empty[ScanId])(_ + _)
    val replayScans = scanIds.reverse.foldLeft(Set.empty[ScanId])(_ + _)
    initialScans shouldBe replayScans
    initialScans.toSeq should not equal replayScans.toSeq

    val trackedBox = trackedBoxGen.sample.get.copy(
      inclusionHeightOpt = Some(0),
      spendingTxIdOpt = None,
      spendingHeightOpt = None,
      scans = initialScans)
    val initialResults = ScanResults(
      ArraySeq(trackedBox), ArraySeq.empty, ArraySeq.empty)
    val initialRegistry = WalletRegistry(isolatedSettings).get
    try {
      initialRegistry.updateOnSnapshotChunk(
        initialResults,
        snapshotId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        nextSubtreeIndex = 32,
        finalChunk = false).get
    } finally initialRegistry.close()

    val reopened = WalletRegistry(isolatedSettings).get
    try {
      val persisted = reopened.getBox(trackedBox.box.id).get
      val replayBox = persisted.copy(scans = replayScans)
      persisted.scans shouldBe replayBox.scans
      java.util.Arrays.equals(
        TrackedBoxSerializer.toBytes(persisted),
        TrackedBoxSerializer.toBytes(replayBox)) shouldBe false
      val digestBeforeReplay = WalletDigestSerializer.toBytes(reopened.fetchDigest())
      val versionBeforeReplay = reopened.lastVersionId

      reopened.updateOnSnapshotChunk(
        ScanResults(ArraySeq(replayBox), ArraySeq.empty, ArraySeq.empty),
        snapshotId,
        snapshotHeight = 100,
        subtreeIndex = 0,
        nextSubtreeIndex = 32,
        finalChunk = false).get

      WalletDigestSerializer.toBytes(reopened.fetchDigest()) should
        contain theSameElementsInOrderAs digestBeforeReplay
      reopened.lastVersionId shouldBe versionBeforeReplay
    } finally reopened.close()
  }

  property("persist exact completed snapshot origin before replying to final Apply") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-origin-final-apply-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(81: Byte))
    val expectedOrigin = snapshotOrigin(0, snapshotId)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceRemoval = Some(_ => Success(()))) { (actor, scanner, client) =>
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get.completed shouldBe false

      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 1, completed = true, boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get.completed shouldBe true

      client.send(actor, RescanWallet(0))
      client.expectMsgType[Try[Unit]].isFailure shouldBe true
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(expectedOrigin)
    }
  }

  property("retain completed snapshot origin after status cleanup and close plus reopen") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-origin-cleanup-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(82: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 1,
      totalSubtrees = 1,
      completed = true)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    val sourceRemovals = new AtomicInteger(0)
    withSeededWalletStorage(bootstrapSettings, directory)(_.completeUtxoSnapshotScan(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      sourceRemoval = Some(_ => {
        sourceRemovals.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        sourceRemovals.get() shouldBe 1
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe
        Some(snapshotOrigin(status.snapshotHeight, status.snapshotBlockId))
    }
  }

  property("allow a post-cleanup-scan-definition change with persisted scan while refusing rescan") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-origin-scan-change-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(106: Byte))
    val completed = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 1,
      totalSubtrees = 1,
      completed = true)
    val expectedOrigin = snapshotOrigin(completed.snapshotHeight, completed.snapshotBlockId)
    val source = UtxoSnapshotSourceIdentity(
      completed.snapshotHeight,
      completed.snapshotBlockId,
      completed.manifestDepth,
      completed.totalSubtrees)
    val sourceRemovals = new AtomicInteger(0)
    withSeededWalletStorage(bootstrapSettings, directory)(_.completeUtxoSnapshotScan(completed).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      registryTip = Some(_ => Success(completed.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      sourceRemoval = Some(_ => {
        sourceRemovals.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        sourceRemovals.get() shouldBe 1
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
    }

    val originBytesAfterCleanup = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotWalletOriginKey).get
    originBytesAfterCleanup.head shouldBe 2.toByte
    originBytesAfterCleanup should contain theSameElementsInOrderAs
      UtxoSnapshotWalletOriginSerializer.toBytes(expectedOrigin)
    val persistedScan = withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(expectedOrigin)
      storage.addScan(ScanRequest(
        "post-snapshot-definition-change",
        ActorDefinitionOtherPredicate,
        Some(ScanWalletInteraction.Off),
        Some(false))).get
    }
    val changedDefinition = UtxoSnapshotScanDefinition.calculate(
      DefaultSnapshotWalletVars.copy(externalScans = Seq(persistedScan)),
      bootstrapSettings.walletSettings.dustLimit).get
    changedDefinition should not be expectedOrigin.scanDefinition
    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey) shouldBe None
    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotWalletOriginKey).get should
      contain theSameElementsInOrderAs originBytesAfterCleanup

    val sourceReads = new AtomicInteger(0)
    val registryRecreations = new AtomicInteger(0)
    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => {
        sourceReads.incrementAndGet()
        Failure(new IllegalStateException("origin-only wallet must not read snapshot source"))
      }),
      rescanRegistryRecreation = Some((_, _) => {
        registryRecreations.incrementAndGet()
        Failure(new IllegalStateException("snapshot origin must refuse registry recreation"))
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      sourceReads.get() shouldBe 0
      client.send(actor, ReadScans)
      client.expectMsgType[ReadScansResponse].apps shouldBe Seq(persistedScan)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None

      client.send(actor, UtxoSnapshotAppliedToState(completed.snapshotHeight, snapshotId, null))
      client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]].get shouldBe None
      sourceReads.get() shouldBe 0
      scanner.expectNoMessage(300.millis)

      client.send(actor, RescanWallet(0))
      val rescan = client.expectMsgType[Try[Unit]]
      rescan.failed.get.getMessage.toLowerCase should include("completed from utxo snapshot")
      registryRecreations.get() shouldBe 0
      scanner.expectNoMessage(300.millis)
    }

    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey) shouldBe None
    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotWalletOriginKey).get should
      contain theSameElementsInOrderAs originBytesAfterCleanup
  }

  property("backfill a missing completed snapshot origin before cleanup") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-origin-backfill-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(83: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 1,
      totalSubtrees = 1,
      completed = true)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      sourceRemoval = Some(_ => Success(()))) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe
        Some(snapshotOrigin(status.snapshotHeight, status.snapshotBlockId))
    }
  }

  property("finalize completed snapshot origin across utxoBootstrap configuration drift") {
    val driftedSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(104: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 1,
      totalSubtrees = 1,
      completed = true)
    val origin = snapshotOrigin(status.snapshotHeight, status.snapshotBlockId)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)

    Seq("same origin" -> true, "missing origin" -> false).foreach { case (label, seedOrigin) =>
      val directory = Files.createTempDirectory(
        s"wallet-snapshot-completed-config-drift-${label.replaceAll("[^a-z0-9]+", "-")}-").toFile
      val sourceRemovals = new AtomicInteger(0)
      withSeededWalletStorage(driftedSettings, directory) { storage =>
        if (seedOrigin) storage.completeUtxoSnapshotScan(status).get
        else storage.writeUtxoSnapshotScanStatus(status).get
      }

      withProbeWalletActor(
        driftedSettings,
        directory,
        strictHistoryReader(),
        sourceIdentity = Some(_ => Success(source)),
        registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
        bestHeaderId = Some(_ => Success(Some(snapshotId))),
        sourceRemoval = Some(_ => {
          sourceRemovals.incrementAndGet()
          Success(())
        })) { (actor, scanner, client) =>
        scanner.expectNoMessage(300.millis)
        client.awaitAssert({
          withClue(label) {
            sourceRemovals.get() shouldBe 1
            client.send(actor, GetWalletStatus)
            client.expectMsgType[WalletStatus].error shouldBe None
          }
        }, 5.seconds, 100.millis)
        scanner.expectNoMessage(300.millis)
      }

      withSeededWalletStorage(driftedSettings, directory) { storage =>
        withClue(label) {
          storage.readUtxoSnapshotScanStatusTry().get shouldBe None
          storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
          storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(origin)
        }
      }
    }
  }

  property("reject rescan from completed snapshot origin before changing a nonempty registry") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-snapshot-origin-rescan-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(84: Byte))
    val registryVersion = ModifierId @@ Algos.encode(Array.fill(32)(85: Byte))
    val completed = snapshotStatus(0, snapshotId, 6, 1, 1, completed = true)
    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.completeUtxoSnapshotScan(completed).get
      storage.removeUtxoSnapshotScanStatus().get
    }
    val (digestBytesBefore, versionBefore) =
      seedNonemptyWalletRegistry(ordinarySettings, directory, registryVersion)

    withProbeWalletActor(ordinarySettings, directory, strictHistoryReader()) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, RescanWallet(0))
      client.expectMsgType[Try[Unit]].isFailure shouldBe true
    }

    val reopened = WalletRegistry(ordinarySettings.copy(directory = directory.getAbsolutePath)).get
    try {
      WalletDigestSerializer.toBytes(reopened.fetchDigest()) should
        contain theSameElementsInOrderAs digestBytesBefore
      reopened.lastVersionId shouldBe versionBefore
    } finally reopened.close()
  }

  property("reject operational rescan when snapshot origin becomes corrupt after startup") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-snapshot-origin-corrupt-").toFile
    val registryVersion = ModifierId @@ Algos.encode(Array.fill(32)(86: Byte))
    val corruptAfterStartup = new AtomicBoolean(false)
    val originReads = new AtomicInteger(0)
    val recreateCalls = new AtomicInteger(0)
    val (digestBytesBefore, versionBefore) =
      seedNonemptyWalletRegistry(ordinarySettings, directory, registryVersion)

    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(),
      walletOriginRead = Some((_, fallback) => {
        originReads.incrementAndGet()
        if (corruptAfterStartup.get()) {
          Failure(new IllegalStateException("corrupt snapshot origin injected after startup"))
        } else {
          fallback()
        }
      }),
      rescanRegistryRecreation = Some((_, _) => {
        recreateCalls.incrementAndGet()
        Failure(new IllegalStateException("unexpected registry recreation after origin corruption"))
      })) { (actor, scanner, client) =>
      originReads.get() should be > 0
      scanner.expectNoMessage(300.millis)
      corruptAfterStartup.set(true)
      client.send(actor, RescanWallet(0))
      val result = client.expectMsgType[Try[Unit]]
      result.failed.get.getMessage.toLowerCase should include("origin")
      recreateCalls.get() shouldBe 0
      scanner.expectNoMessage(300.millis)
    }

    val reopened = WalletRegistry(ordinarySettings.copy(directory = directory.getAbsolutePath)).get
    try {
      WalletDigestSerializer.toBytes(reopened.fetchDigest()) should
        contain theSameElementsInOrderAs digestBytesBefore
      reopened.lastVersionId shouldBe versionBefore
    } finally reopened.close()
  }

  property("retain allowed rescan behavior for a genuinely no-origin wallet") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-genuine-no-origin-").toFile

    withProbeWalletActor(ordinarySettings, directory, strictHistoryReader()) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, RescanWallet(0))
      client.expectMsgType[Try[Unit]].isSuccess shouldBe true
    }
  }

  property("prevent duplicate snapshot start when completed origin has no status") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-origin-no-status-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(87: Byte))
    val completed = snapshotStatus(0, snapshotId, 6, 1, 1, completed = true)
    val definitionCalls = new AtomicInteger(0)
    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.completeUtxoSnapshotScan(completed).get
      storage.removeUtxoSnapshotScanStatus().get
    }

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      snapshotDefinitionCalculation = Some((_, _) => {
        definitionCalls.incrementAndGet()
        Failure(new IllegalStateException(
          "origin-only wallet must not calculate a live snapshot definition"))
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      definitionCalls.get() shouldBe 0
      client.send(actor, UtxoSnapshotAppliedToState(0, snapshotId, null))
      client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]].get shouldBe None
      definitionCalls.get() shouldBe 0
      scanner.expectNoMessage(300.millis)
    }
  }

  property("quarantine a snapshot status that conflicts with completed origin") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-origin-conflict-").toFile
    val statusId = ModifierId @@ Algos.encode(Array.fill(32)(88: Byte))
    val originId = ModifierId @@ Algos.encode(Array.fill(32)(89: Byte))
    val status = snapshotStatus(0, statusId, 6, 0, 1, completed = false)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)
    overwriteWalletStorageEntry(
      bootstrapSettings,
      directory,
      WalletStorage.UtxoSnapshotWalletOriginKey,
      UtxoSnapshotWalletOriginSerializer.toBytes(snapshotOrigin(0, originId)))

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      sourceIdentity = Some(_ => Success(source))) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get.toLowerCase should include("origin")
    }
  }

  property("refuse UTXO snapshot rescan on a pruned bootstrap node before registry destruction") {
    val bootstrapSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        blocksToKeep = 0,
        utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)
      )
    )
    new WalletFixture(bootstrapSettings, parameters, getCurrentView(_).vault).apply { implicit w =>
      val address = getPublicKeys.head
      val snapshot = makeGenesisBlock(address.pubkey)
      val initialBoxes = boxesAvailable(snapshot, address.pubkey)
      val run = await(wallet.walletActor ? UtxoSnapshotAppliedToState(
        snapshot.height, snapshot.id, getUtxoState))
        .asInstanceOf[Try[Option[UtxoSnapshotScanRun]]].get.get

      await(wallet.walletActor ? GetOrInitUtxoSnapshotScanStatus(
        run,
        ManifestSerializer.MainnetManifestDepth.toInt,
        totalSubtrees = 1)) shouldBe a[Success[_]]
      await(wallet.walletActor ? ApplyUtxoSnapshotScanBatch(
        run,
        subtreeIndex = 0,
        nextSubtreeIndex = 1,
        completed = true,
        boxes = initialBoxes.toIndexedSeq)) shouldBe a[Success[_]]

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      val before = eventually {
        val digest = getConfirmedBalances
        digest.walletBalance shouldBe balanceAmount(initialBoxes)
        digest
      }

      await(wallet.rescanWallet(0)).isFailure shouldBe true
      getConfirmedBalances shouldBe before
    }
  }

  property("hold wallet mutations while a snapshot scan is starting or terminally failed") {
    val bootstrapSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        blocksToKeep = 0,
        utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)
      )
    )
    new WalletFixture(bootstrapSettings, parameters, getCurrentView(_).vault).apply { implicit w =>
      val state = getUtxoState
      val snapshotId = versionToId(state.version)

      val run = await(wallet.walletActor ?
        UtxoSnapshotAppliedToState(state.stateContext.currentHeight, snapshotId, state))
        .asInstanceOf[Try[Option[UtxoSnapshotScanRun]]].get.get
      w.testProbe.send(wallet.walletActor, RescanWallet(0))
      w.testProbe.expectMsgType[Try[Unit]].isFailure shouldBe true

      w.testProbe.send(wallet.walletActor,
        UtxoSnapshotScanTerminated(run, "terminal fixture"))
      w.testProbe.send(wallet.walletActor, DeriveKey("m/1"))
      w.testProbe.expectMsgType[Try[_]].isFailure shouldBe true

      w.testProbe.send(wallet.walletActor,
        InitWallet(SecretString.create("blocked-init"), None))
      w.testProbe.expectMsgType[Try[_]].isFailure shouldBe true
      w.testProbe.send(wallet.walletActor, RestoreWallet(
        SecretString.create("blocked mnemonic"),
        None,
        SecretString.create("blocked-restore"),
        usePre1627KeyDerivation = false))
      w.testProbe.expectMsgType[Try[_]].isFailure shouldBe true
    }
  }

  property("surface durable snapshot progress when bootstrap is disabled and reject the previous run") {
    val directory = Files.createTempDirectory("wallet-durable-run-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(77: Byte))
    val enabled = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    var previousRun: Option[UtxoSnapshotScanRun] = None

    withProbeWalletActor(enabled, directory) { (actor, scanner, client) =>
      previousRun = Some(startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId))
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        previousRun.get, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 2))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get.completed shouldBe false
    }

    val disabled = enabled.copy(nodeSettings = enabled.nodeSettings.copy(
      utxoSettings = enabled.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val source = UtxoSnapshotSourceIdentity(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      partCount = 2)
    withProbeWalletActor(
      disabled,
      directory,
      sourceIdentity = Some(_ => Success(source))) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get should include("utxoBootstrap is disabled")
      }, 5.seconds, 100.millis)

      client.send(actor, RescanWallet(0))
      client.expectMsgType[Try[Unit]].isFailure shouldBe true
      client.send(actor, ApplyUtxoSnapshotScanBatch(
        previousRun.get, subtreeIndex = 0, nextSubtreeIndex = 2,
        completed = true, boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].isFailure shouldBe true
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        previousRun.get, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 2))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].isFailure shouldBe true
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure].cause.getMessage should include("utxoBootstrap is disabled")
    }

    withSeededWalletStorage(disabled, directory) { storage =>
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe
        Some(UtxoSnapshotScanInvalidation(0, snapshotId))
    }
  }

  property("Generate asset issuing transaction") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey)
      val genesisTx = genesisBlock.transactions.head
      applyBlock(genesisBlock) shouldBe 'success //scan by wallet happens during apply
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 100.millis)
      eventually {
        val availableAmount = getConfirmedBalances.walletBalance
        val emissionAmount: Int = 100000000
        val tokenName: String = "ERG"
        val tokenDescription: String = s"ERG description"
        val tokenDecimals: Int = 9
        val feeAmount = availableAmount / 4
        val feeReq = PaymentRequest(Pay2SAddress(TrueTree), feeAmount, Array.empty, Map.empty)
        val req = AssetIssueRequest(address, None, emissionAmount, tokenName, tokenDescription, tokenDecimals)
        val tx = await(wallet.generateTransaction(Seq(feeReq, req))).get
        log.info(s"Generated transaction $tx")
        val context = new ErgoStateContext(
          Seq(genesisBlock.header),
          Some(genesisBlock.extension),
          startDigest,
          parameters,
          validationSettingsNoIl,
          VotingData.empty)(settings.chainSettings)
        val boxesToSpend = tx.inputs.map(i => genesisTx.outputs.find(o => java.util.Arrays.equals(o.id, i.boxId)).get)
        tx.statefulValidity(boxesToSpend, emptyDataBoxes, context) shouldBe 'success
      }
    }
  }

  property("Generate transaction with user-defined input") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      val initialBoxes = boxesAvailable(genesisBlock, pubKey)

      val boxesToUseEncoded = initialBoxes.map { box =>
        Base16.encode(ErgoBoxSerializer.toBytes(box))
      }

      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

        //pay out all the wallet balance:
        val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toArray
        assetToSpend should not be empty
        val req1 = PaymentRequest(Pay2SAddress(TrueTree), confirmedBalance, assetToSpend, Map.empty)

        val tx1 = await(wallet.generateTransaction(Seq(req1), boxesToUseEncoded)).get
        tx1.outputs.size shouldBe 1
        tx1.outputs.head.value shouldBe confirmedBalance
        toAssetMap(tx1.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)

        //change == 1:
        val assetToSpend2 = assetToSpend.map { case (tokenId, tokenValue) => (tokenId, tokenValue - 1) }
        val assetToReturn = assetToSpend.map { case (tokenId, _) => (tokenId, 1L) }
        val req2 = PaymentRequest(Pay2SAddress(TrueTree), confirmedBalance - MinBoxValue, assetToSpend2, Map.empty)

        val tx2 = await(wallet.generateTransaction(Seq(req2))).get
        tx2.outputs.size shouldBe 2
        tx2.outputs.head.value shouldBe confirmedBalance - MinBoxValue
        toAssetMap(tx2.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend2)
        tx2.outputs(1).value shouldBe MinBoxValue
        toAssetMap(tx2.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(assetToReturn)
      }
    }
  }

  property("Generate transaction with BurnTokensRequest") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      val initialBoxes = boxesAvailable(genesisBlock, pubKey)

      val boxesToUseEncoded = initialBoxes.map { box =>
        Base16.encode(ErgoBoxSerializer.toBytes(box))
      }

      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

        //pay out all the wallet balance:
        val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toArray
        assetToSpend should not be empty
        val req1 = PaymentRequest(Pay2SAddress(TrueTree), confirmedBalance, assetToSpend, Map.empty)

        val tx1 = await(wallet.generateTransaction(Seq(req1), boxesToUseEncoded)).get
        tx1.outputs.size shouldBe 1
        tx1.outputs.head.value shouldBe confirmedBalance
        toAssetMap(tx1.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)

        //change == 1:
        val assetToSpend2 = assetToSpend.map { case (tokenId, tokenValue) => (tokenId, tokenValue - 1) }
        val assetToReturn = assetToSpend.map { case (tokenId, _) => (tokenId, 1L) }
        val req2 = Seq(BurnTokensRequest(assetToSpend2))

        val tx2 = await(wallet.generateTransaction(req2)).get
        tx2.outputs.size shouldBe 1
        tx2.outputs.head.value shouldBe confirmedBalance
        toAssetMap(tx2.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToReturn)
      }
    }
  }

  property("Generate transaction with PaymentRequest (no tokens) and BurnTokensRequest") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      val initialBoxes = boxesAvailable(genesisBlock, pubKey)

      val boxesToUseEncoded = initialBoxes.map { box =>
        Base16.encode(ErgoBoxSerializer.toBytes(box))
      }

      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance
        log.error(s"Confirmed balance $confirmedBalance")
        //pay out all the wallet balance:
        val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toArray
        assetToSpend should not be empty
        val req1 = PaymentRequest(Pay2SAddress(TrueTree), confirmedBalance, assetToSpend, Map.empty)

        val tx1 = await(wallet.generateTransaction(Seq(req1), boxesToUseEncoded)).get
        tx1.outputs.size shouldBe 1
        tx1.outputs.head.value shouldBe confirmedBalance
        toAssetMap(tx1.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)

        //change == 1:
        val assetToSpend2 = assetToSpend.map{ case (tokenId, tokenValue) => (tokenId, tokenValue - 1) }
        val assetToReturn = assetToSpend.map { case (tokenId, _) => (tokenId, 1L) }
        val req2 = Seq(BurnTokensRequest(assetToSpend2), PaymentRequest(Pay2SAddress(TrueTree), confirmedBalance - MinBoxValue, Array.empty, Map.empty))

        val tx2 = await(wallet.generateTransaction(req2)).get
        tx2.outputs.size shouldBe 2
        tx2.outputs.head.value shouldBe confirmedBalance - MinBoxValue
        toAssetMap(tx2.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(Seq.empty)
        tx2.outputs(1).value shouldBe MinBoxValue
        toAssetMap(tx2.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(assetToReturn)
      }
    }
  }

  property("whitelist set, preserve tokens from auto-burn") {
    val inputs = {
      val x = IndexedSeq(new Input(genesisEmissionBox.id, emptyProverResult))
      Seq(encodedTokenId(x.head.boxId.toTokenId))
    }

    implicit val ww: WalletFixture = new WalletFixture(settings
      .copy(walletSettings = settings
        .walletSettings.copy(tokensWhitelist = Some(inputs))), parameters, getCurrentView(_).vault)

    val pubKey = getPublicKeys.head.pubkey
    val genesisBlock = makeNextBlock(getUtxoState, Seq(makeGenesisTxWithAsset(pubKey, issueAsset = true)))
    val initialBoxes = boxesAvailable(genesisBlock, pubKey)
    val assetR = assetsByTokenId(initialBoxes).toSeq
    Some(assetR.map(x => encodedTokenId(x._1))) shouldBe ww.settings.walletSettings.tokensWhitelist

    val boxesToUseEncoded = initialBoxes.map { box =>
      Base16.encode(ErgoBoxSerializer.toBytes(box))
    }

    applyBlock(genesisBlock) shouldBe 'success
    implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
    eventually {
      val confirmedBalance = getConfirmedBalances.walletBalance
      log.error(s"Confirmed balance $confirmedBalance")
      //pay out all the wallet balance:
      val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toSeq
      Some(assetToSpend.map(x => encodedTokenId(x._1))) shouldBe ww.settings.walletSettings.tokensWhitelist
      assetToSpend should not be empty

      val req1 = PaymentRequest(Pay2SAddress(TrueTree), confirmedBalance / 2, Array.empty, Map.empty)

      val tx1 = await(wallet.generateTransaction(Seq(req1), boxesToUseEncoded)).get
      tx1.outputs.size shouldBe 2
      tx1.outputs.head.value shouldBe (confirmedBalance / 2)
      tx1.outputs.head.additionalTokens.toArray shouldBe Seq.empty
      toAssetMap(tx1.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)
    }
  }

  property("whitelist empty, auto-burn tokens on arbitrary tx") {
    implicit val ww: WalletFixture = new WalletFixture(settings
      .copy(walletSettings = settings
        .walletSettings.copy(tokensWhitelist = Some(Seq.empty))), parameters, getCurrentView(_).vault)

    val pubKey = getPublicKeys.head.pubkey
    val genesisBlock = makeNextBlock(getUtxoState, Seq(makeGenesisTxWithAsset(pubKey, issueAsset = true)))
    val initialBoxes = boxesAvailable(genesisBlock, pubKey)

    val boxesToUseEncoded = initialBoxes.map { box =>
      Base16.encode(ErgoBoxSerializer.toBytes(box))
    }

    applyBlock(genesisBlock) shouldBe 'success
    implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
    eventually {
      val confirmedBalance = getConfirmedBalances.walletBalance
      log.error(s"Confirmed balance $confirmedBalance")
      //pay out all the wallet balance:
      val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toSeq
      assetToSpend should not be empty

      val req1 = PaymentRequest(Pay2SAddress(TrueTree), confirmedBalance / 2, Array.empty, Map.empty)

      val tx1 = await(wallet.generateTransaction(Seq(req1), boxesToUseEncoded)).get
      tx1.outputs.size shouldBe 2
      tx1.outputs.head.value shouldBe (confirmedBalance / 2)
      tx1.outputs.head.additionalTokens.toArray shouldBe Seq.empty
      toAssetMap(tx1.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(Seq.empty)
    }
  }

  property("whitelist not set, ignore auto-burn") {
    implicit val ww: WalletFixture = new WalletFixture(settings
      .copy(walletSettings = settings
        .walletSettings.copy(tokensWhitelist = None)), parameters, getCurrentView(_).vault)

    val pubKey = getPublicKeys.head.pubkey
    val genesisBlock = makeNextBlock(getUtxoState, Seq(makeGenesisTxWithAsset(pubKey, issueAsset = true)))
    val initialBoxes = boxesAvailable(genesisBlock, pubKey)

    val boxesToUseEncoded = initialBoxes.map { box =>
      Base16.encode(ErgoBoxSerializer.toBytes(box))
    }

    applyBlock(genesisBlock) shouldBe 'success
    implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
    eventually {
      val confirmedBalance = getConfirmedBalances.walletBalance
      log.error(s"Confirmed balance $confirmedBalance")
      //pay out all the wallet balance:
      val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toSeq
      assetToSpend should not be empty

      val req1 = PaymentRequest(Pay2SAddress(TrueTree), confirmedBalance / 2, Array.empty, Map.empty)

      val tx1 = await(wallet.generateTransaction(Seq(req1), boxesToUseEncoded)).get
      tx1.outputs.size shouldBe 2
      tx1.outputs.head.value shouldBe (confirmedBalance / 2)
      tx1.outputs.head.additionalTokens.toArray shouldBe Seq.empty
      toAssetMap(tx1.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)
    }
  }

  property("Generate transaction with multiple inputs") {
    withFixture { implicit w =>
      val addresses = getPublicKeys
      val pubkey = addresses.head.pubkey
      addresses.length should be > 0
      val genesisBlock = makeGenesisBlock(pubkey, randomNewAsset)
      val genesisTx = genesisBlock.transactions.head
      val initialBoxes = boxesAvailable(genesisTx, pubkey)
      applyBlock(genesisBlock) shouldBe 'success //scan by wallet happens during apply
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val (tx, block, assetsToSpend) =
        eventually {
          val snap = getConfirmedBalances
          val assetsToSpend = assetsByTokenId(initialBoxes).toArray
          assetsToSpend should not be empty

          val sumToSpend = snap.walletBalance / (addresses.length + 1)
          val req =
            PaymentRequest(addresses.head, sumToSpend, assetsToSpend, Map.empty) +:
              addresses.tail.map(a => PaymentRequest(a, sumToSpend, Array.empty, Map.empty))
          log.info(s"Confirmed balance $snap")
          log.info(s"Payment request $req")
          val tx = await(wallet.generateTransaction(req)).get
          log.info(s"Generated transaction $tx")
          val context = new ErgoStateContext(
            Seq(genesisBlock.header),
            Some(genesisBlock.extension),
            startDigest,
            parameters,
            validationSettingsNoIl,
            VotingData.empty)(settings.chainSettings)
          val boxesToSpend = tx.inputs.map(i => genesisTx.outputs.find(o => java.util.Arrays.equals(o.id, i.boxId)).get)
          tx.statefulValidity(boxesToSpend, emptyDataBoxes, context) shouldBe 'success

          val block = makeNextBlock(getUtxoState, Seq(tx))
          applyBlock(block) shouldBe 'success //scan by wallet happens during apply
          (tx, block, assetsToSpend)
        }
      eventually {
        val newSnap = getConfirmedBalances
        val newSumToSpend = newSnap.walletBalance / addresses.length
        val req2 = PaymentRequest(addresses.head, newSumToSpend, assetsToSpend, Map.empty) +:
          addresses.tail.map(a => PaymentRequest(a, newSumToSpend, Array.empty, Map.empty))
        log.info(s"New balance $newSnap")
        log.info(s"Payment requests 2 $req2")
        val tx2 = await(wallet.generateTransaction(req2)).get
        log.info(s"Generated transaction $tx2")
        val context2 = new ErgoStateContext(Seq(block.header), Some(block.extension), startDigest, parameters, validationSettingsNoIl, VotingData.empty)(settings.chainSettings)
        val knownBoxes = tx.outputs ++ genesisTx.outputs
        val boxesToSpend2 = tx2.inputs.map(i => knownBoxes.find(o => java.util.Arrays.equals(o.id, i.boxId)).get)
        tx2.statefulValidity(boxesToSpend2, emptyDataBoxes, context2) shouldBe 'success
      }
    }
  }

  property("off-chain scan") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.script

      val bs0 = getBalancesWithUnconfirmed
      bs0.walletBalance shouldBe 0
      bs0.walletAssetBalances shouldBe empty

      val balance1 = settings.walletSettings.dustLimit.getOrElse(1000000L) + 1
      val box1 = IndexedSeq(new ErgoBoxCandidate(balance1, pubKey, startHeight, randomNewAsset.toColl))
      wallet.scanOffchain(ErgoTransaction(fakeInputs, box1))

      implicit val patienceConfig: PatienceConfig = PatienceConfig(1.second, 100.millis)
      eventually {
        val bs1 = getBalancesWithUnconfirmed
        bs1.walletBalance shouldBe balance1
        bs1.walletAssetBalances shouldBe assetAmount(box1)
      }

      val balance2 = settings.walletSettings.dustLimit.getOrElse(1000000L) + 1
      val box2 = IndexedSeq(new ErgoBoxCandidate(balance2, pubKey, startHeight, randomNewAsset.toColl))
      wallet.scanOffchain(ErgoTransaction(fakeInputs, IndexedSeq(), box2))

      eventually {
        val bs2 = getBalancesWithUnconfirmed
        bs2.walletBalance shouldBe (balance1 + balance2)
        bs2.walletAssetBalances shouldBe assetAmount(box1 ++ box2)
      }
    }
  }

  property("off-chain box spending") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val tx = makeGenesisTx(address.pubkey, randomNewAsset)
      wallet.scanOffchain(tx)
      val boxesToSpend = boxesAvailable(tx, address.pubkey)
      val balanceToSpend = balanceAmount(boxesToSpend)
      log.info(s"Balance to spent: $balanceToSpend")
      implicit val patienceConfig: PatienceConfig = PatienceConfig(offchainScanTime(tx).millis, 100.millis)
      val (spendingTx, balanceToReturn, assetsAfterSpending) =
        eventually {
          val totalBalance = getBalancesWithUnconfirmed.walletBalance
          totalBalance shouldEqual balanceToSpend
          log.info(s"Total balance with unconfirmed: $totalBalance")
          val balanceToReturn = randomLong(balanceToSpend)
          val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsWithRandom(boxesToSpend))
          val assetsAfterSpending = assetAmount(boxesAvailable(spendingTx, address.pubkey))
          assetsAfterSpending should not be empty
          (spendingTx, balanceToReturn, assetsAfterSpending)
        }
      wallet.scanOffchain(spendingTx)
      eventually {
        val totalAfterSpending = getBalancesWithUnconfirmed

        log.info(s"Balance to return back: $balanceToReturn")
        totalAfterSpending.walletBalance shouldEqual balanceToReturn
        totalAfterSpending.walletAssetBalances shouldEqual assetsAfterSpending
      }
    }
  }

  property("off-chain double registration") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val tx = makeGenesisTx(address.pubkey, randomNewAsset)
      wallet.scanOffchain(tx)
      val boxesToSpend = boxesAvailable(tx, address.pubkey)
      val balanceToSpend = balanceAmount(boxesToSpend)
      implicit val patienceConfig: PatienceConfig = PatienceConfig((offchainScanTime(tx) * 3).millis, 100.millis)
      val (spendingTx, totalBalance, balanceToReturn, assets) =
        eventually {
          val totalBalance = getBalancesWithUnconfirmed.walletBalance

          val balanceToReturn = randomLong(balanceToSpend)
          val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsWithRandom(boxesToSpend))
          //      val doubleSpendingTx = makeSpendingTx(boxesToSpend, address, randomLong(balanceToSpend))
          val assets = assetAmount(boxesAvailable(spendingTx, address.pubkey))
          assets should not be empty
          (spendingTx, totalBalance, balanceToReturn, assets)
        }
      wallet.scanOffchain(Seq(spendingTx, spendingTx))
      wallet.scanOffchain(spendingTx)

      log.info(s"Total with unconfirmed balance: $totalBalance")
      log.info(s"Balance to spent: $balanceToSpend")
      log.info(s"Balance to return back: $balanceToReturn")
      eventually {
        val totalAfterSpending = getBalancesWithUnconfirmed
        totalBalance shouldEqual balanceToSpend
        totalAfterSpending.walletBalance shouldEqual balanceToReturn
        totalAfterSpending.walletAssetBalances shouldEqual assets
      }
    }
  }

  property("off-chain spending of the on-chain box") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      val sumBalance = balanceAmount(boxesToSpend)
      log.info(s"Sum balance: $sumBalance")
      val balanceToReturn = randomLong(sumBalance)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val (spendingTx, assets) =
        eventually {
          val totalBalance = getBalancesWithUnconfirmed.walletBalance
          val confirmedBalance = getConfirmedBalances.walletBalance

          val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsWithRandom(boxesToSpend))
          val assets = assetAmount(boxesAvailable(spendingTx, address.pubkey))
          assets should not be empty
          confirmedBalance shouldBe sumBalance
          totalBalance shouldBe sumBalance
          log.info(s"Balance before spending: $confirmedBalance")
          log.info(s"Total with unconfirmed balance before spending: $totalBalance")
          (spendingTx, assets)
        }
      wallet.scanOffchain(spendingTx)
      eventually {
        val confirmedAfterSpending = getConfirmedBalances.walletBalance
        val totalAfterSpending = getBalancesWithUnconfirmed

        log.info(s"Balance after spending: $confirmedAfterSpending")
        log.info(s"Total with unconfirmed after spending: $totalAfterSpending")

        confirmedAfterSpending shouldBe sumBalance
        totalAfterSpending.walletBalance shouldBe balanceToReturn
        totalAfterSpending.walletAssetBalances shouldBe assets
      }
    }
  }

  property("assets application") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val asset1Sum = randomLong()
      val genesisBlock = makeGenesisBlock(address.pubkey, Seq(newAssetIdStub -> asset1Sum))
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val (asset1Token, asset1ToReturn, asset2Sum, spendingBlock) =
        eventually {
          val initialBalance = getConfirmedBalances
          val initialTotal = getBalancesWithUnconfirmed
          val initialAssets = initialBalance.walletAssetBalances
          log.info(s"Initial assets: ${boxesToSpend.flatMap(_.additionalTokens.toArray)}")
          log.info(s"Confirmed: $initialBalance")
          log.info(s"With unconfirmed: $initialTotal")
          initialAssets should not be empty
          val (asset1Token, asset1InitialValue) = initialAssets.head
          asset1InitialValue shouldBe asset1Sum
          initialTotal.walletAssetBalances shouldBe initialAssets

          val asset2Sum = randomLong()
          val asset1ToReturn = randomLong(asset1Sum)
          val assets2Seq = Seq(decodedTokenId(asset1Token) -> asset1ToReturn, newAssetIdStub -> asset2Sum)
          val balanceToReturn = 1000 * parameters.minValuePerByte
          val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assets2Seq)
          val spendingBlock = makeNextBlock(getUtxoState, Seq(spendingTx))
          applyBlock(spendingBlock) shouldBe 'success
          (asset1Token, asset1ToReturn, asset2Sum, spendingBlock)
        }
      wallet.scanPersistent(spendingBlock)
      eventually {
        val balanceAfterSpending = getConfirmedBalances
        val totalAfterSpending = getBalancesWithUnconfirmed
        log.info(s"After spending: $balanceAfterSpending")
        log.info(s"With unconfirmed after spending: $balanceAfterSpending")
        val assets = balanceAfterSpending.walletAssetBalances
        totalAfterSpending.walletAssetBalances.toMap shouldBe assets.toMap
        assets.find(_._1 == asset1Token).get._2 shouldBe asset1ToReturn
        val asset2 = assets.filter(_._1 != asset1Token)
        asset2 should not be empty
        asset2.head._2 shouldBe asset2Sum
      }
    }
  }

  property("on-chain box spending (without return)") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val (spendingBlock, boxesToSpend, confirmedBalance, balanceToSpend) =
        eventually {
          val confirmedBalance = getConfirmedBalances.walletBalance
          val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
          val balanceToSpend = balanceAmount(boxesToSpend)
          log.info(s"Confirmed balance $confirmedBalance")
          log.info(s"Sum balance: $balanceToSpend")
          confirmedBalance should be > 0L
          confirmedBalance shouldBe balanceToSpend

          val spendingTx = makeSpendingTx(boxesToSpend, address, 0, assetsWithRandom(boxesToSpend))

          val spendingBlock = makeNextBlock(getUtxoState, Seq(spendingTx))
          applyBlock(spendingBlock) shouldBe 'success
          (spendingBlock, boxesToSpend, confirmedBalance, balanceToSpend)
        }
      wallet.scanPersistent(spendingBlock)
      eventually {
        val balanceAfterSpending = getConfirmedBalances
        log.info(s"Boxes to spend: $boxesToSpend")
        log.info(s"Total with unconfirmed balance: $confirmedBalance")
        log.info(s"Balance to spent: $balanceToSpend")
        log.info(s"Balance after spend: ${balanceAfterSpending.walletBalance}")
        balanceAfterSpending.walletBalance shouldEqual 0
        getBalancesWithUnconfirmed shouldEqual balanceAfterSpending
      }
    }
  }

  property("on-chain box spending (with return)") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 300.millis)
      val (confirmedBalance, balanceToSpend, balanceToReturn, assets, spendingBlock) =
        eventually {
          val confirmedBalance = getConfirmedBalances.walletBalance
          val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
          val balanceToSpend = balanceAmount(boxesToSpend)
          log.info(s"Boxes to spend: $boxesToSpend")
          log.info(s"Confirmed balance $confirmedBalance")
          log.info(s"Sum balance: $balanceToSpend")
          confirmedBalance should be > 0L
          confirmedBalance shouldBe balanceToSpend

          val balanceToReturn = randomLong(balanceToSpend)
          val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsWithRandom(boxesToSpend))
          val assets = assetAmount(boxesAvailable(spendingTx, address.pubkey))
          assets should not be empty
          val spendingBlock = makeNextBlock(getUtxoState, Seq(spendingTx))
          applyBlock(spendingBlock) shouldBe 'success
          (confirmedBalance, balanceToSpend, balanceToReturn, assets, spendingBlock)
        }
      wallet.scanPersistent(spendingBlock)
      eventually {
        val balanceAfterSpending = getConfirmedBalances
        log.info(s"Total with unconfirmed balance: $confirmedBalance")
        log.info(s"Balance to spent: $balanceToSpend")
        log.info(s"Balance to return back: $balanceToReturn")
        balanceAfterSpending.walletBalance shouldEqual (confirmedBalance - balanceToSpend + balanceToReturn)
        balanceAfterSpending.walletAssetBalances.toMap shouldBe assets.toMap

        getBalancesWithUnconfirmed.height shouldEqual balanceAfterSpending.height
        getBalancesWithUnconfirmed.walletBalance shouldEqual balanceAfterSpending.walletBalance
        getBalancesWithUnconfirmed.walletAssetBalances.toMap shouldEqual balanceAfterSpending.walletAssetBalances.toMap
      }
    }
  }

  property("off-chain transaction becomes on-chain") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val tx = makeGenesisTx(pubKey, randomNewAsset)
      wallet.scanOffchain(tx)
      implicit val patienceConfig: PatienceConfig = PatienceConfig(offchainScanTime(tx).millis, 100.millis)
      val (initialBalance, sumBalance, sumAssets) =
        eventually {
          val boxesToSpend = boxesAvailable(tx, pubKey)
          val sumBalance = balanceAmount(boxesToSpend)
          val sumAssets = assetAmount(boxesToSpend)
          sumAssets should not be empty

          val initialBalance = getBalancesWithUnconfirmed.walletBalance
          initialBalance shouldBe sumBalance

          val block = makeNextBlock(getUtxoState, Seq(tx))
          applyBlock(block) shouldBe 'success
          (initialBalance, sumBalance, sumAssets)
        }

      eventually {
        val confirmedBalance = getConfirmedBalances
        log.info(s"Confirmed balance $confirmedBalance")
        log.info(s"Sum balance: $sumBalance")
        initialBalance shouldBe sumBalance
        confirmedBalance.walletBalance should be > 0L
        confirmedBalance.walletBalance shouldBe initialBalance
        confirmedBalance.walletAssetBalances shouldBe sumAssets
        getBalancesWithUnconfirmed shouldBe confirmedBalance
      }

    }
  }

  property("off-chain spending rollback") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey)
      val initialBoxes = boxesAvailable(genesisBlock, address.pubkey)
      val initialBalance = balanceAmount(initialBoxes)
      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState

      // We need this second block to have something to rollback. Just spent some balance to anyone
      val balanceToSpend = randomLong(initialBalance)
      val onchainSpendingTx = makeTx(initialBoxes, emptyProverResult, balanceToSpend, ErgoTree.fromSigmaBoolean(address.pubkey))
      val boxesToSpend = boxesAvailable(onchainSpendingTx, address.pubkey)
      val block = makeNextBlock(getUtxoState, Seq(onchainSpendingTx))
      applyBlock(block) shouldBe 'success
      wallet.scanPersistent(block)
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      val confirmedBalance =
        eventually {
          val totalBalance = getBalancesWithUnconfirmed.walletBalance
          val confirmedBalance = getConfirmedBalances.walletBalance

          confirmedBalance shouldBe balanceToSpend
          totalBalance shouldBe confirmedBalance
          log.info(s"Initial balance: $initialBalance")
          log.info(s"Balance before off-chain spending: $confirmedBalance")
          log.info(s"Total with unconfirmed balance before spending: $totalBalance")
          confirmedBalance
        }

      val balanceToReturn = randomLong(balanceAmount(boxesToSpend))
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn)
      wallet.scanOffchain(spendingTx)

      eventually {
        val confirmedAfterSpending = getConfirmedBalances.walletBalance
        val totalAfterSpending = getBalancesWithUnconfirmed.walletBalance

        confirmedAfterSpending shouldBe confirmedBalance
        totalAfterSpending shouldBe balanceToReturn

        log.info(s"After spending before rollback: $confirmedAfterSpending")
        log.info(s"Total with unconfirmed balance after spending before rollback: $totalAfterSpending")
      }

      wallet.rollback(initialState.version)
      eventually {
        val balanceAfterRollback = getConfirmedBalances.walletBalance
        val totalAfterRollback = getBalancesWithUnconfirmed.walletBalance

        log.info(s"Balance after rollback: $balanceAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        balanceAfterRollback shouldBe initialBalance
        totalAfterRollback shouldBe balanceToReturn
      }
    }
  }

  property("on-chain rollback") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey)
      val boxesToSpend = boxesAvailable(genesisBlock, pubKey)
      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      val (initialBalance, creationTx, initialAssets, balanceToSpend) =
        eventually {
          val initialBalance = getConfirmedBalances.walletBalance
          val balanceToSpend = randomLong(balanceAmount(boxesToSpend))
          val creationTx = makeTx(boxesToSpend, emptyProverResult, balanceToSpend, ErgoTree.fromSigmaBoolean(pubKey), randomNewAsset)
          val initialAssets = assetAmount(boxesAvailable(creationTx, pubKey))
          initialAssets should not be empty
          log.info(s"Initial balance: $initialBalance")
          log.info(s"Initial assets: $initialAssets")
          (initialBalance, creationTx, initialAssets, balanceToSpend)
        }

      val block = makeNextBlock(getUtxoState, Seq(creationTx))
      wallet.scanPersistent(block)
      eventually {
        val historyHeight = getHistory.headersHeight

        val confirmedBeforeRollback: WalletDigest = getConfirmedBalances
        val totalBeforeRollback = getBalancesWithUnconfirmed
        log.info(s"History height: $historyHeight")
        log.info(s"Confirmed balance: $confirmedBeforeRollback")
        log.info(s"Total with unconfirmed balance: $totalBeforeRollback")

        confirmedBeforeRollback.walletBalance shouldBe balanceToSpend
        confirmedBeforeRollback.walletAssetBalances shouldBe initialAssets
        totalBeforeRollback shouldBe confirmedBeforeRollback
      }
      wallet.rollback(initialState.version)
      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed

        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        confirmedAfterRollback.walletBalance shouldBe initialBalance
        confirmedAfterRollback.walletAssetBalances shouldBe empty
        totalAfterRollback.walletBalance shouldBe balanceToSpend
        totalAfterRollback.walletAssetBalances shouldBe initialAssets
      }
    }
  }

  property("on-chain spending rollback") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      val sumBalance = balanceAmount(boxesToSpend)
      val sumAssets = assetAmount(boxesToSpend)
      sumAssets should not be empty

      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.second, 100.millis)
      val (block, initialSnapshot) =
        eventually {
          val initialSnapshot = getConfirmedBalances
          log.info(s"Initial balance: $initialSnapshot")
          val spendingTx = makeSpendingTx(boxesToSpend, address)
          val block = makeNextBlock(getUtxoState, Seq(spendingTx))
          initialSnapshot.walletBalance shouldBe sumBalance
          initialSnapshot.walletAssetBalances shouldBe sumAssets
          (block, initialSnapshot)
        }
      wallet.scanPersistent(block)

      val confirmedBeforeRollback =
        eventually {
          val historyHeight = getHistory.headersHeight

          val confirmedBeforeRollback = getConfirmedBalances
          val totalBeforeRollback = getBalancesWithUnconfirmed

          log.info(s"Balance to spend: $sumBalance")
          log.info(s"History height: $historyHeight")
          log.info(s"Confirmed balance: $confirmedBeforeRollback")
          log.info(s"Total with unconfirmed balance: $totalBeforeRollback")

          confirmedBeforeRollback.walletBalance shouldBe 0L
          confirmedBeforeRollback.walletAssetBalances shouldBe empty
          totalBeforeRollback shouldBe confirmedBeforeRollback
          confirmedBeforeRollback
        }

      wallet.rollback(initialState.version)
      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed
        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        confirmedAfterRollback shouldBe initialSnapshot
        totalAfterRollback.walletBalance shouldBe confirmedBeforeRollback.walletBalance
        totalAfterRollback.walletAssetBalances shouldBe confirmedBeforeRollback.walletAssetBalances
      }
    }
  }

  property("on-chain spending with return rollback") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
      val boxesToSpend = boxesAvailable(genesisBlock, address.pubkey)
      val sumBalance = balanceAmount(boxesToSpend)

      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      val (block, initialSnapshot, asset1Map, balanceToReturn) =
        eventually {
          val initialSnapshot = getConfirmedBalances

          val balanceToReturn = randomLong(sumBalance)
          val sumAsset1 = assetsByTokenId(boxesToSpend).toSeq
          sumAsset1 should not be empty

          val asset1Map = toAssetMap(sumAsset1)
          val assetToReturn = sumAsset1.map { case (tokenId, tokenValue) => (tokenId, randomLong(tokenValue)) }
          val assetsForSpending = randomNewAsset ++ assetToReturn
          val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsForSpending)
          val block = makeNextBlock(getUtxoState, Seq(spendingTx))
          log.info(s"Initial balance: $initialSnapshot")
          log.info(s"Balance to spend: $sumBalance")
          log.info(s"Balance to return $balanceToReturn")
          initialSnapshot.walletBalance shouldBe sumBalance
          initialSnapshot.walletAssetBalances.toMap shouldBe asset1Map
          (block, initialSnapshot, asset1Map, balanceToReturn)
        }
      wallet.scanPersistent(block)

      val totalBeforeRollback =
        eventually {
          val historyHeight = getHistory.headersHeight
          val confirmedBeforeRollback = getConfirmedBalances
          val totalBeforeRollback = getBalancesWithUnconfirmed
          log.info(s"History height: $historyHeight")
          log.info(s"Confirmed balance: $confirmedBeforeRollback")
          log.info(s"Total with unconfirmed balance: $totalBeforeRollback")
          confirmedBeforeRollback.walletBalance should be > 0L
          confirmedBeforeRollback.walletBalance shouldBe balanceToReturn
          confirmedBeforeRollback.walletAssetBalances should have size 2
          totalBeforeRollback.walletBalance shouldBe balanceToReturn
          totalBeforeRollback.walletAssetBalances.toMap shouldBe confirmedBeforeRollback.walletAssetBalances.toMap
          totalBeforeRollback
        }
      wallet.rollback(initialState.version)

      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed
        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")
        confirmedAfterRollback shouldBe initialSnapshot
        confirmedAfterRollback.walletAssetBalances.toMap shouldBe asset1Map
        totalAfterRollback.walletBalance shouldBe balanceToReturn
        totalAfterRollback.walletAssetBalances shouldBe totalBeforeRollback.walletAssetBalances
      }
    }
  }

  property("on-chain spent box to off-chain box rollback") {
    withFixture { implicit w =>
      val address = getPublicKeys.head
      val genesisBlock = makeGenesisBlock(address.pubkey)
      val initialBoxes = boxesAvailable(genesisBlock, address.pubkey)
      applyBlock(genesisBlock) shouldBe 'success
      val initialState = getCurrentState
      val initialBalance = balanceAmount(initialBoxes)

      val balancePicked = randomLong(initialBalance)
      val creationTx = makeTx(initialBoxes, emptyProverResult, balancePicked, ErgoTree.fromSigmaBoolean(address.pubkey), randomNewAsset)
      val boxesToSpend = boxesAvailable(creationTx, address.pubkey)
      val balanceToSpend = balanceAmount(boxesToSpend)

      log.info(s"Initial balance: $initialBalance")
      log.info(s"Balance to spend: $balanceToSpend")
      balanceToSpend shouldBe balancePicked

      val balanceToReturn = randomLong(balanceToSpend)
      val sumAsset1 = assetsByTokenId(boxesToSpend).toSeq
      sumAsset1 should not be empty

      val assetToReturn = sumAsset1.map { case (tokenId, tokenValue) => (tokenId, randomLong(tokenValue)) }
      val assetsForSpending = randomNewAsset ++ assetToReturn
      val spendingTx = makeSpendingTx(boxesToSpend, address, balanceToReturn, assetsForSpending)
      val block = makeNextBlock(getUtxoState, Seq(creationTx, spendingTx))
      wallet.scanPersistent(block)

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      val totalBeforeRollback =
        eventually {
          val historyHeight = getHistory.headersHeight

          val confirmedBeforeRollback = getConfirmedBalances
          val totalBeforeRollback = getBalancesWithUnconfirmed
          log.info(s"History height: $historyHeight")
          log.info(s"Confirmed balance: $confirmedBeforeRollback")
          log.info(s"Total with unconfirmed balance: $totalBeforeRollback")

          confirmedBeforeRollback.walletBalance shouldBe balanceToReturn
          confirmedBeforeRollback.walletAssetBalances should have size 2

          totalBeforeRollback.walletBalance shouldBe confirmedBeforeRollback.walletBalance
          totalBeforeRollback.walletAssetBalances.toMap shouldBe confirmedBeforeRollback.walletAssetBalances.toMap
          totalBeforeRollback
        }
      wallet.rollback(initialState.version)

      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed

        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        confirmedAfterRollback.walletBalance shouldBe initialBalance
        totalAfterRollback.walletBalance shouldBe balanceToReturn
        totalAfterRollback.walletAssetBalances shouldBe totalBeforeRollback.walletAssetBalances
      }
    }
  }

  property("single-input transaction generation") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

        //pay out all the wallet balance:
        val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toArray
        assetToSpend should not be empty
        val req1 = PaymentRequest(Pay2SAddress(TrueTree), confirmedBalance, assetToSpend, Map.empty)

        val tx1 = await(wallet.generateTransaction(Seq(req1))).get
        tx1.outputs.size shouldBe 1
        tx1.outputs.head.value shouldBe confirmedBalance
        toAssetMap(tx1.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)

        //change == 1:
        val assetToSpend2 = assetToSpend.map { case (tokenId, tokenValue) => (tokenId, tokenValue - 1) }
        val assetToReturn = assetToSpend.map { case (tokenId, _) => (tokenId, 1L) }
        val req2 = PaymentRequest(Pay2SAddress(TrueTree), confirmedBalance - MinBoxValue, assetToSpend2, Map.empty)

        val tx2 = await(wallet.generateTransaction(Seq(req2))).get
        tx2.outputs.size shouldBe 2
        tx2.outputs.head.value shouldBe confirmedBalance - MinBoxValue
        toAssetMap(tx2.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend2)
        tx2.outputs(1).value shouldBe MinBoxValue
        toAssetMap(tx2.outputs(1).additionalTokens.toArray) shouldBe toAssetMap(assetToReturn)
      }
    }
  }

  property("generate unsigned transaction + sign (single input)") {
    withFixture { implicit w =>
      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

        //pay out all the wallet balance:
        val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toArray
        assetToSpend should not be empty
        val req1 = PaymentRequest(Pay2SAddress(TrueTree), confirmedBalance, assetToSpend, Map.empty)

        val utx = await(wallet.generateUnsignedTransaction(Seq(req1))).get
        utx.outputs.size shouldBe 1
        utx.outputs.head.value shouldBe confirmedBalance
        toAssetMap(utx.outputs.head.additionalTokens.toArray) shouldBe toAssetMap(assetToSpend)

        val tx = await(wallet.signTransaction(utx, Seq.empty, TransactionHintsBag.empty, None, None)).get
        tx.id shouldBe utx.id // signing preserves transaction id
      }
    }
  }

  property("co-signing (external secrets) - 2-out-of-2") {
    withFixture { implicit w =>

      val secret1 = DLogProverInput.random()
      val es1 = ExternalSecret(PrimitiveSecretKey(secret1))

      val secret2 = DLogProverInput.random()
      val es2 = ExternalSecret(PrimitiveSecretKey(secret2))

      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

        //pay out all the wallet balance:
        val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toArray
        assetToSpend should not be empty
        val req1 = PaymentRequest(Pay2SAddress(ErgoTree.fromSigmaBoolean(CAND(Seq(secret1.publicImage, secret2.publicImage)))), confirmedBalance, assetToSpend, Map.empty)

        val tx = await(wallet.generateTransaction(Seq(req1))).get

        val in = tx.outputs.head

        val utx = new UnsignedErgoTransaction(IndexedSeq(new UnsignedInput(in.id)), IndexedSeq.empty, IndexedSeq(in.toCandidate))

        val hints1 = await(wallet.generateCommitmentsFor(utx, Some(Seq(es1)), Some(Seq(in)), None)).response.get

        val txSigned = await(wallet.signTransaction(utx, Seq(es2), hints1, Some(Seq(in)), None)).get

        txSigned.statelessValidity().isSuccess shouldBe true
      }
    }
  }

  property("co-signing (external secrets) - 2-out-of-3") {
    withFixture { implicit w =>
      val secret1 = DLogProverInput.random()
      val es1 = ExternalSecret(PrimitiveSecretKey(secret1))

      val secret2 = DLogProverInput.random()
      val es2 = ExternalSecret(PrimitiveSecretKey(secret2))

      val secret3 = DLogProverInput.random()

      val pubKey = getPublicKeys.head.pubkey
      val genesisBlock = makeGenesisBlock(pubKey, randomNewAsset)
      applyBlock(genesisBlock) shouldBe 'success
      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
      eventually {
        val confirmedBalance = getConfirmedBalances.walletBalance

        //pay out all the wallet balance:
        val assetToSpend = assetsByTokenId(boxesAvailable(genesisBlock, pubKey)).toArray
        assetToSpend should not be empty
        val addr = Pay2SAddress(ErgoTree.fromSigmaBoolean(CTHRESHOLD(2, Seq(secret1.publicImage, secret2.publicImage, secret3.publicImage))))
        val req1 = PaymentRequest(addr, confirmedBalance, assetToSpend, Map.empty)

        val tx = await(wallet.generateTransaction(Seq(req1))).get

        val in = tx.outputs.head

        // secret1 and secret2 are signing
        val utx = new UnsignedErgoTransaction(IndexedSeq(new UnsignedInput(in.id)), IndexedSeq.empty, IndexedSeq(in.toCandidate))

        val cmts1 = await(wallet.generateCommitmentsFor(utx, Some(Seq(es1)), Some(Seq(in)), None)).response.get

        val pubCmts1 = TransactionHintsBag(cmts1.publicHints)

        val ptx = await(wallet.signTransaction(utx, Seq(es2), pubCmts1, Some(Seq(in)), None)).get

        val eh = wallet.extractHints(ptx, Seq(secret1.publicImage, secret2.publicImage), Seq(secret3.publicImage), Some(Seq(in)), None)
        val hintsExtracted = await(eh).transactionHintsBag

        val hints = hintsExtracted.addHintsForInput(0, cmts1.allHintsForInput(0))

        val txSigned = await(wallet.signTransaction(utx, Seq(es1), hints, Some(Seq(in)), None)).get
        txSigned.statelessValidity().isSuccess shouldBe true
      }
    }
  }

}
