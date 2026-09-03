package org.ergoplatform.nodeView.wallet

import akka.actor.{Actor, ActorRef, ActorSystem, Props, Status}
import akka.pattern.ask
import akka.testkit.TestProbe
import akka.util.{ByteString, Timeout}
import com.google.common.primitives.Ints
import org.ergoplatform._
import org.ergoplatform.ErgoBox.R1
import org.ergoplatform.core.{VersionTag, idToVersion, versionToId}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction, UnsignedErgoTransaction}
import org.ergoplatform.modifiers.history.header.PreGenesisHeader
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedMempool, ChangedState, CurrentWalletView, RequestCurrentWalletView, UtxoSnapshotAppliedToState}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader, UtxoStateReader, VotingData}
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.ErgoWalletServiceUtils.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.IdUtils._
import org.ergoplatform.nodeView.wallet.WalletScanLogic.ScanResults
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, UtxoSnapshotScanInvalidation, UtxoSnapshotScanStatus, UtxoSnapshotScanStatusSerializer, UtxoSnapshotWalletOrigin, UtxoSnapshotWalletOriginSerializer, WalletDigest, WalletDigestSerializer, WalletRegistry, WalletRollbackIntent, WalletStorage}
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
import scorex.util.{ModifierId, idToBytes}
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

class ErgoWalletSpec extends ErgoCorePropertyTest with WalletTestOps with MempoolTestHelpers with Eventually {
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

  private def publishCurrentMempool(implicit w: WalletFixture): Unit =
    w.actorSystem.eventStream.publish(ChangedMempool(getCurrentView.pool))

  private def withProbeWalletActor[T](baseSettings: org.ergoplatform.settings.ErgoSettings,
                                      directory: File = Files.createTempDirectory("wallet-run-fence-").toFile,
                                      historyReader: ErgoHistoryReader = strictHistoryReader(),
                                       sourceIdentity: Option[ModifierId => Try[UtxoSnapshotSourceIdentity]] = None,
                                       availableSourceIdentity: Option[() => Try[UtxoSnapshotSourceIdentity]] = None,
                                       registryTip: Option[ErgoWalletState => Try[(Int, Option[ModifierId])]] = None,
                                       currentStateTip: Option[ErgoWalletState => Try[(Int, Option[ModifierId])]] = None,
                                       bestHeaderId: Option[Int => Try[Option[ModifierId]]] = None,
                                       bestHeaderState: Option[Int => Try[Option[(ModifierId, ADDigest)]]] = None,
                                       snapshotFullHeight: Option[ErgoWalletState => Int] = None,
                                       catchUpReady: Option[Int => Boolean] = None,
                                       catchUpScan: Option[(ErgoWalletState, Int) => Try[ErgoWalletState]] = None,
                                       registryRollback: Option[(ErgoWalletState, VersionTag) => Try[Unit]] = None,
                                       rollbackIntentWrite: Option[(ErgoWalletState, WalletRollbackIntent) =>
                                         Try[Unit]] = None,
                                       offChainReconciliation: Option[(ErgoWalletState, Option[Long],
                                         () => ErgoWalletState) => ErgoWalletState] = None,
                                       utxoStateUpdate: Option[(ErgoWalletState, () => ErgoWalletState) =>
                                         ErgoWalletState] = None,
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
                                         actorPreStart: Option[() => Unit] = None,
                                         startupStateReader: Option[ErgoStateReader] = None,
                                         publishStartupState: Boolean = true,
                                         startupViewResponse:
                                           Option[RequestCurrentWalletView => Option[CurrentWalletView]] = None,
                                         expectedInitialized: Boolean = true)
                                     (test: (ActorRef, TestProbe, TestProbe) => T): T = {
    implicit val actorSystem: ActorSystem =
      ActorSystem(s"wallet-run-fence-${UUID.randomUUID().toString}")
    val scanner = TestProbe()
    val client = TestProbe()
    val isolatedDirectory = directory.getAbsolutePath
    val isolatedSettings = baseSettings.copy(
      directory = isolatedDirectory,
      walletSettings = baseSettings.walletSettings.copy(
        secretStorage = baseSettings.walletSettings.secretStorage.copy(
          secretDir = s"$isolatedDirectory/wallet/keystore")))
    val effectiveStartupStateReader = if (publishStartupState) {
      startupStateReader.orElse {
        val storage = WalletStorage.readOrCreate(isolatedSettings)
        try Some(genericStateReader(storage.getStateContext(parameters)))
        finally storage.close()
      }
    } else {
      None
    }
    val startupResponses = new AtomicInteger(0)
    val startupViewResponder = actorSystem.actorOf(Props(new Actor {
      override def receive: Receive = {
        case request: RequestCurrentWalletView =>
          val response = startupViewResponse match {
            case Some(customResponse) => customResponse(request)
            case None => effectiveStartupStateReader.map { reader =>
              CurrentWalletView(
                request.requestId,
                reader,
                new FakeMempool(Seq.empty),
                appliedSnapshot = None)
            }
          }
          response.foreach { currentView =>
            startupResponses.incrementAndGet()
            request.replyTo ! currentView
          }
      }
    }))
    actorSystem.eventStream.subscribe(startupViewResponder, classOf[RequestCurrentWalletView])
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

      override def reconcileOffChainRegistry(
        state: ErgoWalletState,
        dustLimit: Option[Long]): ErgoWalletState =
        offChainReconciliation.fold(super.reconcileOffChainRegistry(state, dustLimit)) { reconcile =>
          reconcile(state, dustLimit, () => super.reconcileOffChainRegistry(state, dustLimit))
        }

      override def updateUtxoState(state: ErgoWalletState): ErgoWalletState =
        utxoStateUpdate.fold(super.updateUtxoState(state)) { update =>
          update(state, () => super.updateUtxoState(state))
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

      override protected[wallet] def readAvailableUtxoSnapshotSourceIdentity(): Try[UtxoSnapshotSourceIdentity] =
        availableSourceIdentity.fold(super.readAvailableUtxoSnapshotSourceIdentity())(_())

      override protected[wallet] def readWalletRegistryTip(
        state: ErgoWalletState): Try[(Int, Option[ModifierId])] =
        registryTip.fold(super.readWalletRegistryTip(state))(_(state))

      override protected[wallet] def readCurrentStateTip(
        state: ErgoWalletState): Try[(Int, Option[ModifierId])] =
        currentStateTip.fold(super.readCurrentStateTip(state))(_(state))

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

      override protected[wallet] def rollbackWalletRegistry(
        state: ErgoWalletState,
        version: VersionTag): Try[Unit] =
        registryRollback.fold(super.rollbackWalletRegistry(state, version))(_(state, version))

      override protected[wallet] def writeWalletRollbackIntent(
        state: ErgoWalletState,
        intent: WalletRollbackIntent): Try[Unit] =
        rollbackIntentWrite.fold(super.writeWalletRollbackIntent(state, intent))(_(state, intent))

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
    client.expectMsgType[WalletStatus](10.seconds).initialized shouldBe expectedInitialized
    if (publishStartupState) {
      client.awaitAssert({
        startupResponses.get() should be > 0
        client.send(actor, GetWalletStatus)
        val status = client.expectMsgType[WalletStatus](10.seconds)
        status.error.exists(_.contains(
          "Wallet operations are unavailable while startup canonical alignment is pending")) shouldBe false
      }, 10.seconds, 100.millis)
    }
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

  private def strictHistoryReader(bestFullBlockReads: AtomicInteger = new AtomicInteger(0),
                                  heightLookup: ModifierId => Option[Int] = _ => None,
                                  bestFullBlockRead: Int => Unit = _ => (),
                                  bestFullBlockResult: Int => Option[ErgoFullBlock] = _ => None,
                                  minFullBlockAvailable: Int = Int.MaxValue,
                                  utxoSnapshotApplied: Boolean = false):
    ErgoHistoryReader = {
    Proxy.newProxyInstance(
      classOf[ErgoHistoryReader].getClassLoader,
      Array(classOf[ErgoHistoryReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef = method.getName match {
          case "heightOf" => heightLookup(args(0).asInstanceOf[ModifierId])
          case "bestFullBlockAt" =>
            bestFullBlockReads.incrementAndGet()
            val height = args(0).asInstanceOf[Int]
            bestFullBlockRead(height)
            bestFullBlockResult(height)
          case "minFullBlockAvailable" => Int.box(minFullBlockAvailable)
          case "isUtxoSnapshotApplied" => Boolean.box(utxoSnapshotApplied)
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

  private def genericStateReader(stateContext: ErgoStateContext): ErgoStateReader =
    Proxy.newProxyInstance(
      classOf[ErgoStateReader].getClassLoader,
      Array(classOf[ErgoStateReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "stateContext" => stateContext
            case "toString" => "wallet-startup-state-reader"
            case "hashCode" => Int.box(System.identityHashCode(proxy))
            case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq args(0))
            case other => throw new UnsupportedOperationException(s"Unexpected state read: $other")
          }
      }).asInstanceOf[ErgoStateReader]

  private final class SnapshotRecoveryPreflightCase(
    val label: String,
    val actorSettings: org.ergoplatform.settings.ErgoSettings,
    val eventHeight: Int,
    val eventBlockId: ModifierId,
    val stateReader: UtxoStateReader,
    val bestHeaderState: Try[Option[(ModifierId, ADDigest)]],
    val sourceIdentity: Try[UtxoSnapshotSourceIdentity],
    val expectedError: String,
    val expectedInitialized: Boolean)

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

  private def seedWalletRegistryVersions(
    baseSettings: org.ergoplatform.settings.ErgoSettings,
    directory: File,
    versions: Seq[(ModifierId, Int)]): Unit = {
    val isolatedSettings = baseSettings.copy(directory = directory.getAbsolutePath)
    val registry = WalletRegistry(isolatedSettings).get
    try {
      versions.foreach { case (versionId, height) =>
        registry.updateOnBlock(
          ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty),
          versionId,
          blockHeight = height).get
      }
    } finally registry.close()
  }

  private def seedEmptySnapshotRegistryBatch(
    baseSettings: org.ergoplatform.settings.ErgoSettings,
    directory: File,
    snapshotBlockId: ModifierId,
    snapshotHeight: Int,
    subtreeIndex: Int,
    nextSubtreeIndex: Int,
    finalChunk: Boolean): Unit = {
    val isolatedSettings = baseSettings.copy(directory = directory.getAbsolutePath)
    val registry = WalletRegistry(isolatedSettings).get
    try {
      registry.updateOnSnapshotChunk(
        ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty),
        snapshotBlockId,
        snapshotHeight,
        subtreeIndex,
        nextSubtreeIndex,
        finalChunk).get
    } finally registry.close()
  }

  private def corruptSnapshotRegistryMarker(
    baseSettings: org.ergoplatform.settings.ErgoSettings,
    directory: File,
    snapshotBlockId: ModifierId,
    subtreeIndex: Int): Unit = {
    val isolatedSettings = baseSettings.copy(directory = directory.getAbsolutePath)
    val store = new LDBVersionedStore(
      WalletRegistry.registryFolder(isolatedSettings),
      isolatedSettings.nodeSettings.keepVersions)
    try {
      val markerKey =
        Array(0x09.toByte) ++ idToBytes(snapshotBlockId) ++ Ints.toByteArray(subtreeIndex)
      val corrupted = store.get(markerKey).get.clone()
      corrupted(corrupted.length - 1) = (corrupted.last ^ 0x01).toByte
      store.update(
        Array.fill(32)(0x76.toByte),
        Seq.empty,
        Seq(markerKey -> corrupted)).get
    } finally store.close()
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

  property("require a correlated retried current view before startup canonical alignment") {
    val directory = Files.createTempDirectory("wallet-startup-alignment-gate-").toFile
    val reconciliations = new AtomicInteger(0)
    val rollbackAttempts = new AtomicInteger(0)
    val requestCount = new AtomicInteger(0)
    val observedRequestId = new AtomicReference[UUID](null)
    val requestIdChanged = new AtomicBoolean(false)
    val firstMempool = new FakeMempool(Seq.empty)
    val latestMempool = new FakeMempool(Seq.empty)
    val startupState = genericStateReader(
      ErgoStateContext.empty(settings.chainSettings, parameters))

    withProbeWalletActor(
      settings,
      directory,
      offChainReconciliation = Some((state, _, fallback) => {
        state.mempoolReaderOpt shouldBe Some(latestMempool)
        reconciliations.incrementAndGet()
        fallback()
      }),
      registryRollback = Some((state, version) => {
        rollbackAttempts.incrementAndGet()
        state.registry.rollback(version)
      }),
      publishStartupState = false,
      startupViewResponse = Some(request => {
        val firstRequestId = observedRequestId.get()
        if (firstRequestId == null) observedRequestId.compareAndSet(null, request.requestId)
        else if (firstRequestId != request.requestId) requestIdChanged.set(true)
        requestCount.incrementAndGet()
        None
      })) { (actor, scanner, client) =>
      client.awaitAssert(requestCount.get() should be > 0, 5.seconds, 100.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get.toLowerCase should include("startup canonical alignment")

      // Ambient view events and a response for another actor incarnation must not unlock startup.
      client.send(actor, ChangedState(startupState))
      client.send(actor, ChangedMempool(firstMempool))
      client.send(actor, ChangedMempool(latestMempool))
      client.send(actor, CurrentWalletView(
        UUID.randomUUID(), startupState, latestMempool, appliedSnapshot = None))
      client.send(actor, Rollback(idToVersion(PreGenesisHeader.id)))
      client.send(actor, DeriveNextKey)
      client.expectMsgType[DeriveNextKeyResult].result.isFailure shouldBe true
      reconciliations.get() shouldBe 0
      rollbackAttempts.get() shouldBe 0
      scanner.expectNoMessage(300.millis)

      client.awaitAssert(requestCount.get() should be >= 2, 5.seconds, 100.millis)
      requestIdChanged.get() shouldBe false
      client.send(actor, CurrentWalletView(
        observedRequestId.get(), startupState, latestMempool, appliedSnapshot = None))
      client.awaitAssert({
        reconciliations.get() shouldBe 1
        rollbackAttempts.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
    }
  }

  property("apply a captured startup view before newer queued state and mempool events") {
    val directory = Files.createTempDirectory("wallet-startup-view-order-").toFile
    val requestCount = new AtomicInteger(0)
    val observedRequestId = new AtomicReference[UUID](null)
    val stateReadEntered = new CountDownLatch(1)
    val releaseStateRead = new CountDownLatch(1)
    val reconciledViews = scala.collection.mutable.ArrayBuffer.empty[
      (Option[ErgoStateReader], Option[ErgoMemPoolReader])]
    val stateContext = ErgoStateContext.empty(settings.chainSettings, parameters)
    val capturedMempool = new FakeMempool(Seq.empty)
    val latestMempool = new FakeMempool(Seq.empty)
    val latestState = genericStateReader(stateContext)
    val capturedState = Proxy.newProxyInstance(
      classOf[ErgoStateReader].getClassLoader,
      Array(classOf[ErgoStateReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "stateContext" =>
              stateReadEntered.countDown()
              if (!releaseStateRead.await(5, TimeUnit.SECONDS)) {
                throw new IllegalStateException("Timed out waiting to queue the newer wallet view")
              }
              stateContext
            case "toString" => "captured-startup-state-reader"
            case "hashCode" => Int.box(System.identityHashCode(proxy))
            case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq args(0))
            case other => throw new UnsupportedOperationException(s"Unexpected state read: $other")
          }
      }).asInstanceOf[ErgoStateReader]

    def recordView(state: ErgoWalletState): Unit = reconciledViews.synchronized {
      reconciledViews += state.stateReaderOpt -> state.mempoolReaderOpt
    }

    withProbeWalletActor(
      settings,
      directory,
      publishStartupState = false,
      startupViewResponse = Some(request => {
        observedRequestId.compareAndSet(null, request.requestId)
        requestCount.incrementAndGet()
        None
      }),
      offChainReconciliation = Some((state, _, fallback) => {
        recordView(state)
        fallback()
      })) { (actor, scanner, client) =>
      client.awaitAssert(requestCount.get() should be > 0, 5.seconds, 100.millis)

      client.send(actor, CurrentWalletView(
        observedRequestId.get(), capturedState, capturedMempool, appliedSnapshot = None))
      stateReadEntered.await(5, TimeUnit.SECONDS) shouldBe true

      client.send(actor, ChangedState(latestState))
      client.send(actor, ChangedMempool(latestMempool))
      releaseStateRead.countDown()

      client.awaitAssert({
        val observed = reconciledViews.synchronized(reconciledViews.toVector)
        observed.size should be >= 2
        observed.head._1.value should be theSameInstanceAs capturedState
        observed.head._2.value should be theSameInstanceAs capturedMempool
        observed.last._1.value should be theSameInstanceAs latestState
        observed.last._2.value should be theSameInstanceAs latestMempool
      }, 5.seconds, 100.millis)
      scanner.expectNoMessage(300.millis)
    }
  }

  property("apply a captured startup view atomically while resuming a durable rollback") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-durable-startup-view-order-").toFile
    val originalTip = ModifierId @@ Algos.encode(Array.fill(32)(125: Byte))
    val expectedIntent = WalletRollbackIntent(PreGenesisHeader.id, expectedHeight = 0)
    seedNonemptyWalletRegistry(ordinarySettings, directory, originalTip)
    val seededRegistry = WalletRegistry(
      ordinarySettings.copy(directory = directory.getAbsolutePath)).get
    try seededRegistry.rollback(idToVersion(PreGenesisHeader.id)).get
    finally seededRegistry.close()
    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.writeWalletRollbackIntent(expectedIntent).get
    }

    val requestCount = new AtomicInteger(0)
    val observedRequestId = new AtomicReference[UUID](null)
    val rollbackAttempts = new AtomicInteger(0)
    val stateReadEntered = new CountDownLatch(1)
    val releaseStateRead = new CountDownLatch(1)
    val reconciledViews = scala.collection.mutable.ArrayBuffer.empty[
      (Option[ErgoStateReader], Option[ErgoMemPoolReader])]
    val stateContext = ErgoStateContext.empty(ordinarySettings.chainSettings, parameters)
    val capturedMempool = new FakeMempool(Seq.empty)
    val latestMempool = new FakeMempool(Seq.empty)
    val latestState = genericStateReader(stateContext)
    val capturedState = Proxy.newProxyInstance(
      classOf[ErgoStateReader].getClassLoader,
      Array(classOf[ErgoStateReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "stateContext" =>
              stateReadEntered.countDown()
              if (!releaseStateRead.await(5, TimeUnit.SECONDS)) {
                throw new IllegalStateException("Timed out waiting to queue the newer wallet view")
              }
              stateContext
            case "toString" => "captured-durable-startup-state-reader"
            case "hashCode" => Int.box(System.identityHashCode(proxy))
            case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq args(0))
            case other => throw new UnsupportedOperationException(s"Unexpected state read: $other")
          }
      }).asInstanceOf[ErgoStateReader]

    def recordView(state: ErgoWalletState): Unit = reconciledViews.synchronized {
      reconciledViews += state.stateReaderOpt -> state.mempoolReaderOpt
    }

    withProbeWalletActor(
      ordinarySettings,
      directory,
      publishStartupState = false,
      startupViewResponse = Some(request => {
        observedRequestId.compareAndSet(null, request.requestId)
        requestCount.incrementAndGet()
        None
      }),
      registryRollback = Some((_, _) => {
        rollbackAttempts.incrementAndGet()
        Failure(new IllegalStateException("completed durable rollback must not run twice"))
      }),
      offChainReconciliation = Some((state, _, fallback) => {
        recordView(state)
        fallback()
      })) { (actor, scanner, client) =>
      client.awaitAssert(requestCount.get() should be > 0, 5.seconds, 100.millis)

      client.send(actor, CurrentWalletView(
        observedRequestId.get(), capturedState, capturedMempool, appliedSnapshot = None))
      stateReadEntered.await(5, TimeUnit.SECONDS) shouldBe true

      client.send(actor, ChangedState(latestState))
      client.send(actor, ChangedMempool(latestMempool))
      releaseStateRead.countDown()

      client.awaitAssert({
        val observed = reconciledViews.synchronized(reconciledViews.toVector)
        observed.size should be >= 2
        observed.head._1.value should be theSameInstanceAs capturedState
        observed.head._2.value should be theSameInstanceAs capturedMempool
        observed.last._1.value should be theSameInstanceAs latestState
        observed.last._2.value should be theSameInstanceAs latestMempool
        rollbackAttempts.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
      scanner.expectNoMessage(300.millis)
    }

    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe None
    }
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
      client.expectMsgType[Status.Failure]

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
      client.expectMsgType[Status.Failure]
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
      }),
      utxoStateUpdate = Some((state, _) => state)) { (actor, scanner, client) =>
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
        client.expectMsgType[Status.Failure]
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
        client.expectMsgType[Status.Failure]

        getResult.isFailure shouldBe true
        getResult.failed.get.getMessage.toLowerCase should include("definition")
        applyResult.isFailure shouldBe true
        applyResult.failed.get.getMessage.toLowerCase should include("definition")
        chunkScans.get() shouldBe 0
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

  property("quarantine an applied snapshot when the height-zero wallet registry is non-pristine") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-non-pristine-snapshot-").toFile
    val isolatedSettings = bootstrapSettings.copy(directory = directory.getAbsolutePath)
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(0x45.toByte))
    val externalScanId = ScanId @@ 51.toShort
    val preexistingBox = new ErgoTransaction(
      fakeInputs,
      IndexedSeq.empty,
      IndexedSeq(new ErgoBoxCandidate(MinBoxValue, TrueTree, creationHeight = 0))).outputs.head
    val registry = WalletRegistry(isolatedSettings).get
    try {
      registry.updateScans(Set(externalScanId), preexistingBox).get
      registry.fetchDigest() shouldBe WalletDigest.empty
    } finally registry.close()

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader()) { (actor, scanner, client) =>
      client.send(actor, UtxoSnapshotAppliedToState(0, snapshotId, null))
      val result = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)

      result.isFailure shouldBe true
      result.failed.get.getMessage.toLowerCase should include("pristine")
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get.toLowerCase should include("pristine")
    }
  }

  property("start an available snapshot from its persisted identity after the state tip advances") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-source-behind-tip-").toFile
    val snapshotHeight = 5
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(0x46.toByte))
    val tipHeight = snapshotHeight + 3
    val tipId = ModifierId @@ Algos.encode(Array.fill(32)(0x47.toByte))
    val source = UtxoSnapshotSourceIdentity(
      snapshotHeight,
      snapshotId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      partCount = 33)
    val sourceReads = new AtomicInteger(0)
    val tipReader = snapshotRecoveryStateReader(
      bootstrapSettings,
      tipId,
      versionId = Some(tipId),
      stateHeight = tipHeight)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(utxoSnapshotApplied = true),
      availableSourceIdentity = Some(() => {
        sourceReads.incrementAndGet()
        Success(source)
      }),
      currentStateTip = Some(_ => Success(tipHeight -> Some(tipId))),
      bestHeaderId = Some(height => Success(
        if (height == tipHeight) Some(tipId) else None)),
      catchUpReady = Some(_ == snapshotHeight + 1),
      startupStateReader = Some(tipReader),
      utxoStateUpdate = Some((state, _) => state)) { (actor, scanner, client) =>
      val start = scanner.expectMsgType[StartUtxoSnapshotScan](5.seconds)
      start.run.hasSnapshot(snapshotHeight, snapshotId) shouldBe true
      start.run.hasSnapshot(tipHeight, tipId) shouldBe false
      sourceReads.get() shouldBe 1
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
    }
  }

  property("fail closed before scanning an available snapshot outside the full-block pruning horizon") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-source-pruned-catch-up-").toFile
    val snapshotHeight = 5
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(0x4a.toByte))
    val tipHeight = snapshotHeight + 3
    val tipId = ModifierId @@ Algos.encode(Array.fill(32)(0x4b.toByte))
    val source = UtxoSnapshotSourceIdentity(
      snapshotHeight,
      snapshotId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      partCount = 33)
    val readinessChecks = new AtomicInteger(0)
    val tipReader = snapshotRecoveryStateReader(
      bootstrapSettings,
      tipId,
      versionId = Some(tipId),
      stateHeight = tipHeight)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(utxoSnapshotApplied = true),
      availableSourceIdentity = Some(() => Success(source)),
      currentStateTip = Some(_ => Success(tipHeight -> Some(tipId))),
      bestHeaderId = Some(height => Success(
        if (height == tipHeight) Some(tipId) else None)),
      snapshotFullHeight = Some(_ => tipHeight),
      catchUpReady = Some(height => {
        height shouldBe snapshotHeight + 1
        readinessChecks.incrementAndGet()
        false
      }),
      startupStateReader = Some(tipReader),
      utxoStateUpdate = Some((state, _) => state)) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      readinessChecks.get() shouldBe 1
      client.send(actor, GetWalletStatus)
      val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
      error should include("pruning horizon")
      error should include("resync")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe None
    }
  }

  property("fail closed before resuming persisted snapshot progress outside the full-block pruning horizon") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-persisted-snapshot-pruned-catch-up-").toFile
    val snapshotHeight = 5
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(0x4c.toByte))
    val tipHeight = snapshotHeight + 3
    val status = snapshotStatus(
      snapshotHeight,
      snapshotId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 0,
      totalSubtrees = 33,
      completed = false)
    val source = UtxoSnapshotSourceIdentity(
      snapshotHeight, snapshotId, status.manifestDepth, status.totalSubtrees)
    val readinessChecks = new AtomicInteger(0)
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      snapshotFullHeight = Some(_ => tipHeight),
      catchUpReady = Some(height => {
        height shouldBe snapshotHeight + 1
        readinessChecks.incrementAndGet()
        false
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      readinessChecks.get() shouldBe 1
      client.send(actor, GetWalletStatus)
      val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
      error should include("pruning horizon")
      error should include("resync from genesis")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe None
    }
  }

  property("quarantine snapshot progress when its registry batch marker is missing after restart") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-cursor-rewind-").toFile
    val snapshotHeight = 5
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(0x48.toByte))
    val status = snapshotStatus(
      snapshotHeight,
      snapshotId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = UtxoSnapshotWalletScanner.SnapshotScanBatchSize,
      totalSubtrees = UtxoSnapshotWalletScanner.SnapshotScanBatchSize * 2,
      completed = false)
    val source = UtxoSnapshotSourceIdentity(
      snapshotHeight,
      snapshotId,
      status.manifestDepth,
      status.totalSubtrees)
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source))) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
      error should include("quarantine")
      error should include("marker")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe
        Some(UtxoSnapshotScanInvalidation(snapshotHeight, snapshotId))
    }
  }

  property("validate the last durable snapshot registry batch without rewriting progress") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-frontier-valid-").toFile
    val snapshotHeight = 5
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(0x47.toByte))
    val batchSize = UtxoSnapshotWalletScanner.SnapshotScanBatchSize
    val status = snapshotStatus(
      snapshotHeight,
      snapshotId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = batchSize,
      totalSubtrees = batchSize * 2,
      completed = false)
    val source = UtxoSnapshotSourceIdentity(
      snapshotHeight, snapshotId, status.manifestDepth, status.totalSubtrees)
    seedEmptySnapshotRegistryBatch(
      bootstrapSettings,
      directory,
      snapshotId,
      snapshotHeight,
      subtreeIndex = 0,
      nextSubtreeIndex = batchSize,
      finalChunk = false)
    withSeededWalletStorage(bootstrapSettings, directory)(
      _.writeUtxoSnapshotScanStatus(status).get)
    val statusBytesBefore = readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source))) { (actor, scanner, client) =>
      val start = scanner.expectMsgType[StartUtxoSnapshotScan](5.seconds)
      client.send(actor, ApplyUtxoSnapshotScanBatch(
        start.run,
        subtreeIndex = 0,
        nextSubtreeIndex = batchSize,
        completed = false,
        boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get shouldBe status
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
    }

    readWalletStorageEntry(
      bootstrapSettings, directory, WalletStorage.UtxoSnapshotScanStatusKey).get should
      contain theSameElementsInOrderAs statusBytesBefore
    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
    }
  }

  property("quarantine a corrupt last durable snapshot registry batch") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-frontier-corrupt-").toFile
    val snapshotHeight = 5
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(0x46.toByte))
    val batchSize = UtxoSnapshotWalletScanner.SnapshotScanBatchSize
    val status = snapshotStatus(
      snapshotHeight,
      snapshotId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = batchSize,
      totalSubtrees = batchSize * 2,
      completed = false)
    val source = UtxoSnapshotSourceIdentity(
      snapshotHeight, snapshotId, status.manifestDepth, status.totalSubtrees)
    seedEmptySnapshotRegistryBatch(
      bootstrapSettings,
      directory,
      snapshotId,
      snapshotHeight,
      subtreeIndex = 0,
      nextSubtreeIndex = batchSize,
      finalChunk = false)
    corruptSnapshotRegistryMarker(
      bootstrapSettings, directory, snapshotId, subtreeIndex = 0)
    withSeededWalletStorage(bootstrapSettings, directory)(
      _.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source))) { (actor, scanner, client) =>
      val start = scanner.expectMsgType[StartUtxoSnapshotScan](5.seconds)
      client.send(actor, ApplyUtxoSnapshotScanBatch(
        start.run,
        subtreeIndex = 0,
        nextSubtreeIndex = batchSize,
        completed = false,
        boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].isFailure shouldBe true
      scanner.expectMsg(AbortUtxoSnapshotScan(start.run))
      client.send(actor, GetWalletStatus)
      val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
      error should include("quarantine")
      error should include("marker")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe
        Some(UtxoSnapshotScanInvalidation(snapshotHeight, snapshotId))
    }
  }

  property("recover the single registry-ahead snapshot batch without double applying it") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-registry-ahead-").toFile
    val snapshotHeight = 5
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(0x45.toByte))
    val batchSize = UtxoSnapshotWalletScanner.SnapshotScanBatchSize
    val status = snapshotStatus(
      snapshotHeight,
      snapshotId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 0,
      totalSubtrees = batchSize * 2,
      completed = false)
    val source = UtxoSnapshotSourceIdentity(
      snapshotHeight, snapshotId, status.manifestDepth, status.totalSubtrees)
    seedEmptySnapshotRegistryBatch(
      bootstrapSettings,
      directory,
      snapshotId,
      snapshotHeight,
      subtreeIndex = 0,
      nextSubtreeIndex = batchSize,
      finalChunk = false)
    withSeededWalletStorage(bootstrapSettings, directory)(
      _.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source))) { (actor, scanner, client) =>
      val start = scanner.expectMsgType[StartUtxoSnapshotScan](5.seconds)
      client.send(actor, ApplyUtxoSnapshotScanBatch(
        start.run,
        subtreeIndex = 0,
        nextSubtreeIndex = batchSize,
        completed = false,
        boxes = IndexedSeq.empty))
      val recovered = client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      recovered.nextSubtreeIndex shouldBe batchSize
      recovered.completed shouldBe false
      scanner.expectNoMessage(300.millis)
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      val recovered = storage.readUtxoSnapshotScanStatusTry().get.get
      recovered.nextSubtreeIndex shouldBe batchSize
      recovered.completed shouldBe false
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
    }
  }

  property("quarantine a snapshot registry more than one batch ahead of durable progress") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-registry-too-far-ahead-").toFile
    val snapshotHeight = 5
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(0x44.toByte))
    val batchSize = UtxoSnapshotWalletScanner.SnapshotScanBatchSize
    val status = snapshotStatus(
      snapshotHeight,
      snapshotId,
      ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 0,
      totalSubtrees = batchSize * 3,
      completed = false)
    val source = UtxoSnapshotSourceIdentity(
      snapshotHeight, snapshotId, status.manifestDepth, status.totalSubtrees)
    Seq(0, batchSize).foreach { subtreeIndex =>
      seedEmptySnapshotRegistryBatch(
        bootstrapSettings,
        directory,
        snapshotId,
        snapshotHeight,
        subtreeIndex,
        nextSubtreeIndex = subtreeIndex + batchSize,
        finalChunk = false)
    }
    withSeededWalletStorage(bootstrapSettings, directory)(
      _.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source))) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
      error should include("quarantine")
      error should include("more than one batch ahead")
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe
        Some(UtxoSnapshotScanInvalidation(snapshotHeight, snapshotId))
    }
  }

  property("retain only the latest mempool reader while a snapshot wallet is quarantined") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-quarantine-mempool-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(125: Byte))
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
    val staleMempool = new FakeMempool(Seq.empty)
    val currentMempool = new FakeMempool(Seq.empty)
    val observedMempool = new AtomicReference[ErgoMemPoolReader](null)
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
      recoveryRestart = Some((state, _, _, fallback) => {
        observedMempool.set(state.mempoolReaderOpt.orNull)
        fallback()
      })) { (actor, scanner, client) =>
      client.send(actor, ChangedMempool(staleMempool))
      client.send(actor, ChangedMempool(currentMempool))
      client.send(actor, UtxoSnapshotAppliedToState(
        invalidation.snapshotHeight, invalidation.snapshotBlockId, stateReader))
      val recoveryResult = client.fishForMessage(5.seconds) {
        case _: Try[_] => true
        case _ => false
      }.asInstanceOf[Try[Option[UtxoSnapshotScanRun]]]

      recoveryResult.isSuccess shouldBe true
      (observedMempool.get() eq currentMempool) shouldBe true
      scanner.expectMsgType[StartUtxoSnapshotScan](5.seconds).forceRestart shouldBe true
    }
  }

  property("skip duplicate operational notifications for the same immutable mempool reader") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-operational-mempool-noop-").toFile
    val stateReader = genericStateReader(
      ErgoStateContext.empty(ordinarySettings.chainSettings, parameters))
    val mempoolReader = new FakeMempool(Seq.empty)
    val reconciliations = new AtomicInteger(0)

    withProbeWalletActor(
      ordinarySettings,
      directory,
      publishStartupState = false,
      startupViewResponse = Some(request => Some(CurrentWalletView(
        request.requestId, stateReader, mempoolReader, appliedSnapshot = None))),
      offChainReconciliation = Some((_, _, fallback) => {
        reconciliations.incrementAndGet()
        fallback()
      })) { (actor, scanner, client) =>
      client.awaitAssert(reconciliations.get() shouldBe 1, 5.seconds, 100.millis)

      (1 to 100).foreach(_ => client.send(actor, ChangedMempool(mempoolReader)))
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
      reconciliations.get() shouldBe 1
      scanner.expectNoMessage(300.millis)
    }
  }

  property("reconcile off-chain outputs when the operational mempool reader changes") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-operational-mempool-reconcile-").toFile
    val walletKey = defaultProver.hdPubKeys.head.key
    val walletAddress = P2PKAddress(walletKey)(ordinarySettings.addressEncoder)
    val fundingTx = makeGenesisTx(walletKey)
    val fundingBoxes = boxesAvailable(fundingTx, walletKey)
    val changeValue = balanceAmount(fundingBoxes) / 2
    val spendingTx = makeSpendingTx(
      fundingBoxes, walletAddress, balanceToReturn = changeValue)

    withProbeWalletActor(ordinarySettings, directory) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, ScanOffChain(spendingTx))
      client.send(actor, ReadBalances(ChainStatus.OffChain))
      client.expectMsgType[WalletDigest].walletBalance shouldBe changeValue

      client.send(actor, ChangedMempool(new FakeMempool(Seq.empty)))
      client.send(actor, ReadBalances(ChainStatus.OffChain))
      client.expectMsgType[WalletDigest].walletBalance shouldBe 0L

      client.send(actor, ChangedMempool(new FakeMempool(Seq(
        UnconfirmedTransaction(spendingTx, None)))))
      client.send(actor, ReadBalances(ChainStatus.OffChain))
      client.expectMsgType[WalletDigest].walletBalance shouldBe changeValue
    }
  }

  property("retry a quarantined operational reconciliation with the same mempool reader") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-operational-mempool-failure-").toFile
    val preStarts = new AtomicInteger(0)
    val rollbackIntentWrites = new AtomicInteger(0)
    val remainingFailures = new AtomicInteger(1)
    val injectedMessage = "injected operational mempool reconciliation failure"
    val failingMempool = new FakeMempool(Seq.empty) {
      override def getAllPrioritized: Seq[UnconfirmedTransaction] = {
        if (remainingFailures.getAndDecrement() > 0) {
          throw new IllegalStateException(injectedMessage)
        }
        super.getAllPrioritized
      }
    }

    withProbeWalletActor(
      ordinarySettings,
      directory,
      actorPreStart = Some(() => preStarts.incrementAndGet()),
      rollbackIntentWrite = Some((state, intent) => {
        rollbackIntentWrites.incrementAndGet()
        state.storage.writeWalletRollbackIntent(intent)
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      preStarts.get() shouldBe 1

      client.send(actor, ChangedMempool(failingMempool))
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get should include(injectedMessage)
      client.send(actor, ReadBalances(ChainStatus.OffChain))
      client.expectMsgType[Status.Failure]
      preStarts.get() shouldBe 1
      rollbackIntentWrites.get() shouldBe 0

      // Quarantine must retry even when the immutable reader instance is unchanged.
      client.send(actor, ChangedMempool(failingMempool))
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
      client.send(actor, ReadBalances(ChainStatus.OffChain))
      client.expectMsgType[WalletDigest].walletBalance shouldBe 0L
      preStarts.get() shouldBe 1
      rollbackIntentWrites.get() shouldBe 0
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
      sourceIdentity: Try[UtxoSnapshotSourceIdentity] = Success(validSource),
      expectedInitialized: Boolean = true):
      SnapshotRecoveryPreflightCase =
      new SnapshotRecoveryPreflightCase(
        label,
        actorSettings,
        eventHeight,
        eventBlockId,
        stateReader,
        bestHeaderState,
        sourceIdentity,
        expectedError,
        expectedInitialized)

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
        stateReader = snapshotRecoveryStateReader(noScanSettings, snapshotId),
        expectedInitialized = false),
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
        }),
        expectedInitialized = testCase.expectedInitialized) { (actor, scanner, client) =>
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
        val fenceReadsBaseline = fenceReads.get()
        client.send(actor, UtxoSnapshotAppliedToState(
          invalidation.snapshotHeight,
          invalidation.snapshotBlockId,
          testCase.stateReader))
        val result = client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]](5.seconds)
        withClue(testCase.label) {
          result.isFailure shouldBe true
          result.failed.get.getMessage.toLowerCase should include(testCase.expectedError)
          fenceReads.get() - fenceReadsBaseline shouldBe testCase.expectedFenceReads
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
      }),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      sourceRemoval = Some(_ => Success(()))) { (actor, scanner, client) =>
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

  property("recover completed finalization after automatic cleanup retry exhaustion") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-cleanup-recovery-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(70: Byte))
    val allowCleanup = new AtomicBoolean(false)
    val statusAttempts = new AtomicInteger(0)
    val sourceRemovals = new AtomicInteger(0)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      snapshotFullHeight = Some(_ => 0),
      statusRemoval = Some(state => {
        statusAttempts.incrementAndGet()
        if (allowCleanup.get()) state.storage.removeUtxoSnapshotScanStatus()
        else Failure(new IllegalStateException("injected persistent status removal failure"))
      }),
      sourceRemoval = Some(id => {
        id shouldBe snapshotId
        sourceRemovals.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 1,
        completed = true, boxes = IndexedSeq.empty))
      val completedStatus = client.expectMsgType[Try[UtxoSnapshotScanStatus]].get

      client.awaitAssert(
        statusAttempts.get() shouldBe ErgoWalletActor.MaxFinalizationCleanupRetries + 1,
        8.seconds,
        100.millis)
      client.expectNoMessage(ErgoWalletActor.FinalizationCleanupRetryDelay + 300.millis)
      sourceRemovals.get() shouldBe 0
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get should include(
        "Unable to persist completed UTXO snapshot origin or remove completed UTXO snapshot scan status")

      allowCleanup.set(true)
      client.send(actor, FinalizeUtxoSnapshotScan(run, completedStatus, cleanupAttempt = 0))
      client.awaitAssert({
        statusAttempts.get() shouldBe ErgoWalletActor.MaxFinalizationCleanupRetries + 2
        sourceRemovals.get() shouldBe 1
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(snapshotOrigin(0, snapshotId))
    }
  }

  property("reject a wallet read queued behind the final snapshot batch until durable finalization") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-prequeued-read-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(119: Byte))
    val actorRef = new AtomicReference[ActorRef]()
    val clientRef = new AtomicReference[ActorRef]()
    val queuedRead = new AtomicBoolean(false)
    val statusRemovals = new AtomicInteger(0)
    val sourceRemovals = new AtomicInteger(0)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      snapshotFullHeight = Some(_ => 0),
      snapshotChunkScan = Some(() => {
        if (queuedRead.compareAndSet(false, true)) {
          actorRef.get().tell(ReadBalances(ChainStatus.OnChain), clientRef.get())
        }
      }),
      statusRemoval = Some(state => {
        statusRemovals.incrementAndGet()
        state.storage.removeUtxoSnapshotScanStatus()
      }),
      sourceRemoval = Some(_ => {
        sourceRemovals.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      actorRef.set(actor)
      clientRef.set(client.ref)
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get

      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 1,
        completed = true, boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get.completed shouldBe true
      client.expectMsgType[Status.Failure]

      client.awaitAssert({
        statusRemovals.get() shouldBe 1
        sourceRemovals.get() shouldBe 1
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest]
    }
  }

  property("drain a deferred snapshot block before reopening wallet operations") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-deferred-finalization-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(120: Byte))
    val deferredBase = invalidErgoFullBlockGen.sample.get
    val deferredBlock = deferredBase.copy(
      header = deferredBase.header.copy(height = 1, parentId = snapshotId))
    val actorRef = new AtomicReference[ActorRef]()
    val clientRef = new AtomicReference[ActorRef]()
    val deferredSent = new AtomicBoolean(false)
    val queuedRead = new AtomicBoolean(false)
    val statusRemovals = new AtomicInteger(0)
    val sourceRemovals = new AtomicInteger(0)
    val events = scala.collection.mutable.ArrayBuffer.empty[String]
    def record(event: String): Unit = events.synchronized(events += event)
    def observedEvents: Seq[String] = events.synchronized(events.toSeq)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      snapshotFullHeight = Some(_ => 0),
      bestHeaderId = Some {
        case 1 => Success(Some(deferredBlock.id))
        case _ => Success(None)
      },
      offChainReconciliation = Some((state, _, fallback) => {
        record(s"reconcile-${state.getWalletHeight}")
        fallback()
      }),
      snapshotChunkScan = Some(() => {
        if (deferredSent.compareAndSet(false, true)) {
          actorRef.get().tell(ScanOnChain(deferredBlock), actorRef.get())
          actorRef.get().tell(
            ChangedMempool(new FakeMempool(Seq.empty)), actorRef.get())
        }
      }),
      statusRemoval = Some(state => {
        record("status-remove")
        statusRemovals.incrementAndGet()
        if (queuedRead.compareAndSet(false, true)) {
          actorRef.get().tell(ReadBalances(ChainStatus.OnChain), clientRef.get())
        }
        state.storage.removeUtxoSnapshotScanStatus()
      }),
      sourceRemoval = Some(_ => {
        sourceRemovals.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      actorRef.set(actor)
      clientRef.set(client.ref)
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get

      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 1,
        completed = true, boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get.completed shouldBe true
      client.expectMsgType[WalletDigest](5.seconds).height shouldBe 1

      client.awaitAssert({
        observedEvents.takeRight(2) shouldBe Seq("reconcile-1", "status-remove")
        statusRemovals.get() shouldBe 1
        sourceRemovals.get() shouldBe 1
      }, 5.seconds, 100.millis)
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(snapshotOrigin(0, snapshotId))
    }
  }

  property("reject a retained snapshot catch-up block that is not canonical") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-retained-noncanonical-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(121: Byte))
    val canonicalSiblingId = ModifierId @@ Algos.encode(Array.fill(32)(122: Byte))
    val retainedBase = invalidErgoFullBlockGen.sample.get
    val retainedBlock = retainedBase.copy(
      header = retainedBase.header.copy(height = 1, parentId = snapshotId))
    val actorRef = new AtomicReference[ActorRef]()
    val retainedSent = new AtomicBoolean(false)
    val statusRemovals = new AtomicInteger(0)
    val sourceRemovals = new AtomicInteger(0)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      snapshotFullHeight = Some(_ => 0),
      bestHeaderId = Some {
        case 1 => Success(Some(canonicalSiblingId))
        case _ => Success(None)
      },
      snapshotChunkScan = Some(() => {
        if (retainedSent.compareAndSet(false, true)) {
          actorRef.get().tell(ScanOnChain(retainedBlock), actorRef.get())
        }
      }),
      statusRemoval = Some(state => {
        statusRemovals.incrementAndGet()
        state.storage.removeUtxoSnapshotScanStatus()
      }),
      sourceRemoval = Some(_ => {
        sourceRemovals.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      actorRef.set(actor)
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get

      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 1,
        completed = true, boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get.completed shouldBe true
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get.toLowerCase should include("not canonical")
      }, 5.seconds, 100.millis)
      statusRemovals.get() shouldBe 0
      sourceRemovals.get() shouldBe 0
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    val registry = WalletRegistry(
      bootstrapSettings.copy(directory = directory.getAbsolutePath)).get
    try {
      registry.lastVersionId shouldBe Some(snapshotId)
      registry.fetchDigest().height shouldBe 0
    } finally registry.close()
    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get.map(_.completed) shouldBe Some(true)
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(snapshotOrigin(0, snapshotId))
    }
  }

  property("reconcile the latest mempool after ordinary catch-up before reopening") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-catchup-final-mempool-").toFile
    val blockOneBase = invalidErgoFullBlockGen.sample.get
    val blockOne = blockOneBase.copy(
      header = blockOneBase.header.copy(height = 1, parentId = PreGenesisHeader.id))
    val blockTwoBase = invalidErgoFullBlockGen.sample.get
    val blockTwo = blockTwoBase.copy(
      header = blockTwoBase.header.copy(height = 2, parentId = blockOne.id))
    val firstCatchUpRead = new CountDownLatch(1)
    val releaseFirstCatchUpRead = new CountDownLatch(1)
    val reconciliationHeights = scala.collection.mutable.ArrayBuffer.empty[Int]
    def recordReconciliation(height: Int): Unit =
      reconciliationHeights.synchronized(reconciliationHeights += height)
    def observedReconciliationHeights: Seq[Int] =
      reconciliationHeights.synchronized(reconciliationHeights.toSeq)

    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(
        bestFullBlockRead = {
          case 1 =>
            firstCatchUpRead.countDown()
            if (!releaseFirstCatchUpRead.await(5, TimeUnit.SECONDS)) {
              throw new IllegalStateException("Timed out waiting to release the first catch-up read")
            }
          case _ => ()
        },
        bestFullBlockResult = {
          case 1 => Some(blockOne)
          case 2 => Some(blockTwo)
          case _ => None
        }),
      offChainReconciliation = Some((state, _, fallback) => {
        recordReconciliation(state.getWalletHeight)
        fallback()
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, ScanOnChain(blockTwo))
      firstCatchUpRead.await(5, TimeUnit.SECONDS) shouldBe true
      client.send(actor, ChangedMempool(new FakeMempool(Seq.empty)))
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      releaseFirstCatchUpRead.countDown()

      client.expectMsgType[Status.Failure](5.seconds)
      client.awaitAssert({
        observedReconciliationHeights.lastOption shouldBe Some(2)
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
      observedReconciliationHeights should contain (1)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest].height shouldBe 2
    }
  }

  property("retry completed snapshot catch-up while its block remains inside the pruning horizon") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 10,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-retained-catchup-retry-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(75: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 1,
      totalSubtrees = 1,
      completed = true)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    val blockBase = invalidErgoFullBlockGen.sample.get
    val catchUpBlock = blockBase.copy(
      header = blockBase.header.copy(height = 1, parentId = snapshotId))
    val blockAvailable = new AtomicBoolean(false)
    val statusRemovals = new AtomicInteger(0)
    val sourceRemovals = new AtomicInteger(0)
    val catchUpStateContext = new ErgoStateContext(
      Seq(catchUpBlock.header),
      None,
      startDigest,
      parameters,
      validationSettingsNoIl,
      VotingData.empty)(bootstrapSettings.chainSettings)
    val stateReader = snapshotRecoveryStateReader(
      bootstrapSettings,
      catchUpBlock.id,
      versionId = Some(catchUpBlock.id),
      stateHeight = 1,
      stateContextOverride = Some(catchUpStateContext))

    seedWalletRegistryVersions(bootstrapSettings, directory, Seq(snapshotId -> 0))
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(bestFullBlockResult = {
        case 1 if blockAvailable.get() => Some(catchUpBlock)
        case _ => None
      }, minFullBlockAvailable = 1),
      sourceIdentity = Some(_ => Success(source)),
      bestHeaderId = Some {
        case 0 => Success(Some(snapshotId))
        case 1 => Success(Some(catchUpBlock.id))
        case _ => Success(None)
      },
      snapshotFullHeight = Some(_ => 1),
      catchUpReady = Some(_ => true),
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
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        error should include("canonical state change or rollback retries")
        error should not include "pruning"
        error should not include "re-bootstrap"
      }, 5.seconds, 100.millis)
      statusRemovals.get() shouldBe 0
      sourceRemovals.get() shouldBe 0
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]

      blockAvailable.set(true)
      actor.tell(ChangedState(stateReader), ActorRef.noSender)

      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
        statusRemovals.get() shouldBe 1
        sourceRemovals.get() shouldBe 1
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest].height shouldBe 1
      scanner.expectNoMessage(300.millis)
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(snapshotOrigin(0, snapshotId))
    }
  }

  property("keep the ordinary post-catch-up barrier closed when final mempool reconciliation fails") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-catchup-final-mempool-failure-").toFile
    val blockOneBase = invalidErgoFullBlockGen.sample.get
    val blockOne = blockOneBase.copy(
      header = blockOneBase.header.copy(height = 1, parentId = PreGenesisHeader.id))
    val blockTwoBase = invalidErgoFullBlockGen.sample.get
    val blockTwo = blockTwoBase.copy(
      header = blockTwoBase.header.copy(height = 2, parentId = blockOne.id))
    val firstCatchUpRead = new CountDownLatch(1)
    val releaseFirstCatchUpRead = new CountDownLatch(1)

    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(
        bestFullBlockRead = {
          case 1 =>
            firstCatchUpRead.countDown()
            if (!releaseFirstCatchUpRead.await(5, TimeUnit.SECONDS)) {
              throw new IllegalStateException("Timed out waiting to release the first catch-up read")
            }
          case _ => ()
        },
        bestFullBlockResult = {
          case 1 => Some(blockOne)
          case 2 => Some(blockTwo)
          case _ => None
        }),
      offChainReconciliation = Some((state, _, fallback) => {
        if (state.getWalletHeight == 2) {
          throw new IllegalStateException("injected post-catch-up reconciliation failure")
        }
        fallback()
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, ScanOnChain(blockTwo))
      firstCatchUpRead.await(5, TimeUnit.SECONDS) shouldBe true
      client.send(actor, ChangedMempool(new FakeMempool(Seq.empty)))
      releaseFirstCatchUpRead.countDown()

      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        error should include("final mempool reconciliation")
        error should include("injected post-catch-up reconciliation failure")
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }
  }

  property("keep the snapshot post-catch-up barrier closed when final mempool reconciliation fails") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-final-mempool-failure-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(76: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 1,
      totalSubtrees = 1,
      completed = true)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    val blockBase = invalidErgoFullBlockGen.sample.get
    val catchUpBlock = blockBase.copy(
      header = blockBase.header.copy(height = 1, parentId = snapshotId))
    val statusRemovals = new AtomicInteger(0)

    seedWalletRegistryVersions(bootstrapSettings, directory, Seq(snapshotId -> 0))
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(bestFullBlockResult = {
        case 1 => Some(catchUpBlock)
        case _ => None
      }),
      sourceIdentity = Some(_ => Success(source)),
      bestHeaderId = Some {
        case 0 => Success(Some(snapshotId))
        case _ => Success(None)
      },
      snapshotFullHeight = Some(_ => 1),
      catchUpReady = Some(_ => true),
      offChainReconciliation = Some((state, _, fallback) => {
        if (state.getWalletHeight == 1) {
          throw new IllegalStateException("injected post-catch-up reconciliation failure")
        }
        fallback()
      }),
      statusRemoval = Some(state => {
        statusRemovals.incrementAndGet()
        state.storage.removeUtxoSnapshotScanStatus()
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        error should include("injected post-catch-up reconciliation failure")
        statusRemovals.get() shouldBe 0
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
    }
  }

  property("reject an ordinary catch-up block with the wrong height or parent before registry mutation") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val canonicalAncestor = ModifierId @@ Algos.encode(Array.fill(32)(77: Byte))
    val foreignParent = ModifierId @@ Algos.encode(Array.fill(32)(78: Byte))
    val wrongHeightBase = invalidErgoFullBlockGen.sample.get
    val wrongHeightBlock = wrongHeightBase.copy(
      header = wrongHeightBase.header.copy(height = 8, parentId = canonicalAncestor))
    val wrongParentBase = invalidErgoFullBlockGen.sample.get
    val wrongParentBlock = wrongParentBase.copy(
      header = wrongParentBase.header.copy(height = 7, parentId = foreignParent))

    Seq(
      ("height", wrongHeightBlock, "requested height"),
      ("parent", wrongParentBlock, "parent")
    ).foreach { case (label, returnedBlock, expectedError) =>
      withClue(s"$label mutant: ") {
        val directory = Files.createTempDirectory(s"wallet-catchup-$label-mutant-").toFile
        val stateReader = snapshotRecoveryStateReader(
          ordinarySettings,
          returnedBlock.id,
          versionId = Some(returnedBlock.id),
          stateHeight = 7)
        seedWalletRegistryVersions(
          ordinarySettings, directory, Seq(canonicalAncestor -> 6))
        withSeededWalletStorage(ordinarySettings, directory)(
          _.updateStateContext(stateReader.stateContext).get)

        withProbeWalletActor(
          ordinarySettings,
          directory,
          strictHistoryReader(bestFullBlockResult = {
            case 7 => Some(returnedBlock)
            case _ => None
          }),
          currentStateTip = Some(_ => Success(7 -> Some(returnedBlock.id))),
          bestHeaderId = Some {
            case 6 => Success(Some(canonicalAncestor))
            case 7 => Success(Some(returnedBlock.id))
            case _ => Success(None)
          }) { (actor, scanner, client) =>
          client.send(actor, ChangedState(stateReader))
          scanner.expectNoMessage(300.millis)
          client.awaitAssert({
            client.send(actor, GetWalletStatus)
            client.expectMsgType[WalletStatus].error.get.toLowerCase should include(expectedError)
          }, 5.seconds, 100.millis)
          client.send(actor, ReadBalances(ChainStatus.OnChain))
          client.expectMsgType[Status.Failure]
        }

        val registry = WalletRegistry(ordinarySettings.copy(directory = directory.getAbsolutePath)).get
        try {
          registry.lastVersionId shouldBe Some(canonicalAncestor)
          registry.fetchDigest().height shouldBe 6
        } finally registry.close()
      }
    }
  }

  property("reject a snapshot catch-up block with a foreign parent before registry mutation") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-catchup-parent-mutant-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(79: Byte))
    val foreignParent = ModifierId @@ Algos.encode(Array.fill(32)(80: Byte))
    val status = snapshotStatus(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      nextSubtreeIndex = 1,
      totalSubtrees = 1,
      completed = true)
    val source = UtxoSnapshotSourceIdentity(
      status.snapshotHeight, status.snapshotBlockId, status.manifestDepth, status.totalSubtrees)
    val blockBase = invalidErgoFullBlockGen.sample.get
    val foreignBlock = blockBase.copy(
      header = blockBase.header.copy(height = 1, parentId = foreignParent))
    val statusRemovals = new AtomicInteger(0)

    seedWalletRegistryVersions(bootstrapSettings, directory, Seq(snapshotId -> 0))
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(bestFullBlockResult = {
        case 1 => Some(foreignBlock)
        case _ => None
      }),
      sourceIdentity = Some(_ => Success(source)),
      bestHeaderId = Some {
        case 0 => Success(Some(snapshotId))
        case _ => Success(None)
      },
      snapshotFullHeight = Some(_ => 1),
      catchUpReady = Some(_ => true),
      statusRemoval = Some(state => {
        statusRemovals.incrementAndGet()
        state.storage.removeUtxoSnapshotScanStatus()
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        error should include("parent")
        error should include(snapshotId.toString.toLowerCase)
      }, 5.seconds, 100.millis)
      statusRemovals.get() shouldBe 0
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    val registry = WalletRegistry(bootstrapSettings.copy(directory = directory.getAbsolutePath)).get
    try {
      registry.lastVersionId shouldBe Some(snapshotId)
      registry.fetchDigest().height shouldBe 0
    } finally registry.close()
    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
    }
  }

  property("quarantine completed finalization when its first catch-up block is unavailable") {
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
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        error should include("pruning horizon")
        error should include("resync from genesis")
      }, 5.seconds, 100.millis)
      catchUpScans.get() shouldBe 0
      statusRemovals.get() shouldBe 0
      sourceRemovals.get() shouldBe 0
      val abort = scanner.expectMsgType[AbortUtxoSnapshotScan](5.seconds)
      abort.run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId) shouldBe true

      catchUpAvailable.set(true)
      actor.tell(ChangedState(stateReader), ActorRef.noSender)
      actor.tell(ChangedState(stateReader), ActorRef.noSender)

      client.awaitAssert({
        catchUpScans.get() shouldBe 0
        statusRemovals.get() shouldBe 0
        sourceRemovals.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get.toLowerCase should include("pruning horizon")
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
      scanner.expectNoMessage(300.millis)
    }
    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(status)
    }
  }

  property("restart blocked snapshot catch-up in producer order using its retained canonical tip") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-blocked-reorg-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(74: Byte))
    val oldHeightOneBase = invalidErgoFullBlockGen.sample.get
    val oldHeightOneBlock = oldHeightOneBase.copy(
      header = oldHeightOneBase.header.copy(height = 1, parentId = snapshotId))
    val oldHeightOneId = oldHeightOneBlock.id
    val oldHeightTwoId = ModifierId @@ Algos.encode(Array.fill(32)(126: Byte))
    val newHeightOneBase = invalidErgoFullBlockGen.sample.get
    val newHeightOneBlock = newHeightOneBase.copy(
      header = newHeightOneBase.header.copy(height = 1, parentId = snapshotId))
    val newHeightOneId = newHeightOneBlock.id
    val newHeightTwoBase = invalidErgoFullBlockGen.sample.get
    val newHeightTwoBlock = newHeightTwoBase.copy(
      header = newHeightTwoBase.header.copy(height = 2, parentId = newHeightOneId))
    val newHeightTwoId = newHeightTwoBlock.id
    val source = UtxoSnapshotSourceIdentity(
      snapshotHeight = 0,
      snapshotBlockId = snapshotId,
      manifestDepth = ManifestSerializer.MainnetManifestDepth.toInt,
      partCount = 1)
    val newFork = new AtomicBoolean(false)
    val newHeightTwoReady = new AtomicBoolean(false)
    val stateTipUpdated = new AtomicBoolean(false)
    val actorRef = new AtomicReference[ActorRef]()
    val clientRef = new AtomicReference[ActorRef]()
    val readinessChecks = new AtomicInteger(0)
    val statusRemovals = new AtomicInteger(0)
    val sourceRemovals = new AtomicInteger(0)
    val rollbackAttempts = new AtomicInteger(0)
    val rollbackIntentWrites = new AtomicInteger(0)
    val catchUpCalls = scala.collection.mutable.ArrayBuffer.empty[(String, Int)]
    def observedCatchUpCalls: Seq[(String, Int)] =
      catchUpCalls.synchronized(catchUpCalls.toSeq)
    def updateAt(state: ErgoWalletState, blockId: ModifierId, height: Int): Try[ErgoWalletState] =
      state.registry.updateOnBlock(
        ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty),
        blockId,
        height).map(_ => state)
    val currentStateContext = snapshotRecoveryStateReader(
      bootstrapSettings,
      newHeightTwoId,
      versionId = Some(newHeightTwoId),
      stateHeight = 2).stateContext
    val currentState = Proxy.newProxyInstance(
      classOf[ErgoStateReader].getClassLoader,
      Array(classOf[ErgoStateReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "stateContext" => currentStateContext
            case "toString" => "blocked-catch-up-state-reader"
            case "hashCode" => Int.box(System.identityHashCode(proxy))
            case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq args(0))
            case other => throw new UnsupportedOperationException(s"Unexpected state read: $other")
          }
      }).asInstanceOf[ErgoStateReader]

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(
        heightLookup = id => if (id == snapshotId) Some(0) else None,
        bestFullBlockResult = {
          case 1 if !newFork.get() => Some(oldHeightOneBlock)
          case _ => None
        },
        minFullBlockAvailable = 2),
      sourceIdentity = Some(_ => Success(source)),
      currentStateTip = Some(_ =>
        if (stateTipUpdated.get()) Success(2 -> Some(newHeightTwoId))
        else Success(2 -> Some(oldHeightTwoId))),
      bestHeaderId = Some {
        case 0 => Success(Some(snapshotId))
        case 1 => Success(Some(if (newFork.get()) newHeightOneId else oldHeightOneId))
        case 2 => Success(Some(if (newFork.get()) newHeightTwoId else oldHeightTwoId))
        case _ => Success(None)
      },
      snapshotFullHeight = Some(_ => 2),
      catchUpReady = Some(_ => {
        if (readinessChecks.incrementAndGet() == 3) {
          actorRef.get().tell(ReadBalances(ChainStatus.OnChain), clientRef.get())
        }
        true
      }),
      catchUpScan = Some((state, height) => {
        val fork = if (newFork.get()) "new" else "old"
        catchUpCalls.synchronized(catchUpCalls += fork -> height)
        (fork, height) match {
          case ("old", 1) => updateAt(state, oldHeightOneId, height)
          case ("old", 2) => Failure(new IllegalStateException("old fork tip unavailable"))
          case ("new", 1) => updateAt(state, newHeightOneId, height)
          case ("new", 2) if newHeightTwoReady.get() => updateAt(state, newHeightTwoId, height)
          case ("new", 2) => Failure(new IllegalStateException("new fork tip unavailable"))
          case other => Failure(new IllegalStateException(s"unexpected catch-up request $other"))
        }
      }),
      registryRollback = Some((state, version) => {
        rollbackAttempts.incrementAndGet()
        state.registry.rollback(version)
      }),
      rollbackIntentWrite = Some((state, intent) => {
        rollbackIntentWrites.incrementAndGet()
        state.storage.writeWalletRollbackIntent(intent)
      }),
      statusRemoval = Some(state => {
        statusRemovals.incrementAndGet()
        state.storage.removeUtxoSnapshotScanStatus()
      }),
      sourceRemoval = Some(_ => {
        sourceRemovals.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      actorRef.set(actor)
      clientRef.set(client.ref)
      val oldRun = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        oldRun, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      client.send(actor, ApplyUtxoSnapshotScanBatch(
        oldRun, subtreeIndex = 0, nextSubtreeIndex = 1,
        completed = true, boxes = IndexedSeq.empty))
      val completedStatus = client.expectMsgType[Try[UtxoSnapshotScanStatus]].get

      client.awaitAssert({
        observedCatchUpCalls shouldBe Seq("old" -> 1, "old" -> 2)
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get should include("old fork tip unavailable")
      }, 5.seconds, 100.millis)
      statusRemovals.get() shouldBe 0
      sourceRemovals.get() shouldBe 0
      client.send(actor, GetWalletStatus)
      val oldForkFailure = client.expectMsgType[WalletStatus].error.get.toLowerCase
      oldForkFailure should not include "pruning"
      oldForkFailure should not include "re-bootstrap"
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
      client.send(actor, DeriveNextKey)
      client.expectMsgType[DeriveNextKeyResult].result.isFailure shouldBe true

      newFork.set(true)
      client.send(actor, Rollback(idToVersion(snapshotId)))
      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        rollbackIntentWrites.get() shouldBe 0
        statusRemovals.get() shouldBe 0
        sourceRemovals.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        val preparation = client.expectMsgType[WalletStatus].error.get.toLowerCase
        preparation should include("preparation")
        preparation should include("waiting")
      }, 5.seconds, 100.millis)
      scanner.expectNoMessage(300.millis)

      client.send(actor, ScanOnChain(newHeightOneBlock))
      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        rollbackIntentWrites.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        val preparation = client.expectMsgType[WalletStatus].error.get.toLowerCase
        preparation should include("preparation")
        preparation should include("waiting")
      }, 5.seconds, 100.millis)
      scanner.expectNoMessage(300.millis)

      client.send(actor, ScanOnChain(newHeightTwoBlock))
      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        rollbackIntentWrites.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        val preparation = client.expectMsgType[WalletStatus].error.get.toLowerCase
        preparation should include("preparation")
        preparation should include("waiting")
      }, 5.seconds, 100.millis)
      scanner.expectNoMessage(300.millis)

      stateTipUpdated.set(true)
      client.send(actor, ChangedState(currentState))
      client.awaitAssert({
        rollbackAttempts.get() shouldBe 1
        rollbackIntentWrites.get() shouldBe 1
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get.toLowerCase should include("pending")
      }, 5.seconds, 100.millis)

      client.send(actor, ChangedMempool(new FakeMempool(Seq.empty)))
      scanner.expectMsg(AbortUtxoSnapshotScan(oldRun))
      client.awaitAssert({
        observedCatchUpCalls shouldBe
          Seq("old" -> 1, "old" -> 2, "new" -> 1, "new" -> 2)
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get should include("new fork tip unavailable")
      }, 5.seconds, 100.millis)
      client.send(actor, GetWalletStatus)
      val newForkFailure = client.expectMsgType[WalletStatus].error.get.toLowerCase
      newForkFailure should not include "pruning"
      newForkFailure should not include "re-bootstrap"
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
      client.send(actor, DeriveNextKey)
      client.expectMsgType[DeriveNextKeyResult].result.isFailure shouldBe true

      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        oldRun, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].isFailure shouldBe true

      newHeightTwoReady.set(true)
      client.send(actor, ChangedState(currentState))
      client.expectMsgType[Status.Failure]
      client.send(actor, ContinueUtxoSnapshotCatchUp(oldRun, blockHeight = 1))
      client.send(actor, FinalizeUtxoSnapshotScan(oldRun, completedStatus, cleanupAttempt = 1))
      client.awaitAssert({
        observedCatchUpCalls shouldBe
          Seq("old" -> 1, "old" -> 2, "new" -> 1, "new" -> 2, "new" -> 2)
        statusRemovals.get() shouldBe 1
        sourceRemovals.get() shouldBe 1
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest].height shouldBe 2
      readinessChecks.get() shouldBe 3
      scanner.expectNoMessage(300.millis)
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(snapshotOrigin(0, snapshotId))
    }
  }

  property("quarantine an incomplete producer-ordered snapshot rollback at batch close") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory =
      Files.createTempDirectory("wallet-snapshot-rollback-pruned-preflight-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(127: Byte))
    val oldHeightOneBase = invalidErgoFullBlockGen.sample.get
    val oldHeightOneBlock = oldHeightOneBase.copy(
      header = oldHeightOneBase.header.copy(height = 1, parentId = snapshotId))
    val newHeightOneBase = invalidErgoFullBlockGen.sample.get
    val newHeightOneBlock = newHeightOneBase.copy(
      header = newHeightOneBase.header.copy(height = 1, parentId = snapshotId))
    val newHeightTwoBase = invalidErgoFullBlockGen.sample.get
    val newHeightTwoBlock = newHeightTwoBase.copy(
      header = newHeightTwoBase.header.copy(height = 2, parentId = newHeightOneBlock.id))
    val reorgStarted = new AtomicBoolean(false)
    val rollbackAttempts = new AtomicInteger(0)
    val statusRemovals = new AtomicInteger(0)
    val sourceRemovals = new AtomicInteger(0)
    val catchUpCalls = scala.collection.mutable.ArrayBuffer.empty[Int]

    def observedCatchUpCalls: Seq[Int] =
      catchUpCalls.synchronized(catchUpCalls.toSeq)

    def updateAt(
      state: ErgoWalletState,
      blockId: ModifierId,
      height: Int): Try[ErgoWalletState] =
      state.registry
        .updateOnBlock(
          ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty),
          blockId,
          height)
        .map(_ => state)

    val currentState = snapshotRecoveryStateReader(
      bootstrapSettings,
      newHeightTwoBlock.id,
      versionId = Some(newHeightTwoBlock.id),
      stateHeight = 2)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(
        heightLookup = {
          case `snapshotId` => Some(0)
          case _ => None
        },
        bestFullBlockResult = {
          case 1 if !reorgStarted.get() => Some(oldHeightOneBlock)
          case _ => None
        },
        minFullBlockAvailable = 3),
      currentStateTip = Some(_ => Success(2 -> Some(newHeightTwoBlock.id))),
      bestHeaderId = Some {
        case 0 => Success(Some(snapshotId))
        case 1 => Success(Some(newHeightOneBlock.id))
        case 2 => Success(Some(newHeightTwoBlock.id))
        case _ => Success(None)
      },
      snapshotFullHeight = Some(_ => 2),
      catchUpReady = Some(_ => true),
      catchUpScan = Some((state, height) => {
        catchUpCalls.synchronized(catchUpCalls += height)
        height match {
          case 1 => updateAt(state, oldHeightOneBlock.id, height)
          case 2 => Failure(new IllegalStateException("injected old-fork tip failure"))
          case other => Failure(new IllegalStateException(s"unexpected catch-up height $other"))
        }
      }),
      registryRollback = Some((_, _) => {
        rollbackAttempts.incrementAndGet()
        Failure(new IllegalStateException(
          "registry rollback must not run when required evidence was pruned"))
      }),
      statusRemoval = Some(state => {
        statusRemovals.incrementAndGet()
        state.storage.removeUtxoSnapshotScanStatus()
      }),
      sourceRemoval = Some(_ => {
        sourceRemovals.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get

      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 1,
        completed = true, boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get.completed shouldBe true

      client.awaitAssert({
        observedCatchUpCalls shouldBe Seq(1, 2)
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get should include("injected old-fork tip failure")
      }, 5.seconds, 100.millis)

      reorgStarted.set(true)
      client.send(actor, Rollback(idToVersion(snapshotId)))
      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        statusRemovals.get() shouldBe 0
        sourceRemovals.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        error should include("preparation is waiting")
        error should not include "pruning horizon"
      }, 5.seconds, 100.millis)
      scanner.expectNoMessage(300.millis)

      client.send(actor, ScanOnChain(newHeightOneBlock))
      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get.toLowerCase should
          include("preparation is waiting")
      }, 5.seconds, 100.millis)

      client.send(actor, ChangedState(currentState))
      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        observedCatchUpCalls shouldBe Seq(1, 2)
        statusRemovals.get() shouldBe 0
        sourceRemovals.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get.toLowerCase should include("pruning horizon")
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
      scanner.expectNoMessage(300.millis)
    }

    val registry =
      WalletRegistry(bootstrapSettings.copy(directory = directory.getAbsolutePath)).get
    try {
      registry.lastVersionId shouldBe Some(oldHeightOneBlock.id)
      registry.fetchDigest().height shouldBe 1
    } finally registry.close()

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe None
      storage.readUtxoSnapshotScanStatusTry().get.map(_.completed) shouldBe Some(true)
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe
        Some(snapshotOrigin(0, snapshotId))
    }
  }

  property("reconcile a completed snapshot rollback whose branch point is ahead of the wallet") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory =
      Files.createTempDirectory("wallet-snapshot-rollback-ahead-of-wallet-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(123: Byte))
    val branchBase = invalidErgoFullBlockGen.sample.get
    val branchBlock = branchBase.copy(
      header = branchBase.header.copy(height = 1, parentId = snapshotId))
    val tipBase = invalidErgoFullBlockGen.sample.get
    val tipBlock = tipBase.copy(
      header = tipBase.header.copy(height = 2, parentId = branchBlock.id))
    val retryCatchUp = new AtomicBoolean(false)
    val rollbackAttempts = new AtomicInteger(0)
    val catchUpCalls = scala.collection.mutable.ArrayBuffer.empty[Int]

    def observedCatchUpCalls: Seq[Int] =
      catchUpCalls.synchronized(catchUpCalls.toSeq)

    def updateAt(
      state: ErgoWalletState,
      blockId: ModifierId,
      height: Int): Try[ErgoWalletState] =
      state.registry
        .updateOnBlock(
          ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty),
          blockId,
          height)
        .map(_ => state)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(
        heightLookup = {
          case `snapshotId` => Some(0)
          case id if id == branchBlock.id => Some(1)
          case id if id == tipBlock.id => Some(2)
          case _ => None
        },
        bestFullBlockResult = {
          case 1 => Some(branchBlock)
          case 2 => Some(tipBlock)
          case _ => None
        }),
      currentStateTip = Some(_ => Success(2 -> Some(tipBlock.id))),
      bestHeaderId = Some {
        case 0 => Success(Some(snapshotId))
        case 1 => Success(Some(branchBlock.id))
        case 2 => Success(Some(tipBlock.id))
        case _ => Success(None)
      },
      snapshotFullHeight = Some(_ => 2),
      catchUpScan = Some((state, height) => {
        catchUpCalls.synchronized(catchUpCalls += height)
        height match {
          case 1 if !retryCatchUp.get() =>
            Failure(new IllegalStateException("injected initial catch-up failure"))
          case 1 =>
            updateAt(state, branchBlock.id, height)
          case 2 =>
            updateAt(state, tipBlock.id, height)
          case other =>
            Failure(new IllegalStateException(s"unexpected catch-up height $other"))
        }
      }),
      registryRollback = Some((_, _) => {
        rollbackAttempts.incrementAndGet()
        Failure(new IllegalStateException(
          "registry rollback must not run while the wallet is below the branch point"))
      })) { (actor, scanner, client) =>
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get

      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 1,
        completed = true, boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get.completed shouldBe true

      client.awaitAssert({
        observedCatchUpCalls shouldBe Seq(1)
        client.send(actor, GetWalletStatus)
        client
          .expectMsgType[WalletStatus]
          .error
          .get should include("injected initial catch-up failure")
      }, 5.seconds, 100.millis)

      retryCatchUp.set(true)
      client.send(actor, Rollback(idToVersion(branchBlock.id)))

      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        client
          .expectMsgType[WalletStatus]
          .error
          .get
          .toLowerCase should include("pending")
      }, 5.seconds, 100.millis)

      client.send(actor, ChangedMempool(new FakeMempool(Seq.empty)))
      scanner.expectMsg(5.seconds, AbortUtxoSnapshotScan(run))

      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        observedCatchUpCalls shouldBe Seq(1, 1, 2)
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
        client.send(actor, ReadBalances(ChainStatus.OnChain))
        client.expectMsgType[WalletDigest].height shouldBe 2
      }, 5.seconds, 100.millis)
    }

    val registry =
      WalletRegistry(bootstrapSettings.copy(directory = directory.getAbsolutePath)).get
    try {
      registry.lastVersionId shouldBe Some(tipBlock.id)
      registry.fetchDigest().height shouldBe 2
    } finally registry.close()

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe None
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe
        Some(snapshotOrigin(0, snapshotId))
    }
  }

  property("reject a producer-ordered non-canonical retained block during snapshot rollback preflight") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory =
      Files.createTempDirectory("wallet-snapshot-rollback-retained-noncanonical-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(124: Byte))
    val blockOneBase = invalidErgoFullBlockGen.sample.get
    val blockOne = blockOneBase.copy(
      header = blockOneBase.header.copy(height = 1, parentId = snapshotId))
    val retainedBase = invalidErgoFullBlockGen.sample.get
    val retainedBlock = retainedBase.copy(
      header = retainedBase.header.copy(height = 2, parentId = blockOne.id))
    val canonicalSiblingId =
      ModifierId @@ Algos.encode(Array.fill(32)(125: Byte))
    val blockThreeBase = invalidErgoFullBlockGen.sample.get
    val blockThree = blockThreeBase.copy(
      header = blockThreeBase.header.copy(height = 3, parentId = retainedBlock.id))
    val rollbackAttempts = new AtomicInteger(0)
    val catchUpCalls = scala.collection.mutable.ArrayBuffer.empty[Int]

    def observedCatchUpCalls: Seq[Int] =
      catchUpCalls.synchronized(catchUpCalls.toSeq)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(
        heightLookup = {
          case `snapshotId` => Some(0)
          case id if id == blockOne.id => Some(1)
          case _ => None
        },
        bestFullBlockResult = {
          case 1 => Some(blockOne)
          case 3 => Some(blockThree)
          case _ => None
        },
        minFullBlockAvailable = 0),
      currentStateTip = Some(_ => Success(3 -> Some(blockThree.id))),
      bestHeaderId = Some {
        case 0 => Success(Some(snapshotId))
        case 1 => Success(Some(blockOne.id))
        case 2 => Success(Some(canonicalSiblingId))
        case 3 => Success(Some(blockThree.id))
        case _ => Success(None)
      },
      snapshotFullHeight = Some(_ => 3),
      catchUpScan = Some((state, height) => {
        catchUpCalls.synchronized(catchUpCalls += height)
        height match {
          case 1 =>
            state.registry
              .updateOnBlock(
                ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty),
                blockOne.id,
                height)
              .map(_ => state)
          case 2 =>
            Failure(new IllegalStateException("injected intermediate catch-up failure"))
          case other =>
            Failure(new IllegalStateException(s"unexpected catch-up height $other"))
        }
      }),
      registryRollback = Some((_, _) => {
        rollbackAttempts.incrementAndGet()
        Failure(new IllegalStateException(
          "registry rollback must not run for a non-canonical retained block"))
      })) { (actor, scanner, client) =>
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get

      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 1,
        completed = true, boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get.completed shouldBe true

      client.awaitAssert({
        observedCatchUpCalls shouldBe Seq(1, 2)
        client.send(actor, GetWalletStatus)
        client
          .expectMsgType[WalletStatus]
          .error
          .get should include("injected intermediate catch-up failure")
      }, 5.seconds, 100.millis)

      client.send(actor, Rollback(idToVersion(snapshotId)))

      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        val preparation = client.expectMsgType[WalletStatus].error.get.toLowerCase
        preparation should include("preparation")
        preparation should include("waiting")
      }, 5.seconds, 100.millis)
      scanner.expectNoMessage(300.millis)

      client.send(actor, Rollback(idToVersion(blockOne.id)))
      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        val preparation = client.expectMsgType[WalletStatus].error.get.toLowerCase
        preparation should include("preparation")
        preparation should include("waiting")
        preparation should not include "superseded"
      }, 5.seconds, 100.millis)
      scanner.expectNoMessage(300.millis)

      client.send(actor, ScanOnChain(retainedBlock))
      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        error should include("retained wallet catch-up block")
        error should include("not canonical")
      }, 5.seconds, 100.millis)

      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
      scanner.expectNoMessage(300.millis)
    }

    val registry =
      WalletRegistry(bootstrapSettings.copy(directory = directory.getAbsolutePath)).get
    try {
      registry.lastVersionId shouldBe Some(blockOne.id)
      registry.fetchDigest().height shouldBe 1
    } finally registry.close()

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe None
      storage.readUtxoSnapshotScanStatusTry().get.map(_.completed) shouldBe Some(true)
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe
        Some(snapshotOrigin(0, snapshotId))
    }
  }

  property("wait for producer-ordered evidence before an ordinary wallet rollback") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-ordinary-rollback-producer-order-").toFile
    val staleTipId = ModifierId @@ Algos.encode(Array.fill(32)(126: Byte))
    val forkBase = invalidErgoFullBlockGen.sample.get
    val forkBlock = forkBase.copy(
      header = forkBase.header.copy(height = 1, parentId = PreGenesisHeader.id))
    val currentStateTip = new AtomicReference[(Int, Option[ModifierId])](
      1 -> Some(staleTipId))
    val bestHeaderTip = new AtomicReference[ModifierId](staleTipId)
    val rollbackCompleted = new AtomicBoolean(false)
    val rollbackAttempts = new AtomicInteger(0)
    val forkStateContext = snapshotRecoveryStateReader(
      ordinarySettings,
      forkBlock.id,
      versionId = Some(forkBlock.id),
      stateHeight = 1).stateContext
    val forkState = Proxy.newProxyInstance(
      classOf[ErgoStateReader].getClassLoader,
      Array(classOf[ErgoStateReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "stateContext" => forkStateContext
            case "toString" => "ordinary-rollback-state-reader"
            case "hashCode" => Int.box(System.identityHashCode(proxy))
            case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq args(0))
            case other => throw new UnsupportedOperationException(
              s"Unexpected state read: $other")
          }
      }).asInstanceOf[ErgoStateReader]

    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(minFullBlockAvailable = 0),
      registryTip = Some(_ => Success(
        if (rollbackCompleted.get()) 0 -> Some(PreGenesisHeader.id)
        else 1 -> Some(staleTipId))),
      currentStateTip = Some(_ => Success(currentStateTip.get())),
      bestHeaderId = Some {
        case 1 => Success(Some(bestHeaderTip.get()))
        case _ => Success(None)
      },
      registryRollback = Some((_, version) => {
        version shouldBe idToVersion(PreGenesisHeader.id)
        rollbackAttempts.incrementAndGet()
        rollbackCompleted.set(true)
        Success(())
    })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      bestHeaderTip.set(forkBlock.id)
      client.send(actor, Rollback(idToVersion(PreGenesisHeader.id)))

      client.awaitAssert({
        rollbackAttempts.get() shouldBe 0
        client.send(actor, GetWalletStatus)
        val preparation = client.expectMsgType[WalletStatus].error.get.toLowerCase
        preparation should include("preparation")
        preparation should include("waiting")
        preparation should not include "restart"
      }, 5.seconds, 100.millis)

      client.send(actor, ScanOnChain(forkBlock))
      rollbackAttempts.get() shouldBe 0
      currentStateTip.set(1 -> Some(forkBlock.id))
      client.send(actor, ChangedState(forkState))

      client.awaitAssert({
        rollbackAttempts.get() shouldBe 1
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get.toLowerCase should
          include("pending a fresh mempool")
      }, 5.seconds, 100.millis)

      client.send(actor, ChangedMempool(new FakeMempool(Seq.empty)))
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
      scanner.expectNoMessage(300.millis)
    }

    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe None
    }
  }

  property("quarantine blocked snapshot catch-up when registry rollback fails") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-blocked-rollback-failure-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(78: Byte))
    val oldHeightOneId = ModifierId @@ Algos.encode(Array.fill(32)(79: Byte))
    val rollbackAttempts = new AtomicInteger(0)
    val catchUpCalls = new AtomicInteger(0)
    var completedStatus: Option[UtxoSnapshotScanStatus] = None

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(heightLookup = id => if (id == snapshotId) Some(0) else None),
      currentStateTip = Some(_ => Success(0 -> Some(snapshotId))),
      bestHeaderId = Some {
        case 0 => Success(Some(snapshotId))
        case _ => Success(None)
      },
      snapshotFullHeight = Some(_ => 2),
      catchUpReady = Some(_ => true),
      catchUpScan = Some((state, height) => {
        catchUpCalls.incrementAndGet()
        if (height == 1) {
          state.registry.updateOnBlock(
            ScanResults(ArraySeq.empty, ArraySeq.empty, ArraySeq.empty),
            oldHeightOneId,
            height).map(_ => state)
        } else {
          Failure(new IllegalStateException("catch-up blocked before rollback"))
        }
      }),
      registryRollback = Some((_, version) => {
        version shouldBe idToVersion(snapshotId)
        rollbackAttempts.incrementAndGet()
        Failure(new IllegalStateException("injected registry rollback failure"))
      })) { (actor, scanner, client) =>
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 1,
        completed = true, boxes = IndexedSeq.empty))
      completedStatus = Some(client.expectMsgType[Try[UtxoSnapshotScanStatus]].get)

      client.awaitAssert({
        catchUpCalls.get() shouldBe 2
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get should include("catch-up blocked before rollback")
      }, 5.seconds, 100.millis)

      client.send(actor, Rollback(idToVersion(snapshotId)))
      scanner.expectMsg(AbortUtxoSnapshotScan(run))
      rollbackAttempts.get() shouldBe 1
      client.send(actor, GetWalletStatus)
      val quarantineError = client.expectMsgType[WalletStatus].error.get.toLowerCase
      quarantineError should include("quarantine")
      quarantineError should include("injected registry rollback failure")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
      storage.readUtxoSnapshotScanStatusTry().get shouldBe completedStatus
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe Some(snapshotOrigin(0, snapshotId))
      storage.readWalletRollbackIntentTry().get shouldBe
        Some(WalletRollbackIntent(snapshotId, expectedHeight = 0))
    }
  }

  property("fail closed when ordinary rollback succeeds but off-chain reconciliation fails") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-rollback-reconciliation-failure-").toFile
    val registryTip = ModifierId @@ Algos.encode(Array.fill(32)(116: Byte))
    seedNonemptyWalletRegistry(ordinarySettings, directory, registryTip)
    val failNextReconciliation = new AtomicBoolean(false)
    val reconciliationAttempts = new AtomicInteger(0)
    val currentStateTip = new AtomicReference[(Int, Option[ModifierId])](
      7 -> Some(registryTip))

    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(),
      currentStateTip = Some(_ => Success(currentStateTip.get())),
      bestHeaderId = Some {
        case 7 => Success(Some(registryTip))
        case _ => Success(None)
      },
      offChainReconciliation = Some((state, _, fallback) => {
        if (failNextReconciliation.compareAndSet(true, false)) {
          reconciliationAttempts.incrementAndGet()
          state.registry.lastVersionId shouldBe Some(PreGenesisHeader.id)
          throw new IllegalStateException("injected post-rollback off-chain reconciliation failure")
        }
        fallback()
    })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      currentStateTip.set(0 -> None)
      client.send(actor, ChangedState(genericStateReader(
        ErgoStateContext.empty(ordinarySettings.chainSettings, parameters))))
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
      failNextReconciliation.set(true)
      client.send(actor, Rollback(idToVersion(PreGenesisHeader.id)))
      client.send(actor, GetWalletStatus)
      val waitingError = client.expectMsgType[WalletStatus].error.get
      reconciliationAttempts.get() shouldBe 0
      waitingError.toLowerCase should include("pending")

      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
      client.send(actor, DeriveNextKey)
      client.expectMsgType[DeriveNextKeyResult].result.isFailure shouldBe true

      client.send(actor, ChangedMempool(new FakeMempool(Seq.empty)))
      client.send(actor, GetWalletStatus)
      val rollbackError = client.expectMsgType[WalletStatus].error.get
      reconciliationAttempts.get() shouldBe 1
      rollbackError should include("injected post-rollback off-chain reconciliation failure")
      client.send(actor, ReadBalances(ChainStatus.OffChain))
      client.expectMsgType[Status.Failure]

      client.send(actor, ChangedMempool(new FakeMempool(Seq.empty)))
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      val confirmed = client.expectMsgType[WalletDigest]
      client.send(actor, ReadBalances(ChainStatus.OffChain))
      client.expectMsgType[WalletDigest] shouldBe confirmed
    }

    val reopened = WalletRegistry(ordinarySettings.copy(directory = directory.getAbsolutePath)).get
    try reopened.lastVersionId shouldBe Some(PreGenesisHeader.id)
    finally reopened.close()
    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe None
    }
  }

  property("resume a durable rollback after restart without rolling the registry back twice") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-durable-rollback-restart-").toFile
    val originalTip = ModifierId @@ Algos.encode(Array.fill(32)(117: Byte))
    val expectedIntent = WalletRollbackIntent(PreGenesisHeader.id, expectedHeight = 0)
    val firstRollbackAttempts = new AtomicInteger(0)
    val currentStateTip = new AtomicReference[(Int, Option[ModifierId])](
      7 -> Some(originalTip))

    seedNonemptyWalletRegistry(ordinarySettings, directory, originalTip)
    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(),
      currentStateTip = Some(_ => Success(currentStateTip.get())),
      bestHeaderId = Some {
        case 7 => Success(Some(originalTip))
        case _ => Success(None)
      },
      registryRollback = Some((state, version) => {
        state.storage.readWalletRollbackIntentTry().get shouldBe Some(expectedIntent)
        firstRollbackAttempts.incrementAndGet()
        state.registry.rollback(version)
    })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      currentStateTip.set(0 -> None)
      client.send(actor, ChangedState(genericStateReader(
        ErgoStateContext.empty(ordinarySettings.chainSettings, parameters))))
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
      client.send(actor, Rollback(idToVersion(PreGenesisHeader.id)))
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get.toLowerCase should include("pending")
      firstRollbackAttempts.get() shouldBe 1
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe Some(expectedIntent)
    }

    val restartedRollbackAttempts = new AtomicInteger(0)
    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(),
      registryRollback = Some((_, _) => {
        restartedRollbackAttempts.incrementAndGet()
        Failure(new IllegalStateException("restart must not repeat an exact rollback"))
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
        restartedRollbackAttempts.get() shouldBe 0
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest]
    }

    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe None
    }
  }

  property("recover a completed durable rollback whose target became non-canonical while stopped") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-stale-rollback-intent-").toFile
    val canonicalAncestor = ModifierId @@ Algos.encode(Array.fill(32)(121: Byte))
    val staleTarget = ModifierId @@ Algos.encode(Array.fill(32)(122: Byte))
    val currentBestBase = invalidErgoFullBlockGen.sample.get
    val currentBestBlock = currentBestBase.copy(
      header = currentBestBase.header.copy(height = 7, parentId = canonicalAncestor))
    val currentBest = currentBestBlock.id
    val staleIntent = WalletRollbackIntent(staleTarget, expectedHeight = 7)
    val replacementIntent = WalletRollbackIntent(canonicalAncestor, expectedHeight = 6)
    val rollbackAttempts = new AtomicInteger(0)
    val reconciliationAttempts = new AtomicInteger(0)
    val catchUpHeight = new AtomicInteger(-1)

    seedWalletRegistryVersions(
      ordinarySettings,
      directory,
      Seq(canonicalAncestor -> 6, staleTarget -> 7))
    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.writeWalletRollbackIntent(staleIntent).get
    }

    val history = strictHistoryReader(
      heightLookup = {
        case `canonicalAncestor` => Some(6)
        case `staleTarget` => Some(7)
        case _ => None
      },
      bestFullBlockRead = catchUpHeight.set,
      bestFullBlockResult = {
        case 7 => Some(currentBestBlock)
        case _ => None
      })
    val catchUpStateContext = snapshotRecoveryStateReader(
      ordinarySettings, currentBest, versionId = Some(currentBest), stateHeight = 7).stateContext
    val catchUpStateReader = Proxy.newProxyInstance(
      classOf[ErgoStateReader].getClassLoader,
      Array(classOf[ErgoStateReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "stateContext" => catchUpStateContext
            case "toString" => "rollback-catchup-state-reader"
            case "hashCode" => Int.box(System.identityHashCode(proxy))
            case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq args(0))
            case other => throw new UnsupportedOperationException(s"Unexpected state read: $other")
          }
      }).asInstanceOf[ErgoStateReader]
    withProbeWalletActor(
      ordinarySettings,
      directory,
      history,
      currentStateTip = Some(_ => Success(7 -> Some(currentBest))),
      bestHeaderId = Some {
        case 6 => Success(Some(canonicalAncestor))
        case 7 => Success(Some(currentBest))
        case _ => Success(None)
      },
      registryRollback = Some((state, version) => {
        version shouldBe idToVersion(canonicalAncestor)
        state.storage.readWalletRollbackIntentTry().get shouldBe Some(replacementIntent)
        rollbackAttempts.incrementAndGet()
        state.registry.rollback(version)
      }),
      offChainReconciliation = Some((state, _, fallback) => {
        val expectedTip = reconciliationAttempts.getAndIncrement() match {
          case 0 => canonicalAncestor
          case 1 => currentBest
          case attempt => fail(s"Unexpected rollback reconciliation attempt $attempt")
        }
        state.registry.lastVersionId shouldBe Some(expectedTip)
        fallback()
      }),
      startupStateReader = Some(catchUpStateReader)) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
        rollbackAttempts.get() shouldBe 1
        reconciliationAttempts.get() shouldBe 2
        catchUpHeight.get() shouldBe 7
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest].height shouldBe 7
    }

    val reopened = WalletRegistry(ordinarySettings.copy(directory = directory.getAbsolutePath)).get
    try reopened.lastVersionId shouldBe Some(currentBest)
    finally reopened.close()
    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe None
    }
  }

  property("recover a same-height orphan wallet tip without a durable intent after restart") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-no-intent-orphan-restart-").toFile
    val canonicalAncestor = ModifierId @@ Algos.encode(Array.fill(32)(81: Byte))
    val canonicalTipBase = invalidErgoFullBlockGen.sample.get
    val canonicalTipBlock = canonicalTipBase.copy(
      header = canonicalTipBase.header.copy(height = 7, parentId = canonicalAncestor))
    val canonicalTip = canonicalTipBlock.id
    val orphanHeader = canonicalTipBlock.header.copy(
      timestamp = canonicalTipBlock.header.timestamp + 1L)
    val orphanTip = orphanHeader.id
    val staleIntent = WalletRollbackIntent(orphanTip, expectedHeight = 7)
    val replacementIntent = WalletRollbackIntent(canonicalAncestor, expectedHeight = 6)
    val rollbackAttempts = new AtomicInteger(0)
    val restartedRollbackAttempts = new AtomicInteger(0)

    seedWalletRegistryVersions(
      ordinarySettings,
      directory,
      Seq(canonicalAncestor -> 6, orphanTip -> 7))
    orphanTip should not be canonicalTip
    val orphanStateContext = new ErgoStateContext(
      Seq(orphanHeader),
      None,
      startDigest,
      parameters,
      validationSettingsNoIl,
      VotingData.empty)(ordinarySettings.chainSettings)
    val canonicalStateContext = new ErgoStateContext(
      Seq(canonicalTipBlock.header),
      None,
      startDigest,
      parameters,
      validationSettingsNoIl,
      VotingData.empty)(ordinarySettings.chainSettings)
    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.updateStateContext(orphanStateContext).get
      storage.readWalletRollbackIntentTry().get shouldBe None
    }

    val history = strictHistoryReader(
      heightLookup = {
        case `canonicalAncestor` => Some(6)
        case `orphanTip` => Some(7)
        case `canonicalTip` => Some(7)
        case _ => None
      },
      bestFullBlockResult = {
        case 7 => Some(canonicalTipBlock)
        case _ => None
      })
    val canonicalStateReader = Proxy.newProxyInstance(
      classOf[ErgoStateReader].getClassLoader,
      Array(classOf[ErgoStateReader]),
      new InvocationHandler {
        override def invoke(proxy: Any, method: Method, args: Array[AnyRef]): AnyRef =
          method.getName match {
            case "stateContext" => canonicalStateContext
            case "toString" => "no-intent-canonical-state-reader"
            case "hashCode" => Int.box(System.identityHashCode(proxy))
            case "equals" => Boolean.box(proxy.asInstanceOf[AnyRef] eq args(0))
            case other => throw new UnsupportedOperationException(
              s"Unexpected state read: $other")
          }
      }).asInstanceOf[ErgoStateReader]

    withProbeWalletActor(
      ordinarySettings,
      directory,
      history,
      startupStateReader = Some(canonicalStateReader),
      bestHeaderId = Some {
        case 6 => Success(Some(canonicalAncestor))
        case 7 => Success(Some(canonicalTip))
        case _ => Success(None)
      },
      registryRollback = Some((state, version) => {
        version shouldBe idToVersion(canonicalAncestor)
        state.storage.readWalletRollbackIntentTry().get shouldBe Some(replacementIntent)
        rollbackAttempts.incrementAndGet()
        state.registry.rollback(version)
      }),
      rollbackIntentWrite = Some((state, intent) => {
        intent shouldBe staleIntent
        state.storage.writeWalletRollbackIntent(intent)
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
        rollbackAttempts.get() shouldBe 1
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest].height shouldBe 7
    }

    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe None
    }

    withProbeWalletActor(
      ordinarySettings,
      directory,
      history,
      bestHeaderId = Some {
        case 6 => Success(Some(canonicalAncestor))
        case 7 => Success(Some(canonicalTip))
        case _ => Success(None)
      },
      registryRollback = Some((_, _) => {
        restartedRollbackAttempts.incrementAndGet()
        Failure(new IllegalStateException("restart must not repeat the recovered rollback"))
      }),
      startupStateReader = Some(canonicalStateReader)) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
        restartedRollbackAttempts.get() shouldBe 0
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest].height shouldBe 7
    }

    val reopened = WalletRegistry(ordinarySettings.copy(directory = directory.getAbsolutePath)).get
    try reopened.lastVersionId shouldBe Some(canonicalTip)
    finally reopened.close()
    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe None
    }
  }

  property("retain a stale durable rollback intent when its catch-up block was pruned") {
    val prunedSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-stale-rollback-pruned-catch-up-").toFile
    val canonicalAncestor = ModifierId @@ Algos.encode(Array.fill(32)(84: Byte))
    val staleTarget = ModifierId @@ Algos.encode(Array.fill(32)(85: Byte))
    val currentBest = ModifierId @@ Algos.encode(Array.fill(32)(86: Byte))
    val staleIntent = WalletRollbackIntent(staleTarget, expectedHeight = 7)
    val rollbackAttempts = new AtomicInteger(0)
    val catchUpChecks = new AtomicInteger(0)
    val stateContext = snapshotRecoveryStateReader(
      prunedSettings, currentBest, versionId = Some(currentBest), stateHeight = 7).stateContext

    seedWalletRegistryVersions(
      prunedSettings,
      directory,
      Seq(canonicalAncestor -> 6, staleTarget -> 7))
    withSeededWalletStorage(prunedSettings, directory) { storage =>
      storage.writeWalletRollbackIntent(staleIntent).get
      storage.updateStateContext(stateContext).get
    }
    val intentBytes = readWalletStorageEntry(
      prunedSettings, directory, WalletStorage.WalletRollbackIntentKey).get

    withProbeWalletActor(
      prunedSettings,
      directory,
      strictHistoryReader(
        heightLookup = {
          case `canonicalAncestor` => Some(6)
          case `staleTarget` => Some(7)
          case _ => None
        },
        bestFullBlockRead = _ => catchUpChecks.incrementAndGet()),
      bestHeaderId = Some {
        case 6 => Success(Some(canonicalAncestor))
        case 7 => Success(Some(currentBest))
        case _ => Success(None)
      },
      registryRollback = Some((_, _) => {
        rollbackAttempts.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
      error should include("pruning horizon")
      error should include("resync from genesis")
      rollbackAttempts.get() shouldBe 0
      catchUpChecks.get() shouldBe 1
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    readWalletStorageEntry(
      prunedSettings,
      directory,
      WalletStorage.WalletRollbackIntentKey).get should contain theSameElementsInOrderAs intentBytes
  }

  property("retain a stale durable rollback intent when a later multiblock catch-up height was pruned") {
    val prunedSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-stale-rollback-multiblock-pruned-").toFile
    val canonicalAncestor = ModifierId @@ Algos.encode(Array.fill(32)(101: Byte))
    val staleTarget = ModifierId @@ Algos.encode(Array.fill(32)(102: Byte))
    val blockSevenBase = invalidErgoFullBlockGen.sample.get
    val blockSeven = blockSevenBase.copy(
      header = blockSevenBase.header.copy(height = 7, parentId = canonicalAncestor))
    val blockEightBase = invalidErgoFullBlockGen.sample.get
    val blockEight = blockEightBase.copy(
      header = blockEightBase.header.copy(height = 8, parentId = blockSeven.id))
    val currentBest = blockEight.id
    val staleIntent = WalletRollbackIntent(staleTarget, expectedHeight = 7)
    val rollbackAttempts = new AtomicInteger(0)
    val catchUpHeights = scala.collection.mutable.ArrayBuffer.empty[Int]
    def observedCatchUpHeights: Seq[Int] = catchUpHeights.synchronized(catchUpHeights.toSeq)
    val stateContext = snapshotRecoveryStateReader(
      prunedSettings, currentBest, versionId = Some(currentBest), stateHeight = 8).stateContext

    seedWalletRegistryVersions(
      prunedSettings,
      directory,
      Seq(canonicalAncestor -> 6, staleTarget -> 7))
    withSeededWalletStorage(prunedSettings, directory) { storage =>
      storage.writeWalletRollbackIntent(staleIntent).get
      storage.updateStateContext(stateContext).get
    }
    val intentBytes = readWalletStorageEntry(
      prunedSettings, directory, WalletStorage.WalletRollbackIntentKey).get

    withProbeWalletActor(
      prunedSettings,
      directory,
      strictHistoryReader(
        heightLookup = {
          case `canonicalAncestor` => Some(6)
          case `staleTarget` => Some(7)
          case _ => None
        },
        bestFullBlockRead = height => catchUpHeights.synchronized(catchUpHeights += height),
        bestFullBlockResult = {
          case 7 => Some(blockSeven)
          case _ => None
        }),
      currentStateTip = Some(_ => Success(8 -> Some(currentBest))),
      bestHeaderId = Some {
        case 6 => Success(Some(canonicalAncestor))
        case 7 => Success(Some(blockSeven.id))
        case 8 => Success(Some(currentBest))
        case _ => Success(None)
      },
      registryRollback = Some((_, _) => {
        rollbackAttempts.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
      error should include("pruning horizon")
      error should include("height 8")
      rollbackAttempts.get() shouldBe 0
      observedCatchUpHeights shouldBe Seq(7, 8)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
    }

    readWalletStorageEntry(
      prunedSettings,
      directory,
      WalletStorage.WalletRollbackIntentKey).get should contain theSameElementsInOrderAs intentBytes
    val reopened = WalletRegistry(prunedSettings.copy(directory = directory.getAbsolutePath)).get
    try reopened.lastVersionId shouldBe Some(staleTarget)
    finally reopened.close()
  }

  property("refuse rollback when the frozen state tip is no longer the best header") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-rollback-tip-reorg-").toFile
    val blockOneBase = invalidErgoFullBlockGen.sample.get
    val blockOne = blockOneBase.copy(
      header = blockOneBase.header.copy(height = 1, parentId = PreGenesisHeader.id))
    val competingTip = ModifierId @@ Algos.encode(Array.fill(32)(104: Byte))
    val rollbackAttempts = new AtomicInteger(0)

    seedWalletRegistryVersions(ordinarySettings, directory, Seq(blockOne.id -> 1))

    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(bestFullBlockResult = {
        case 1 => Some(blockOne)
        case _ => None
      }),
      currentStateTip = Some(_ => Success(1 -> Some(blockOne.id))),
      bestHeaderId = Some {
        case 1 => Success(Some(competingTip))
        case _ => Success(None)
      },
      registryRollback = Some((_, _) => {
        rollbackAttempts.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      client.send(actor, Rollback(idToVersion(PreGenesisHeader.id)))
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        error should include("state tip")
        error should include("best header")
        rollbackAttempts.get() shouldBe 0
      }, 5.seconds, 100.millis)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
      scanner.expectNoMessage(300.millis)
    }

    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe None
    }
    val reopened = WalletRegistry(ordinarySettings.copy(directory = directory.getAbsolutePath)).get
    try reopened.lastVersionId shouldBe Some(blockOne.id)
    finally reopened.close()
  }

  property("retain a stale durable rollback intent when the registry tip is not its exact target") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-stale-rollback-intent-mismatch-").toFile
    val canonicalAncestor = ModifierId @@ Algos.encode(Array.fill(32)(124: Byte))
    val staleTarget = ModifierId @@ Algos.encode(Array.fill(32)(125: Byte))
    val currentBest = ModifierId @@ Algos.encode(Array.fill(32)(126: Byte))
    val staleIntent = WalletRollbackIntent(staleTarget, expectedHeight = 7)
    val rollbackAttempts = new AtomicInteger(0)

    seedWalletRegistryVersions(ordinarySettings, directory, Seq(canonicalAncestor -> 6))
    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.writeWalletRollbackIntent(staleIntent).get
    }
    val intentBytes = readWalletStorageEntry(
      ordinarySettings, directory, WalletStorage.WalletRollbackIntentKey).get

    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(heightLookup = {
        case `canonicalAncestor` => Some(6)
        case `staleTarget` => Some(7)
        case _ => None
      }),
      bestHeaderId = Some {
        case 6 => Success(Some(canonicalAncestor))
        case 7 => Success(Some(currentBest))
        case _ => Success(None)
      },
      registryRollback = Some((_, _) => {
        rollbackAttempts.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
      error should include("indeterminate")
      error should include("restart")
      client.send(actor, ChangedMempool(new FakeMempool(Seq.empty)))
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
      rollbackAttempts.get() shouldBe 0
    }

    readWalletStorageEntry(
      ordinarySettings,
      directory,
      WalletStorage.WalletRollbackIntentKey).get should contain theSameElementsInOrderAs intentBytes
  }

  property("refuse stale retarget when target canonicality is indeterminate") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val cases = Seq[(String, Try[Option[ModifierId]], String)](
      ("history read failure",
        Failure(new IllegalStateException("injected best-header read failure")),
        "injected best-header read failure"),
      ("missing best header", Success(None), "no best header"))

    cases.zipWithIndex.foreach { case ((label, targetCanonicality, expectedError), index) =>
      val directory = Files.createTempDirectory(s"wallet-stale-retarget-indeterminate-$index-").toFile
      val canonicalAncestor = ModifierId @@ Algos.encode(Array.fill(32)((90 + index).toByte))
      val staleTarget = ModifierId @@ Algos.encode(Array.fill(32)((92 + index).toByte))
      val staleIntent = WalletRollbackIntent(staleTarget, expectedHeight = 7)
      val rollbackAttempts = new AtomicInteger(0)

      seedWalletRegistryVersions(
        ordinarySettings,
        directory,
        Seq(canonicalAncestor -> 6, staleTarget -> 7))
      withSeededWalletStorage(ordinarySettings, directory) { storage =>
        storage.writeWalletRollbackIntent(staleIntent).get
      }
      val intentBytes = readWalletStorageEntry(
        ordinarySettings, directory, WalletStorage.WalletRollbackIntentKey).get

      withProbeWalletActor(
        ordinarySettings,
        directory,
        strictHistoryReader(heightLookup = {
          case `canonicalAncestor` => Some(6)
          case `staleTarget` => Some(7)
          case _ => None
        }),
        bestHeaderId = Some {
          case 6 => Success(Some(canonicalAncestor))
          case 7 => targetCanonicality
          case _ => Success(None)
        },
        registryRollback = Some((_, _) => {
          rollbackAttempts.incrementAndGet()
          Success(())
        })) { (actor, scanner, client) =>
        scanner.expectNoMessage(300.millis)
        client.send(actor, GetWalletStatus)
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        withClue(label) {
          error should include("indeterminate")
          error should include("restart")
          error should include(expectedError)
          rollbackAttempts.get() shouldBe 0
        }
      }

      readWalletStorageEntry(
        ordinarySettings,
        directory,
        WalletStorage.WalletRollbackIntentKey).get should contain theSameElementsInOrderAs intentBytes
    }
  }

  property("resume ordinary catch-up after restart cleared a completed rollback intent") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = -1,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-cleared-rollback-catchup-").toFile
    val canonicalAncestor = ModifierId @@ Algos.encode(Array.fill(32)(96: Byte))
    val currentBestBase = invalidErgoFullBlockGen.sample.get
    val currentBestBlock = currentBestBase.copy(
      header = currentBestBase.header.copy(height = 7, parentId = canonicalAncestor))
    val currentBest = currentBestBlock.id
    val catchUpHeight = new AtomicInteger(-1)

    seedWalletRegistryVersions(
      ordinarySettings,
      directory,
      Seq(canonicalAncestor -> 6))
    val fullStateReader = snapshotRecoveryStateReader(
      ordinarySettings, currentBest, versionId = Some(currentBest), stateHeight = 7)
    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.updateStateContext(fullStateReader.stateContext).get
      storage.readWalletRollbackIntentTry().get shouldBe None
    }

    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(
        bestFullBlockRead = catchUpHeight.set,
        bestFullBlockResult = {
          case 7 => Some(currentBestBlock)
          case _ => None
        }),
      currentStateTip = Some(_ => Success(7 -> Some(currentBest))),
      bestHeaderId = Some {
        case 6 => Success(Some(canonicalAncestor))
        case 7 => Success(Some(currentBest))
        case _ => Success(None)
      },
      startupStateReader = Some(fullStateReader),
      utxoStateUpdate = Some((state, _) => state)) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert(catchUpHeight.get() shouldBe 7, 5.seconds, 100.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest].height shouldBe 7
    }
  }

  property("quarantine startup catch-up when the next wallet block was pruned") {
    val prunedSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-startup-pruned-catchup-").toFile
    val canonicalAncestor = ModifierId @@ Algos.encode(Array.fill(32)(87: Byte))
    val currentBest = ModifierId @@ Algos.encode(Array.fill(32)(88: Byte))
    val catchUpChecks = new AtomicInteger(0)
    val stateReader = snapshotRecoveryStateReader(
      prunedSettings, currentBest, versionId = Some(currentBest), stateHeight = 7)

    seedWalletRegistryVersions(prunedSettings, directory, Seq(canonicalAncestor -> 6))
    withSeededWalletStorage(prunedSettings, directory)(
      _.updateStateContext(stateReader.stateContext).get)

    withProbeWalletActor(
      prunedSettings,
      directory,
      strictHistoryReader(bestFullBlockRead = _ => catchUpChecks.incrementAndGet()),
      currentStateTip = Some(_ => Success(7 -> Some(currentBest))),
      bestHeaderId = Some {
        case 6 => Success(Some(canonicalAncestor))
        case 7 => Success(Some(currentBest))
        case _ => Success(None)
      },
      startupStateReader = Some(stateReader),
      utxoStateUpdate = Some((state, _) => state)) {
      (actor, scanner, client) =>
        scanner.expectNoMessage(300.millis)
        client.send(actor, GetWalletStatus)
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        error should include("pruning horizon")
        error should include("resync from genesis")
        catchUpChecks.get() shouldBe 1
        client.send(actor, ReadBalances(ChainStatus.OnChain))
        client.expectMsgType[Status.Failure]
    }
  }

  property("quarantine when pruning advances after ordinary catch-up preflight") {
    val prunedSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-catchup-pruning-race-").toFile
    val canonicalAncestor = ModifierId @@ Algos.encode(Array.fill(32)(117: Byte))
    val currentBest = ModifierId @@ Algos.encode(Array.fill(32)(118: Byte))
    val readinessChecks = new AtomicInteger(0)
    val consumingReads = new AtomicInteger(0)
    val stateReader = snapshotRecoveryStateReader(
      prunedSettings, currentBest, versionId = Some(currentBest), stateHeight = 7)

    seedWalletRegistryVersions(prunedSettings, directory, Seq(canonicalAncestor -> 6))
    withSeededWalletStorage(prunedSettings, directory)(
      _.updateStateContext(stateReader.stateContext).get)

    withProbeWalletActor(
      prunedSettings,
      directory,
      strictHistoryReader(bestFullBlockRead = _ => consumingReads.incrementAndGet()),
      catchUpReady = Some(height => {
        height shouldBe 7
        readinessChecks.incrementAndGet()
        true
      }),
      currentStateTip = Some(_ => Success(7 -> Some(currentBest))),
      bestHeaderId = Some {
        case 6 => Success(Some(canonicalAncestor))
        case 7 => Success(Some(currentBest))
        case _ => Success(None)
      }) { (actor, scanner, client) =>
      client.send(actor, ChangedState(stateReader))
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        consumingReads.get() shouldBe 1
        client.send(actor, GetWalletStatus)
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        error should include("pruning horizon")
        error should include("resync from genesis")
      }, 5.seconds, 100.millis)
      readinessChecks.get() shouldBe 1
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
      client.send(actor, DeriveNextKey)
      client.expectMsgType[DeriveNextKeyResult].result.isFailure shouldBe true
    }
  }

  property("quarantine an unreadable durable rollback intent before wallet operations") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-corrupt-rollback-intent-").toFile
    val rollbackAttempts = new AtomicInteger(0)
    overwriteWalletStorageEntry(
      ordinarySettings,
      directory,
      WalletStorage.WalletRollbackIntentKey,
      Array[Byte](1, 2, 3))

    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(),
      registryRollback = Some((_, _) => {
        rollbackAttempts.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
      error should include("rollback intent")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
      rollbackAttempts.get() shouldBe 0
    }

    val retainedBytes = readWalletStorageEntry(
      ordinarySettings,
      directory,
      WalletStorage.WalletRollbackIntentKey).get
    retainedBytes should contain theSameElementsInOrderAs Array[Byte](1, 2, 3)
  }

  property("fail closed when ordinary registry rollback result is indeterminate") {
    val ordinarySettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = false)))
    val directory = Files.createTempDirectory("wallet-rollback-indeterminate-").toFile
    val originalTip = ModifierId @@ Algos.encode(Array.fill(32)(118: Byte))
    val rollbackAttempts = new AtomicInteger(0)
    val reconciliationAttempts = new AtomicInteger(0)
    seedNonemptyWalletRegistry(ordinarySettings, directory, originalTip)
    val currentStateTip = new AtomicReference[(Int, Option[ModifierId])](
      7 -> Some(originalTip))

    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(),
      currentStateTip = Some(_ => Success(currentStateTip.get())),
      bestHeaderId = Some {
        case 7 => Success(Some(originalTip))
        case _ => Success(None)
      },
      registryRollback = Some((_, _) => {
        rollbackAttempts.incrementAndGet()
        Failure(new IllegalStateException("injected indeterminate registry rollback failure"))
      }),
      offChainReconciliation = Some((_, _, fallback) => {
        reconciliationAttempts.incrementAndGet()
        fallback()
    })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      val reconciliationBaseline = reconciliationAttempts.get()
      currentStateTip.set(0 -> None)
      client.send(actor, ChangedState(genericStateReader(
        ErgoStateContext.empty(ordinarySettings.chainSettings, parameters))))
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
      client.send(actor, Rollback(idToVersion(PreGenesisHeader.id)))
      client.send(actor, GetWalletStatus)
      val rollbackError = client.expectMsgType[WalletStatus].error.get.toLowerCase
      rollbackAttempts.get() shouldBe 1
      rollbackError should include("quarantine")
      rollbackError should include("injected indeterminate registry rollback failure")
      rollbackError should include("restart")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]

      client.send(actor, ChangedMempool(new FakeMempool(Seq.empty)))
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get.toLowerCase should include("restart")
      reconciliationAttempts.get() shouldBe reconciliationBaseline
      client.send(actor, ReadBalances(ChainStatus.OffChain))
      client.expectMsgType[Status.Failure]
      scanner.expectNoMessage(300.millis)
    }

    withSeededWalletStorage(ordinarySettings, directory) { storage =>
      storage.readWalletRollbackIntentTry().get shouldBe
        Some(WalletRollbackIntent(PreGenesisHeader.id, expectedHeight = 0))
    }
  }

  property("reject guarded mutation before queued initial catch-up failure is delivered") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-queued-catch-up-failure-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(115: Byte))
    val actorRef = new AtomicReference[ActorRef]()
    val clientRef = new AtomicReference[ActorRef]()
    val readinessChecks = new AtomicInteger(0)
    val statusRemovals = new AtomicInteger(0)
    val sourceRemovals = new AtomicInteger(0)
    val scanRequest = ScanRequest(
      "queued-catch-up-mutation",
      ActorDefinitionOtherPredicate,
      Some(ScanWalletInteraction.Off),
      Some(false))

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      snapshotFullHeight = Some(_ => 1),
      catchUpReady = Some(height => {
        height shouldBe 1
        readinessChecks.incrementAndGet()
        actorRef.get().tell(AddScan(scanRequest), clientRef.get())
        false
      }),
      statusRemoval = Some(state => {
        statusRemovals.incrementAndGet()
        state.storage.removeUtxoSnapshotScanStatus()
      }),
      sourceRemoval = Some(_ => {
        sourceRemovals.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      actorRef.set(actor)
      clientRef.set(client.ref)
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 1))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 1,
        completed = true, boxes = IndexedSeq.empty))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get.completed shouldBe true

      val mutation = client.expectMsgType[AddScanResponse](5.seconds).response
      mutation.isFailure shouldBe true
      mutation.failed.get.getMessage.toLowerCase should include("pruning horizon")
      client.send(actor, ReadScans)
      client.expectMsgType[Status.Failure]
      readinessChecks.get() shouldBe 1
      statusRemovals.get() shouldBe 0
      sourceRemovals.get() shouldBe 0
      client.awaitAssert({
        client.send(actor, GetWalletStatus)
        val error = client.expectMsgType[WalletStatus].error.get.toLowerCase
        error should include("pruning horizon")
        error should include("resync from genesis")
      }, 5.seconds, 100.millis)
      scanner.expectMsg(AbortUtxoSnapshotScan(run))
    }
  }

  property("retry source cleanup after completed status removal while keeping the wallet usable") {
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
    val sourceAttempts = new AtomicInteger(0)
    val firstAttemptAt = new AtomicLong(0L)
    val secondAttemptAt = new AtomicLong(0L)
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      sourceRemoval = Some(id => {
        id shouldBe snapshotId
        sourceAttempts.incrementAndGet() match {
          case 1 =>
            firstAttemptAt.set(System.nanoTime())
            Failure(new IllegalStateException("injected source cleanup failure"))
          case 2 =>
            secondAttemptAt.set(System.nanoTime())
            Success(())
          case attempt =>
            Failure(new IllegalStateException(s"unexpected source cleanup attempt $attempt"))
        }
      })) {
      (actor, scanner, client) =>
        scanner.expectNoMessage(300.millis)
        client.awaitAssert({
          sourceAttempts.get() shouldBe 2
          client.send(actor, GetWalletStatus)
          client.expectMsgType[WalletStatus].error shouldBe None
        }, 6.seconds, 100.millis)
        client.send(actor, ReadBalances(ChainStatus.OnChain))
        client.expectMsgType[WalletDigest]
    }
    (secondAttemptAt.get() - firstAttemptAt.get()).nanos should be >= 500.millis
    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe
        Some(snapshotOrigin(status.snapshotHeight, status.snapshotBlockId))
    }
  }

  property("rearm exhausted snapshot source cleanup on state change") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-source-cleanup-rearm-").toFile
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
    val allowCleanup = new AtomicBoolean(false)
    val sourceAttempts = new AtomicInteger(0)
    withSeededWalletStorage(bootstrapSettings, directory)(_.writeUtxoSnapshotScanStatus(status).get)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source)),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      currentStateTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      sourceRemoval = Some(id => {
        id shouldBe snapshotId
        sourceAttempts.incrementAndGet()
        if (allowCleanup.get()) Success(())
        else Failure(new IllegalStateException("persistent source cleanup failure"))
      })) { (actor, scanner, client) =>
      val initialStateReader = snapshotRecoveryStateReader(
        bootstrapSettings, snapshotId, versionId = Some(snapshotId))
      client.send(actor, ChangedState(initialStateReader))
      scanner.expectNoMessage(300.millis)
      client.awaitAssert(
        sourceAttempts.get() shouldBe ErgoWalletActor.MaxFinalizationCleanupRetries + 1,
        8.seconds,
        100.millis)
      client.expectNoMessage(ErgoWalletActor.FinalizationCleanupRetryDelay + 300.millis)
      sourceAttempts.get() shouldBe ErgoWalletActor.MaxFinalizationCleanupRetries + 1
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error.get should
        include("Unable to remove completed UTXO snapshot scan source")

      allowCleanup.set(true)
      val stateReader = snapshotRecoveryStateReader(
        bootstrapSettings, snapshotId, versionId = Some(snapshotId))
      client.send(actor, ChangedState(stateReader))
      client.send(actor, ChangedState(stateReader))
      client.awaitAssert({
        sourceAttempts.get() shouldBe ErgoWalletActor.MaxFinalizationCleanupRetries + 2
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
      client.expectNoMessage(ErgoWalletActor.FinalizationCleanupRetryDelay + 300.millis)
      sourceAttempts.get() shouldBe ErgoWalletActor.MaxFinalizationCleanupRetries + 2
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest]
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe
        Some(snapshotOrigin(status.snapshotHeight, status.snapshotBlockId))
    }
  }

  property("resume a cursor-zero recovery after a crash preserved the completed origin") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-recovery-resume-").toFile
    val snapshotHeight = 5
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(121: Byte))
    val completed = snapshotStatus(
      snapshotHeight, snapshotId, manifestDepth = 6,
      nextSubtreeIndex = 3, totalSubtrees = 3, completed = true)
    val freshStatus = completed.copy(nextSubtreeIndex = 0, completed = false)
    val invalidation = UtxoSnapshotScanInvalidation(snapshotHeight, snapshotId)
    val source = UtxoSnapshotSourceIdentity(
      snapshotHeight, snapshotId, freshStatus.manifestDepth, freshStatus.totalSubtrees)

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.completeUtxoSnapshotScan(completed).get
      storage.writeUtxoSnapshotScanInvalidation(invalidation).get
      storage.restartUtxoSnapshotScanRecovery(invalidation, freshStatus).get shouldBe true
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
    }

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(_ => Success(source))) { (actor, scanner, client) =>
      val start = scanner.expectMsgType[StartUtxoSnapshotScan](5.seconds)
      start.run.hasSnapshot(snapshotHeight, snapshotId) shouldBe true
      start.forceRestart shouldBe false
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
      storage.readUtxoSnapshotScanStatusTry().get shouldBe Some(freshStatus)
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe
        Some(snapshotOrigin(snapshotHeight, snapshotId))
    }
  }

  property("resume source-only cleanup and fence rollback below the completed origin") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-origin-cleanup-restart-").toFile
    val snapshotId = ModifierId @@ Algos.encode(Array.fill(32)(113: Byte))
    val rollbackId = ModifierId @@ Algos.encode(Array.fill(32)(114: Byte))
    val status = snapshotStatus(0, snapshotId, 6, 1, 1, completed = true)
    val sourceReads = new AtomicInteger(0)
    val sourceAttempts = new AtomicInteger(0)
    val rollbackAttempts = new AtomicInteger(0)
    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.completeUtxoSnapshotScan(status).get
      storage.removeUtxoSnapshotScanStatus().get
    }

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(heightLookup = id => if (id == rollbackId) Some(-1) else None),
      sourceIdentity = Some(_ => {
        sourceReads.incrementAndGet()
        Failure(new IllegalStateException("source-only cleanup must not resume the scanner"))
      }),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      sourceRemoval = Some(id => {
        id shouldBe snapshotId
        if (sourceAttempts.incrementAndGet() == 1) {
          Failure(new IllegalStateException("injected restart cleanup failure"))
        } else {
          Success(())
        }
      }),
      registryRollback = Some((_, version) => {
        version shouldBe idToVersion(rollbackId)
        rollbackAttempts.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.awaitAssert({
        sourceAttempts.get() shouldBe 2
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 6.seconds, 100.millis)
      sourceReads.get() shouldBe 0
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[WalletDigest]
      scanner.expectNoMessage(300.millis)

      client.send(actor, Rollback(idToVersion(rollbackId)))
      client.send(actor, GetWalletStatus)
      val rollbackError = client.expectMsgType[WalletStatus].error.get.toLowerCase
      rollbackAttempts.get() shouldBe 0
      rollbackError should include("quarantine")
      rollbackError should include("invalidates completed utxo snapshot origin")
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]
      scanner.expectNoMessage(300.millis)
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe
        Some(UtxoSnapshotScanInvalidation(status.snapshotHeight, status.snapshotBlockId))
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe
        Some(snapshotOrigin(status.snapshotHeight, status.snapshotBlockId))
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
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      expectedDigestBytes = WalletDigestSerializer.toBytes(client.expectMsgType[WalletDigest])
      val run = startProbeRun(actor, scanner, client, snapshotHeight = 0, snapshotId)
      client.send(actor, GetOrInitUtxoSnapshotScanStatus(
        run, ManifestSerializer.MainnetManifestDepth.toInt, totalSubtrees = 33))
      client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      client.send(actor, ApplyUtxoSnapshotScanBatch(
        run, subtreeIndex = 0, nextSubtreeIndex = 32, completed = false, boxes = IndexedSeq.empty))
      val applied = client.expectMsgType[Try[UtxoSnapshotScanStatus]].get
      expectedStatusBytes = UtxoSnapshotScanStatusSerializer.toBytes(applied)
      client.send(actor, ReadBalances(ChainStatus.OnChain))
      client.expectMsgType[Status.Failure]

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

  property("drain an on-chain block retained while UTXO snapshot scan is pending") {
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
      stopFixtureUtxoSnapshotScanner(wallet.walletActor, w.actorSystem)
      val run = await(wallet.walletActor ? UtxoSnapshotAppliedToState(
        genesisBlock.height, genesisBlock.id, getUtxoState))
        .asInstanceOf[Try[Option[UtxoSnapshotScanRun]]].get.get

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
      Try(await(wallet.walletActor ? ReadBalances(ChainStatus.OnChain))).failed.get.getMessage should include("unresolved")

      applyBlock(genesisBlock) shouldBe 'success

      val returnBalance = initialBalance / 2
      val spendingTemplate = makeSpendingTx(initialBoxes, address, returnBalance)
      val unsignedSpendingTx = UnsignedErgoTransaction(
        initialBoxes.map(box => new UnsignedInput(box.id)).toIndexedSeq,
        IndexedSeq.empty,
        spendingTemplate.outputCandidates)
      val spendingTx = ErgoTransaction(defaultProver.sign(
        unsignedSpendingTx,
        initialBoxes.toIndexedSeq,
        emptyDataBoxes,
        getUtxoState.stateContext).get)
      val nextBlock = makeNextBlock(getUtxoState, Seq(spendingTx))

      applyBlock(nextBlock) shouldBe 'success
      wallet.scanPersistent(nextBlock)
      eventually {
        getHistory.bestHeaderAtHeight(nextBlock.height).map(_.id) shouldBe Some(nextBlock.id)
      }
      Thread.sleep(500)
      Try(await(wallet.walletActor ? ReadBalances(ChainStatus.OnChain))).failed.get.getMessage should include("unresolved")

      await(wallet.walletActor ? ApplyUtxoSnapshotScanBatch(
        run,
        subtreeIndex = 32,
        nextSubtreeIndex = 33,
        completed = true,
        boxes = IndexedSeq.empty
      )) shouldBe a[Success[_]]

      eventually {
        val balances = getConfirmedBalances
        balances.walletBalance shouldBe returnBalance
        balances.height shouldBe nextBlock.height
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
      stopFixtureUtxoSnapshotScanner(wallet.walletActor, w.actorSystem)
      val run = await(wallet.walletActor ? UtxoSnapshotAppliedToState(
        snapshot.height, snapshot.id, getUtxoState))
        .asInstanceOf[Try[Option[UtxoSnapshotScanRun]]].get.get

      implicit val patienceConfig: PatienceConfig = PatienceConfig(5.seconds, 100.millis)
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

      val addResponse = await(wallet.addBox(blockedAddBox, Set(org.ergoplatform.wallet.Constants.PaymentsScanId)))
      val stopTrackingResponse = await(wallet.stopTracking(
        org.ergoplatform.wallet.Constants.PaymentsScanId,
        boxToStopTracking.id))

      addResponse.status.isFailure shouldBe true
      stopTrackingResponse.status.isFailure shouldBe true

      await(wallet.walletActor ? ApplyUtxoSnapshotScanBatch(
        run,
        subtreeIndex = 32,
        nextSubtreeIndex = 33,
        completed = true,
        boxes = IndexedSeq.empty)) shouldBe a[Success[_]]
      eventually {
        val tracked = await(wallet.walletBoxes(unspentOnly = false, considerUnconfirmed = false))
        tracked.exists(box => java.util.Arrays.equals(box.trackedBox.box.id, blockedAddBox.id)) shouldBe false
        tracked.find(box => java.util.Arrays.equals(box.trackedBox.box.id, boxToStopTracking.id))
          .get.trackedBox.scans should contain(org.ergoplatform.wallet.Constants.PaymentsScanId)
        getConfirmedBalances.walletBalance shouldBe balanceAmount(snapshotBoxes)
      }
    }
  }

  property("enforce UTXO snapshot identity cursor and replay quarantine invariants") {
    val bootstrapSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    new WalletFixture(bootstrapSettings, parameters, getCurrentView(_).vault).apply { implicit w =>
      val address = getPublicKeys.head
      val snapshot = makeGenesisBlock(address.pubkey)
      val trackedBox = boxesAvailable(snapshot, address.pubkey).head
      val otherBlockId = ModifierId @@ Algos.encode(Array.fill(32)(91: Byte))
      stopFixtureUtxoSnapshotScanner(wallet.walletActor, w.actorSystem)
      val run = await(wallet.walletActor ? UtxoSnapshotAppliedToState(
        snapshot.height, snapshot.id, getUtxoState))
        .asInstanceOf[Try[Option[UtxoSnapshotScanRun]]].get.get
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

      Try(await(wallet.walletActor ? ReadBalances(ChainStatus.OnChain))).failed.get
        .isInstanceOf[IllegalStateException] shouldBe true

      val divergentReplay = ApplyUtxoSnapshotScanBatch(
        run, 0, 32, completed = false, IndexedSeq.empty)
      askTry(divergentReplay).isFailure shouldBe true

      Try(await(wallet.walletActor ? ReadPublicKeys(0, Int.MaxValue))).failed.get
        .isInstanceOf[IllegalStateException] shouldBe true
      Try(await(wallet.walletActor ? ReadScans)).failed.get
        .isInstanceOf[IllegalStateException] shouldBe true
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
      Try(await(wallet.walletActor ? ReadBalances(ChainStatus.OnChain))).failed.get
        .isInstanceOf[IllegalStateException] shouldBe true
      w.testProbe.send(wallet.walletActor, Rollback(idToVersion(snapshot.id)))
      w.testProbe.send(wallet.walletActor, GetOrInitUtxoSnapshotScanStatus(
        run,
        ManifestSerializer.MainnetManifestDepth.toInt,
        totalSubtrees = 33))
      w.testProbe.expectMsgType[Status.Failure].cause.getMessage.toLowerCase should include("quarantined")
      Try(await(wallet.walletActor ? ReadBalances(ChainStatus.OnChain))).failed.get
        .isInstanceOf[IllegalStateException] shouldBe true
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
    val registryId = ModifierId @@ Algos.encode(Array.fill(32)(107: Byte))
    val stateId = ModifierId @@ Algos.encode(Array.fill(32)(108: Byte))
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
      registryTip = Some(_ => Success(
        (completed.snapshotHeight + 1) -> Some(registryId))),
      currentStateTip = Some(_ => Success(
        (completed.snapshotHeight + 2) -> Some(stateId))),
      bestHeaderId = Some {
        case height if height == completed.snapshotHeight => Success(Some(snapshotId))
        case height if height == completed.snapshotHeight + 1 => Success(Some(registryId))
        case height if height == completed.snapshotHeight + 2 => Success(Some(stateId))
        case _ => Success(None)
      },
      sourceRemoval = Some(_ => Success(())),
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

    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(),
      registryTip = Some(_ => Success(7 -> Some(registryVersion))),
      currentStateTip = Some(_ => Success(7 -> Some(registryVersion))),
      bestHeaderId = Some {
        case 0 => Success(Some(snapshotId))
        case 7 => Success(Some(registryVersion))
        case _ => Success(None)
      },
      sourceRemoval = Some(_ => Success(()))) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, RescanWallet(0))
      client.expectMsgType[Try[Unit]].failed.get.getMessage.toLowerCase should
        include("completed from utxo snapshot")
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
      registryTip = Some(_ => Success(7 -> Some(registryVersion))),
      currentStateTip = Some(_ => Success(7 -> Some(registryVersion))),
      bestHeaderId = Some {
        case 7 => Success(Some(registryVersion))
        case _ => Success(None)
      },
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

    val fullBlockReads = new AtomicInteger(0)
    withProbeWalletActor(
      ordinarySettings,
      directory,
      strictHistoryReader(fullBlockReads)) { (actor, scanner, client) =>
      scanner.expectNoMessage(300.millis)
      client.send(actor, RescanWallet(0))
      client.expectMsgType[Try[Unit]].isSuccess shouldBe true
      client.awaitAssert({
        fullBlockReads.get() shouldBe 1
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error shouldBe None
      }, 5.seconds, 100.millis)
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
      strictHistoryReader(),
      registryTip = Some(_ => Success(completed.snapshotHeight -> Some(snapshotId))),
      bestHeaderId = Some(_ => Success(Some(snapshotId))),
      sourceRemoval = Some(_ => Success(())),
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

  property("quarantine applied snapshots that conflict with a completed wallet origin") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val originId = ModifierId @@ Algos.encode(Array.fill(32)(107: Byte))
    val otherId = ModifierId @@ Algos.encode(Array.fill(32)(108: Byte))
    val originHeight = 0

    Seq[(String, (Int, ModifierId))](
      ("height", (originHeight + 1, originId)),
      ("block id", (originHeight, otherId))
    ).foreach { case (label, (eventHeight, eventId)) =>
      val directory = Files.createTempDirectory(
        s"wallet-snapshot-origin-event-${label.replace(' ', '-')}-").toFile
      val completed = snapshotStatus(originHeight, originId, 6, 1, 1, completed = true)
      withSeededWalletStorage(bootstrapSettings, directory) { storage =>
        storage.completeUtxoSnapshotScan(completed).get
        storage.removeUtxoSnapshotScanStatus().get
      }

      withProbeWalletActor(
        bootstrapSettings,
        directory,
        strictHistoryReader(),
        registryTip = Some(_ => Success(originHeight -> Some(originId))),
        bestHeaderId = Some(_ => Success(Some(originId))),
        sourceRemoval = Some(_ => Success(()))) { (actor, scanner, client) =>
        scanner.expectNoMessage(300.millis)
        client.send(actor, UtxoSnapshotAppliedToState(eventHeight, eventId, null))
        withClue(label) {
          client.expectMsgType[Try[Option[UtxoSnapshotScanRun]]].isFailure shouldBe true
        }
        client.send(actor, GetWalletStatus)
        client.expectMsgType[WalletStatus].error.get.toLowerCase should include("origin")
        scanner.expectNoMessage(300.millis)
      }

      withSeededWalletStorage(bootstrapSettings, directory) { storage =>
        withClue(label) {
          storage.readUtxoSnapshotScanInvalidationTry().get shouldBe
            Some(UtxoSnapshotScanInvalidation(originHeight, originId))
          storage.readUtxoSnapshotWalletOriginTry().get shouldBe
            Some(snapshotOrigin(originHeight, originId))
        }
      }
    }
  }

  property("quarantine a completed wallet origin that diverges from history or current state") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val originHeight = 5
    val originId = ModifierId @@ Algos.encode(Array.fill(32)(109: Byte))
    val otherId = ModifierId @@ Algos.encode(Array.fill(32)(110: Byte))
    val registryId = ModifierId @@ Algos.encode(Array.fill(32)(111: Byte))
    val stateId = ModifierId @@ Algos.encode(Array.fill(32)(112: Byte))

    final case class DivergenceCase(label: String,
                                    currentStateTip: (Int, Option[ModifierId]),
                                    registryTip: (Int, Option[ModifierId]),
                                    bestAt: Int => Option[ModifierId],
                                    expectedError: String,
                                    expectedFence: Option[UtxoSnapshotScanInvalidation])
    val cases = Seq(
      DivergenceCase(
        "origin header",
        originHeight -> Some(otherId),
        originHeight -> Some(otherId),
        _ => Some(otherId),
        expectedError = "origin",
        expectedFence = Some(UtxoSnapshotScanInvalidation(originHeight, originId))),
      DivergenceCase(
        "state below origin",
        (originHeight - 1) -> Some(otherId),
        originHeight -> Some(originId),
        height => if (height == originHeight) Some(originId) else Some(otherId),
        expectedError = "origin",
        expectedFence = Some(UtxoSnapshotScanInvalidation(originHeight, originId))),
      DivergenceCase(
        "noncanonical current state tip",
        (originHeight + 2) -> Some(stateId),
        (originHeight + 1) -> Some(registryId),
        height => height match {
          case `originHeight` => Some(originId)
          case h if h == originHeight + 1 => Some(registryId)
          case _ => Some(otherId)
        },
        expectedError = "state tip",
        expectedFence = None)
    )

    cases.foreach { testCase =>
      val directory = Files.createTempDirectory(
        s"wallet-snapshot-origin-divergence-${testCase.label.replace(' ', '-')}-").toFile
      val completed = snapshotStatus(originHeight, originId, 6, 1, 1, completed = true)
      withSeededWalletStorage(bootstrapSettings, directory) { storage =>
        storage.completeUtxoSnapshotScan(completed).get
        storage.removeUtxoSnapshotScanStatus().get
      }

      withProbeWalletActor(
        bootstrapSettings,
        directory,
        strictHistoryReader(),
        registryTip = Some(_ => Success(testCase.registryTip)),
        currentStateTip = Some(_ => Success(testCase.currentStateTip)),
        bestHeaderId = Some(height => Success(testCase.bestAt(height))),
        sourceRemoval = Some(_ => Success(()))) { (actor, scanner, client) =>
        scanner.expectNoMessage(300.millis)
        client.send(actor, GetWalletStatus)
        withClue(testCase.label) {
          client.expectMsgType[WalletStatus].error.get.toLowerCase should include(testCase.expectedError)
        }
        scanner.expectNoMessage(300.millis)
      }

      withSeededWalletStorage(bootstrapSettings, directory) { storage =>
        withClue(testCase.label) {
          storage.readUtxoSnapshotScanInvalidationTry().get shouldBe testCase.expectedFence
          storage.readUtxoSnapshotWalletOriginTry().get shouldBe
            Some(snapshotOrigin(originHeight, originId))
        }
      }
    }
  }

  property("accept a canonical wallet registry ahead of a delayed canonical state notification") {
    val bootstrapSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
      blocksToKeep = 0,
      utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)))
    val directory = Files.createTempDirectory("wallet-snapshot-origin-registry-ahead-").toFile
    val originHeight = 5
    val stateHeight = originHeight + 1
    val registryHeight = stateHeight + 1
    val originId = ModifierId @@ Algos.encode(Array.fill(32)(122: Byte))
    val stateId = ModifierId @@ Algos.encode(Array.fill(32)(123: Byte))
    val registryId = ModifierId @@ Algos.encode(Array.fill(32)(124: Byte))
    val completed = snapshotStatus(originHeight, originId, 6, 1, 1, completed = true)
    val source = UtxoSnapshotSourceIdentity(
      originHeight, originId, completed.manifestDepth, completed.totalSubtrees)
    val cleanupCalls = new AtomicInteger(0)
    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.completeUtxoSnapshotScan(completed).get
    }
    seedNonemptyWalletRegistry(bootstrapSettings, directory, registryId)

    withProbeWalletActor(
      bootstrapSettings,
      directory,
      strictHistoryReader(),
      sourceIdentity = Some(id => {
        id shouldBe originId
        Success(source)
      }),
      currentStateTip = Some(_ => Success(stateHeight -> Some(stateId))),
      bestHeaderId = Some(height => Success(height match {
        case `originHeight` => Some(originId)
        case `stateHeight` => Some(stateId)
        case `registryHeight` => Some(registryId)
        case _ => None
      })),
      snapshotFullHeight = Some(_ => stateHeight),
      sourceRemoval = Some(_ => {
        cleanupCalls.incrementAndGet()
        Success(())
      })) { (actor, scanner, client) =>
      eventually(cleanupCalls.get() shouldBe 1)
      scanner.expectNoMessage(300.millis)
      client.send(actor, GetWalletStatus)
      client.expectMsgType[WalletStatus].error shouldBe None
    }

    withSeededWalletStorage(bootstrapSettings, directory) { storage =>
      storage.readUtxoSnapshotScanStatusTry().get shouldBe None
      storage.readUtxoSnapshotScanInvalidationTry().get shouldBe None
      storage.readUtxoSnapshotWalletOriginTry().get shouldBe
        Some(snapshotOrigin(originHeight, originId))
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
      strictHistoryReader(),
      registryTip = Some(_ => Success(status.snapshotHeight -> Some(originId))),
      currentStateTip = Some(_ => Success(
        status.snapshotHeight -> Option.empty[ModifierId])),
      bestHeaderId = Some(_ => Success(Some(originId))),
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

      val blockedInitPass = SecretString.create("blocked-init")
      val blockedMnemonicPass = SecretString.create("blocked-mnemonic-pass")
      w.testProbe.send(wallet.walletActor,
        InitWallet(blockedInitPass, Some(blockedMnemonicPass)))
      w.testProbe.expectMsgType[Try[_]].isFailure shouldBe true
      an[RuntimeException] should be thrownBy blockedInitPass.getData
      an[RuntimeException] should be thrownBy blockedMnemonicPass.getData
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
      // applyBlock returns after syntactic acceptance. This ask is processed only after the
      // NodeViewHolder finishes that actor turn, including its state and mempool notifications.
      getCurrentView
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
      wallet.walletActor ! ChangedMempool(new FakeMempool(Seq(
        UnconfirmedTransaction(spendingTx, None))))

      eventually {
        val confirmedAfterSpending = getConfirmedBalances.walletBalance
        val totalAfterSpending = getBalancesWithUnconfirmed.walletBalance

        confirmedAfterSpending shouldBe confirmedBalance
        totalAfterSpending shouldBe balanceToReturn

        log.info(s"After spending before rollback: $confirmedAfterSpending")
        log.info(s"Total with unconfirmed balance after spending before rollback: $totalAfterSpending")
      }

      wallet.rollback(initialState.version)
      publishCurrentMempool
      eventually {
        val balanceAfterRollback = getConfirmedBalances.walletBalance
        val totalAfterRollback = getBalancesWithUnconfirmed.walletBalance

        log.info(s"Balance after rollback: $balanceAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        balanceAfterRollback shouldBe initialBalance
        totalAfterRollback shouldBe balanceAfterRollback
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
      publishCurrentMempool
      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed

        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        confirmedAfterRollback.walletBalance shouldBe initialBalance
        confirmedAfterRollback.walletAssetBalances shouldBe empty
        totalAfterRollback shouldBe confirmedAfterRollback
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
      }

      wallet.rollback(initialState.version)
      publishCurrentMempool
      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed
        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        confirmedAfterRollback shouldBe initialSnapshot
        totalAfterRollback shouldBe confirmedAfterRollback
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
      }
      wallet.rollback(initialState.version)
      publishCurrentMempool

      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed
        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")
        confirmedAfterRollback shouldBe initialSnapshot
        confirmedAfterRollback.walletAssetBalances.toMap shouldBe asset1Map
        totalAfterRollback shouldBe confirmedAfterRollback
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
      }
      wallet.rollback(initialState.version)
      publishCurrentMempool

      eventually {
        val confirmedAfterRollback = getConfirmedBalances
        val totalAfterRollback = getBalancesWithUnconfirmed

        log.info(s"Balance after rollback: $confirmedAfterRollback")
        log.info(s"Total with unconfirmed balance after rollback: $totalAfterRollback")

        confirmedAfterRollback.walletBalance shouldBe initialBalance
        totalAfterRollback.walletBalance shouldBe confirmedAfterRollback.walletBalance
        totalAfterRollback.walletAssetBalances.toMap shouldBe
          confirmedAfterRollback.walletAssetBalances.toMap
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
