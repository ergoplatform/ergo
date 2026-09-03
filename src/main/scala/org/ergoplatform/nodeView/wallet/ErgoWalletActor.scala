package org.ergoplatform.nodeView.wallet

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor._
import akka.pattern.StatusReply
import org.ergoplatform.ErgoBox._
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedMempool, ChangedState, CurrentWalletView, RequestCurrentWalletView, UtxoSnapshotAppliedToState}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.PreGenesisHeader
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.{EmptyHistoryHeight, Height}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletService._
import org.ergoplatform.nodeView.wallet.ErgoWalletServiceUtils.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, UtxoSnapshotChunkIntegrityException, UtxoSnapshotScanInvalidation, UtxoSnapshotScanStatus, UtxoSnapshotWalletOrigin, WalletRollbackIntent}
import org.ergoplatform.sdk.wallet.secrets.DerivationPath
import org.ergoplatform.settings._
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.BoxSelector
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform._
import org.ergoplatform.core.{VersionTag, idToVersion, versionToId}
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.utils.ScorexEncoding
import scorex.crypto.authds.ADDigest
import scorex.util.{ModifierId, ScorexLogging}

import java.util.UUID
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class ErgoWalletActor(settings: ErgoSettings,
                      parameters: Parameters,
                      ergoWalletService: ErgoWalletService,
                      boxSelector: BoxSelector,
                      historyReader: ErgoHistoryReader)
  extends Actor with Stash with Timers with ScorexLogging with ScorexEncoding {

  import ErgoWalletActor.{ApplyCurrentWalletMempool, ApplyCurrentWalletSnapshot,
    ApplyCurrentWalletState, ContinueCurrentWalletViewApplication,
    CurrentWalletViewApplicationStep, CurrentWalletViewRetryDelay,
    CurrentWalletViewRetryTimerKey, ExecuteCurrentWalletSnapshot,
    PendingWalletRollbackPreparation, RetryCurrentWalletViewRequest,
    WalletCatchUpBlockUnavailable, WalletRollbackEvidencePending}

  private val ergoAddressEncoder: ErgoAddressEncoder = settings.addressEncoder
  private var utxoSnapshotScanner: ActorRef = _
  private var startingUtxoSnapshot: Option[UtxoSnapshotScanRun] = None
  private var terminalUtxoSnapshot: Option[(Height, ModifierId)] = None
  private var finalizingUtxoSnapshot: Option[UtxoSnapshotScanRun] = None
  private var finalizingUtxoSnapshotStatus: Option[UtxoSnapshotScanStatus] = None
  private var activeUtxoSnapshotRun: Option[UtxoSnapshotScanRun] = None
  private var invalidUtxoSnapshotProgress: Boolean = false
  private var utxoSnapshotQuarantine: Option[UtxoSnapshotQuarantine] = None
  private var rollbackReconciliationQuarantine: Option[String] = None
  private var operationalMempoolReconciliationQuarantine: Option[String] = None
  private var rollbackFailureRequiresRestart: Boolean = false
  private var activeRollbackIntent: Option[WalletRollbackIntent] = None
  private var pendingWalletRollbackPreparation: Option[PendingWalletRollbackPreparation] = None
  private var walletRollbackPreparationReason: Option[String] = None
  private var rollbackRecoveryContinuation: Option[ErgoWalletState => Unit] = None
  private var rollbackRecoveryResumeSnapshotLifecycle: Boolean = false
  private var startupNoIntentAlignmentPending: Boolean = false
  private var startupDeferredMempool: Option[(ErgoMemPoolReader, ActorRef)] = None
  private var startupDeferredSnapshotEvent:
    Option[(UtxoSnapshotAppliedToState, Vector[ActorRef])] = None
  private var startupCanonicalStateTip: Option[(Height, Option[ModifierId])] = None
  private var pendingCurrentWalletViewRequest: Option[UUID] = Some(UUID.randomUUID())
  private var currentWalletViewApplicationInProgress: Boolean = false
  private var currentWalletViewApplicationSteps: Vector[CurrentWalletViewApplicationStep] =
    Vector.empty
  private var deferredRollbackReconciliationBlocks: Map[Height, ErgoFullBlock] = Map.empty
  private var blockedUtxoSnapshotCatchUp: Option[(Height, ModifierId)] = None
  private var deferredSnapshotBlock: Option[(Height, ErgoFullBlock)] = None
  private var pendingWalletCatchUpTarget: Option[Height] = None
  private var utxoSnapshotFinalization: UtxoSnapshotFinalizationState =
    UtxoSnapshotFinalizationState.empty
  private var utxoSnapshotSourceCleanupErrors: Map[ModifierId, String] = Map.empty

  override val supervisorStrategy: OneForOneStrategy =
    OneForOneStrategy(maxNrOfRetries = 5, withinTimeRange = 1.minute) {
      case _: ActorKilledException =>
        log.info("Wallet actor got KILL message")
        Stop
      case _: DeathPactException =>
        log.info("Wallet actor forced to stop")
        Stop
      case e: ActorInitializationException =>
        log.error(s"Wallet failed during initialization with: $e")
        Stop
      case e: Exception =>
        log.error(s"Wallet failed with: $e")
        Restart
    }

  override def postRestart(reason: Throwable): Unit = {
    log.error(s"Wallet actor restarted due to ${reason.getMessage}", reason)
    super.postRestart(reason)
  }

  override def postStop(): Unit = {
    logger.info("Wallet actor stopped")
    super.postStop()
  }

  protected[wallet] def createUtxoSnapshotScanner(): ActorRef =
    context.actorOf(
      UtxoSnapshotWalletScanner.props(self, settings, historyReader)
        .withDispatcher(GlobalConstants.ApiDispatcher),
      "utxo-snapshot-wallet-scanner")

  protected[wallet] def readUtxoSnapshotSourceIdentity(
    expectedBlockId: ModifierId): Try[UtxoSnapshotSourceIdentity] =
    Try(historyReader.readUtxoSnapshotScanSource(expectedBlockId)).flatten.map { source =>
      UtxoSnapshotSourceIdentity(
        source.snapshotHeight,
        source.snapshotBlockId,
        source.manifestDepth.toInt,
        source.partCount)
    }

  protected[wallet] def readAvailableUtxoSnapshotSourceIdentity(): Try[UtxoSnapshotSourceIdentity] =
    Try(historyReader.readUtxoSnapshotScanSource()).flatten.map { source =>
      UtxoSnapshotSourceIdentity(
        source.snapshotHeight,
        source.snapshotBlockId,
        source.manifestDepth.toInt,
        source.partCount)
    }

  protected[wallet] def readWalletRegistryTip(
    state: ErgoWalletState): Try[(Height, Option[ModifierId])] =
    Try(state.getWalletHeight -> state.registry.lastVersionId)

  protected[wallet] def readCurrentStateTip(
    state: ErgoWalletState): Try[(Height, Option[ModifierId])] = Try {
    val stateContext = state.stateContext
    stateContext.currentHeight -> stateContext.lastHeaderOpt.map(_.id)
  }

  protected[wallet] def readBestHeaderIdAtHeight(
    height: Height): Try[Option[ModifierId]] =
    Try(historyReader.bestHeaderAtHeight(height).map(_.id))

  protected[wallet] def readBestHeaderStateAtHeight(
    height: Height): Try[Option[(ModifierId, ADDigest)]] =
    Try(historyReader.bestHeaderAtHeight(height).map(header => header.id -> header.stateRoot))

  protected[wallet] def readUtxoSnapshotRecoveryFence(
    state: ErgoWalletState): Try[Option[UtxoSnapshotScanInvalidation]] =
    state.storage.readUtxoSnapshotScanInvalidationTry()

  protected[wallet] def readUtxoSnapshotWalletOrigin(
    state: ErgoWalletState): Try[Option[UtxoSnapshotWalletOrigin]] =
    state.storage.readUtxoSnapshotWalletOriginTry()

  protected[wallet] def calculateUtxoSnapshotScanDefinition(
    state: ErgoWalletState): Try[UtxoSnapshotScanDefinition] =
    UtxoSnapshotScanDefinition.calculate(state.walletVars, settings.walletSettings.dustLimit)

  protected[wallet] def updateUtxoSnapshotRecoveryWalletVars(
    state: ErgoWalletState,
    currentParameters: Parameters): Try[WalletVars] =
    Try(state.walletVars.withParameters(currentParameters)).flatten

  protected[wallet] def persistUtxoSnapshotRecoveryStateContext(
    state: ErgoWalletState,
    stateContext: ErgoStateContext): Try[Unit] =
    Try(state.storage.updateStateContext(stateContext)).flatten

  protected[wallet] def recreateRegistryForUtxoSnapshotRecovery(
    state: ErgoWalletState): RegistryResetOutcome =
    ergoWalletService.recreateRegistryForUtxoSnapshotRecovery(state, settings)

  protected[wallet] def closeWalletStorageForUtxoSnapshotRecovery(
    state: ErgoWalletState): Try[Unit] =
    Try(state.storage.close())

  protected[wallet] def restartUtxoSnapshotScanRecovery(
    state: ErgoWalletState,
    expected: UtxoSnapshotScanInvalidation,
    freshStatus: UtxoSnapshotScanStatus): Try[Boolean] =
    state.storage.restartUtxoSnapshotScanRecovery(expected, freshStatus)

  protected[wallet] def utxoSnapshotFullHeight(state: ErgoWalletState): Height =
    state.fullHeight

  protected[wallet] def isUtxoSnapshotCatchUpReady(height: Height): Boolean =
    historyReader.bestFullBlockAt(height).isDefined ||
      deferredRollbackReconciliationBlocks.contains(height) ||
      deferredSnapshotBlock.exists(_._1 == height)

  private def isWalletCatchUpBlockDefinitelyPruned(
    unavailable: WalletCatchUpBlockUnavailable): Boolean =
    Try(historyReader.minFullBlockAvailable).toOption.exists { minFullBlockHeight =>
      ErgoWalletActor.isWalletCatchUpBlockDefinitelyPruned(
        settings.nodeSettings.isFullBlocksPruned,
        minFullBlockHeight,
        unavailable.height)
    }

  private def requiredWalletCatchUpHeight(
    completedHeight: Height,
    fullHeight: Height): Try[Option[Height]] = {
    if (completedHeight >= fullHeight) {
      Success(None)
    } else {
      Try(Math.addExact(completedHeight, 1)).flatMap { nextHeight =>
        if (!settings.nodeSettings.isFullBlocksPruned) {
          Success(Some(nextHeight))
        } else {
          Try(isUtxoSnapshotCatchUpReady(nextHeight)).flatMap {
            case true => Success(Some(nextHeight))
            case false => Failure(WalletCatchUpBlockUnavailable(nextHeight))
          }
        }
      }
    }
  }

  private final class WalletRollbackCatchUpPlan(
    val stateTipHeight: Height,
    val stateTipId: Option[ModifierId])

  private def awaitWalletRollbackEvidence[T](message: String): Failure[T] =
    Failure(WalletRollbackEvidencePending(message))

  private def preflightWalletRollbackCatchUp(
    state: ErgoWalletState,
    intent: WalletRollbackIntent): Try[WalletRollbackCatchUpPlan] =
    for {
      currentTip <- readCurrentStateTip(state)
      plan = new WalletRollbackCatchUpPlan(currentTip._1, currentTip._2)
      _ <- if (intent.expectedHeight <= plan.stateTipHeight) Success(()) else awaitWalletRollbackEvidence(
        s"Wallet rollback target height ${intent.expectedHeight} is ahead of current state tip " +
          s"${plan.stateTipHeight}")
      _ <- validateCanonicalWalletRollbackTarget(intent)
      _ <- Try {
        if (intent.expectedHeight == plan.stateTipHeight) {
          val endpointMatches = plan.stateTipId.contains(intent.targetVersionId) ||
            (intent.targetVersionId == PreGenesisHeader.id && plan.stateTipId.isEmpty)
          if (!endpointMatches) {
            throw WalletRollbackEvidencePending(
              s"Current state tip (${plan.stateTipHeight}, ${plan.stateTipId}) does not match " +
                s"wallet rollback target (${intent.expectedHeight}, ${intent.targetVersionId})")
          }
        } else {
          var height = Math.addExact(intent.expectedHeight, 1)
          var expectedParent = intent.targetVersionId
          var finished = false
          while (!finished) {
            val (block, retainedInActor) = historyReader
              .bestFullBlockAt(height)
              .map(_ -> false)
              .orElse(deferredRollbackReconciliationBlocks.get(height).map(_ -> true))
              .getOrElse(throw WalletCatchUpBlockUnavailable(height))
            if (block.height != height) {
              throw new IllegalStateException(
                s"Wallet rollback catch-up returned block ${block.id} at height ${block.height}, " +
                  s"expected height $height")
            }
            if (retainedInActor) {
              validateRetainedWalletCatchUpBlock(height, block).get
            }
            if (block.header.parentId != expectedParent) {
              throw new IllegalStateException(
                s"Wallet rollback catch-up block ${block.id} at height $height has parent " +
                  s"${block.header.parentId}, expected $expectedParent")
            }
            expectedParent = block.id
            if (height == plan.stateTipHeight) {
              if (!plan.stateTipId.contains(expectedParent)) {
                throw WalletRollbackEvidencePending(
                  s"Wallet rollback catch-up endpoint $expectedParent at height $height does not " +
                    s"match frozen current state tip ${plan.stateTipId}")
              }
              finished = true
            } else {
              height = Math.addExact(height, 1)
            }
          }
        }
      }
      _ <- validateWalletRollbackCatchUpPlan(state, intent, plan)
    } yield plan

  private def validateWalletRollbackCatchUpPlan(
    state: ErgoWalletState,
    intent: WalletRollbackIntent,
    plan: WalletRollbackCatchUpPlan): Try[Unit] =
    for {
      currentTip <- readCurrentStateTip(state)
      _ <- if (currentTip == (plan.stateTipHeight -> plan.stateTipId)) Success(()) else awaitWalletRollbackEvidence(
        s"Current state tip changed from (${plan.stateTipHeight}, ${plan.stateTipId}) " +
          s"to $currentTip during wallet rollback catch-up preflight")
      currentBestTipId <-
        if (plan.stateTipHeight == EmptyHistoryHeight && plan.stateTipId.isEmpty) Success(None)
        else readBestHeaderIdAtHeight(plan.stateTipHeight)
      bestTipMatches = (plan.stateTipId, currentBestTipId) match {
        case (Some(expected), Some(actual)) => expected == actual
        case (None, None) if plan.stateTipHeight == EmptyHistoryHeight => true
        case _ => false
      }
      _ <- if (bestTipMatches) Success(()) else awaitWalletRollbackEvidence(
        s"Frozen current state tip (${plan.stateTipHeight}, ${plan.stateTipId}) is no longer " +
          s"the best header at that height: $currentBestTipId")
      _ <- validateCanonicalWalletRollbackTarget(intent)
    } yield ()

  private def utxoSnapshotCatchUpHeight(
    state: ErgoWalletState,
    completedHeight: Height): Try[Option[Height]] =
    Try(utxoSnapshotFullHeight(state)).flatMap(requiredWalletCatchUpHeight(completedHeight, _))

  private def walletCatchUpPruningReason(subject: String, cause: Throwable): String =
    s"$subject is outside the full-block pruning horizon: ${cause.getMessage}. " +
      "Re-bootstrap from a fresh UTXO snapshot or resync from genesis with the wallet initialized."

  private def walletCatchUpFailureReason(subject: String, cause: Throwable): String = cause match {
    case unavailable: WalletCatchUpBlockUnavailable
      if isWalletCatchUpBlockDefinitelyPruned(unavailable) =>
      walletCatchUpPruningReason(subject, cause)
    case _ =>
      s"$subject catch-up readiness could not be determined: ${cause.getMessage}. " +
        "Correct the underlying history or storage failure and restart the wallet."
  }

  private def mandatoryWalletCatchUpFailureReason(height: Height, cause: Throwable): String =
    s"Mandatory wallet catch-up failed at height $height: ${cause.getMessage}. " +
      "Wallet operations are disabled; correct the underlying history or storage failure and restart the wallet."

  protected[wallet] def scanUtxoSnapshotCatchUpHeight(
    state: ErgoWalletState,
    height: Height): Try[ErgoWalletState] =
    readMandatoryWalletCatchUpBlock(height).flatMap {
      case Some((block, retainedInActor)) =>
        val canonicalValidation =
          if (retainedInActor) validateRetainedWalletCatchUpBlock(height, block)
          else Success(())
        validateMandatoryWalletCatchUpBlock(state, height, block).flatMap { _ =>
          canonicalValidation
        }.flatMap { _ =>
          log.info(s"Wallet is scanning mandatory post-snapshot block ${block.id} at height ${block.height}")
          Try(ergoWalletService.scanBlockUpdate(state, block, settings.walletSettings.dustLimit)).flatten
        }
      case None =>
        Failure(WalletCatchUpBlockUnavailable(height))
    }

  private def readMandatoryWalletCatchUpBlock(
    height: Height): Try[Option[(ErgoFullBlock, Boolean)]] =
    Try(historyReader.bestFullBlockAt(height)).map {
      case Some(block) => Some(block -> false)
      case None => deferredRollbackReconciliationBlocks.get(height).map(_ -> true)
        .orElse(deferredSnapshotBlock.collect {
          case (`height`, block) => block -> true
        })
    }

  private def validateRetainedWalletCatchUpBlock(
    height: Height,
    block: ErgoFullBlock): Try[Unit] =
    readBestHeaderIdAtHeight(height).flatMap {
      case Some(canonicalId) if canonicalId == block.id => Success(())
      case Some(canonicalId) => fail(
        s"Retained wallet catch-up block ${block.id} at height $height is not canonical; " +
          s"the best header is $canonicalId")
      case None => fail(
        s"Retained wallet catch-up block ${block.id} at height $height cannot be proven canonical")
    }

  private def validateMandatoryWalletCatchUpBlock(
    state: ErgoWalletState,
    requestedHeight: Height,
    block: ErgoFullBlock): Try[Unit] =
    Try(Math.subtractExact(requestedHeight, 1)).flatMap { expectedWalletHeight =>
      if (block.height != requestedHeight) {
        fail(
          s"History returned wallet catch-up block ${block.id} at height ${block.height}, " +
            s"but the requested height was $requestedHeight")
      } else {
        readWalletRegistryTip(state).flatMap { case (walletHeight, walletVersion) =>
          if (walletHeight != expectedWalletHeight) {
            fail(
              s"Wallet catch-up block ${block.id} at height $requestedHeight requires registry " +
                s"height $expectedWalletHeight, but the wallet is at height $walletHeight")
          } else if (walletVersion != Some(block.header.parentId)) {
            fail(
              s"Wallet catch-up block ${block.id} at height $requestedHeight has parent " +
                s"${block.header.parentId}, but the wallet expected $walletVersion")
          } else {
            Success(())
          }
        }
      }
    }

  private def blockRecoverableUtxoSnapshotCatchUp(
    state: ErgoWalletState,
    run: UtxoSnapshotScanRun,
    stage: String,
    cause: Throwable): Unit = {
    val errorMsg =
      s"Mandatory post-snapshot catch-up $stage failed: ${cause.getMessage}. " +
        "Wallet operations are disabled until a canonical state change or rollback retries the catch-up; " +
        "otherwise correct the underlying history or storage failure and restart the wallet."
    utxoSnapshotFinalization = utxoSnapshotFinalization.catchUpFailed(run.snapshotBlockId)
    blockedUtxoSnapshotCatchUp = Some(run.snapshotHeight -> run.snapshotBlockId)
    finalizingUtxoSnapshot = None
    finalizingUtxoSnapshotStatus = None
    log.error(errorMsg, cause)
    context.become(loadedWallet(state.copy(error = Some(errorMsg), rescanInProgress = false)))
  }

  protected[wallet] def rollbackWalletRegistry(
    state: ErgoWalletState,
    version: VersionTag): Try[Unit] =
    state.registry.rollback(version)

  protected[wallet] def readWalletRollbackIntent(
    state: ErgoWalletState): Try[Option[WalletRollbackIntent]] =
    state.storage.readWalletRollbackIntentTry()

  protected[wallet] def writeWalletRollbackIntent(
    state: ErgoWalletState,
    intent: WalletRollbackIntent): Try[Unit] =
    state.storage.writeWalletRollbackIntent(intent)

  protected[wallet] def clearWalletRollbackIntent(
    state: ErgoWalletState,
    intent: WalletRollbackIntent): Try[Boolean] =
    state.storage.clearWalletRollbackIntent(intent)

  protected[wallet] def replaceWalletRollbackIntent(
    state: ErgoWalletState,
    expected: WalletRollbackIntent,
    replacement: WalletRollbackIntent): Try[Boolean] =
    state.storage.replaceWalletRollbackIntent(expected, replacement)

  protected[wallet] def walletRegistryVersionExists(
    state: ErgoWalletState,
    versionId: ModifierId): Try[Boolean] =
    state.registry.versionIdExists(versionId)

  protected[wallet] def readWalletRegistryRollbackVersionIds(
    state: ErgoWalletState): Try[Seq[ModifierId]] =
    state.registry.rollbackVersionIds

  private def reconcileAfterWalletRollback(state: ErgoWalletState): Try[ErgoWalletState] =
    Try(ergoWalletService.reconcileOffChainRegistry(
      state.copy(outputsFilter = None, error = None),
      settings.walletSettings.dustLimit))

  private def reconcileAfterMandatoryWalletCatchUp(
    state: ErgoWalletState): Try[ErgoWalletState] =
    Try {
      val reconciledState = ergoWalletService.reconcileOffChainRegistry(
        state, settings.walletSettings.dustLimit)
      ergoWalletService.updateUtxoState(reconciledState)
    }

  protected[wallet] def removeUtxoSnapshotScanStatus(state: ErgoWalletState): Try[Unit] =
    state.storage.removeUtxoSnapshotScanStatus()

  protected[wallet] def removeUtxoSnapshotScanSource(snapshotBlockId: ModifierId): Try[Unit] =
    Try(historyReader.removeUtxoSnapshotScanSource(snapshotBlockId)).flatten

  private def requestCurrentWalletView(requestId: UUID): Unit = {
    if (pendingCurrentWalletViewRequest.contains(requestId)) {
      context.system.eventStream.publish(RequestCurrentWalletView(requestId, self))
      timers.startSingleTimer(
        CurrentWalletViewRetryTimerKey,
        RetryCurrentWalletViewRequest(requestId),
        CurrentWalletViewRetryDelay)
    }
  }

  override def preStart(): Unit = {
    log.info("Initializing wallet actor")
    ErgoWalletState.initial(settings, parameters) match {
      case Success(state) =>
        utxoSnapshotScanner = createUtxoSnapshotScanner()
        context.system.eventStream.subscribe(self, classOf[ChangedState])
        context.system.eventStream.subscribe(self, classOf[ChangedMempool])
        context.system.eventStream.subscribe(self, classOf[UtxoSnapshotAppliedToState])
        pendingCurrentWalletViewRequest.foreach(requestCurrentWalletView)
        self ! ReadWallet(state)
      case Failure(ex) =>
        log.error("Unable to initialize wallet", ex)
        ErgoApp.shutdownSystem()(context.system)
    }
  }

  private def emptyWallet: Receive = {
    case ReadWallet(state) =>
      val ws = settings.walletSettings
      // Try to read wallet from json file or test mnemonic provided in a config file
      val newState = ergoWalletService.readWallet(state, ws.testMnemonic.map(SecretString.create(_)), ws.testKeysQty, ws.secretStorage)
      initializeWalletRollbackRecovery(newState)
      unstashAll()
    case _ => // stashing all messages until wallet is setup
      stash()
  }

  private def hasPendingUtxoSnapshotScan(state: ErgoWalletState): Boolean =
    utxoSnapshotQuarantine.nonEmpty || activeUtxoSnapshotRun.nonEmpty ||
      startingUtxoSnapshot.nonEmpty ||
      terminalUtxoSnapshot.nonEmpty || finalizingUtxoSnapshot.nonEmpty ||
      invalidUtxoSnapshotProgress || blockedUtxoSnapshotCatchUp.nonEmpty ||
      utxoSnapshotFinalization.catchUpScheduled.nonEmpty ||
      (state.storage.readUtxoSnapshotScanStatusTry() match {
      case Success(status) => status.nonEmpty
      case Failure(_) => true
    })

  private def hasPendingWalletRecovery(state: ErgoWalletState): Boolean =
    hasPendingUtxoSnapshotScan(state) || pendingWalletRollbackPreparation.nonEmpty ||
      pendingWalletCatchUpTarget.nonEmpty

  private val startupAlignmentPendingReason: String =
    "Wallet operations are unavailable while startup canonical alignment is pending"

  private def deferStartupUtxoSnapshotEvent(
    event: UtxoSnapshotAppliedToState,
    replyTo: ActorRef): Unit = {
    startupDeferredSnapshotEvent = startupDeferredSnapshotEvent match {
      case None => Some(event -> Vector(replyTo))
      case Some((current, waiters))
        if current.blockHeight == event.blockHeight && current.blockId == event.blockId =>
        Some(event -> (waiters :+ replyTo))
      case Some((current, waiters)) =>
        val cause = new IllegalStateException(
          s"Deferred UTXO snapshot event ${current.blockId} at height ${current.blockHeight} " +
            s"was superseded before startup alignment by ${event.blockId} at height ${event.blockHeight}")
        waiters.foreach(_ ! Failure(cause))
        Some(event -> Vector(replyTo))
    }
  }

  private def enqueueCurrentWalletViewApplication(
    steps: Seq[CurrentWalletViewApplicationStep]): Unit = {
    currentWalletViewApplicationSteps ++= steps
    if (!currentWalletViewApplicationInProgress && currentWalletViewApplicationSteps.nonEmpty) {
      currentWalletViewApplicationInProgress = true
      self ! ContinueCurrentWalletViewApplication
    }
  }

  private def replayStartupDeferredMempool(): Unit = {
    val deferred = startupDeferredMempool
    startupDeferredMempool = None
    enqueueCurrentWalletViewApplication(deferred.toSeq.map { case (reader, _) =>
      ApplyCurrentWalletMempool(reader)
    })
  }

  private def canReplayStartupSnapshotEvent: Boolean =
    !startupNoIntentAlignmentPending && pendingWalletRollbackPreparation.isEmpty &&
      rollbackReconciliationQuarantine.isEmpty && !rollbackFailureRequiresRestart &&
      pendingWalletCatchUpTarget.isEmpty

  private def maybeReplayStartupSnapshotEvent(): Unit = {
    if (canReplayStartupSnapshotEvent) {
      val deferred = startupDeferredSnapshotEvent
      startupDeferredSnapshotEvent = None
      deferred.foreach { case (event, waiters) =>
        startupCanonicalStateTip match {
          case Some((height, Some(blockId)))
            if event.blockHeight == height && event.blockId == blockId =>
            enqueueCurrentWalletViewApplication(
              waiters.map(replyTo => ApplyCurrentWalletSnapshot(event, replyTo)))
          case currentTip =>
            val cause = new IllegalStateException(
              s"Deferred UTXO snapshot event ${event.blockId} at height ${event.blockHeight} " +
                s"does not match the accepted startup state tip $currentTip")
            waiters.foreach(_ ! Failure(cause))
        }
      }
    }
  }

  private def failDeferredStartupInputs(cause: Throwable): Unit = {
    startupDeferredMempool = None
    startupCanonicalStateTip = None
    val deferredEvent = startupDeferredSnapshotEvent
    startupDeferredSnapshotEvent = None
    deferredEvent.foreach { case (_, waiters) =>
      waiters.foreach(_ ! Failure(cause))
    }
    deferredSnapshotBlock = None
    deferredRollbackReconciliationBlocks = Map.empty
  }

  private def shouldDeferSnapshotBlock(state: ErgoWalletState): Boolean =
    startingUtxoSnapshot.nonEmpty || finalizingUtxoSnapshot.nonEmpty ||
      state.storage.readUtxoSnapshotScanStatusTry().toOption.flatten.exists(!_.completed)

  private def currentUtxoSnapshotBoundary(state: ErgoWalletState): Option[(Height, ModifierId)] =
    startingUtxoSnapshot.map(run => run.snapshotHeight -> run.snapshotBlockId)
      .orElse(activeUtxoSnapshotRun.map(run => run.snapshotHeight -> run.snapshotBlockId))
      .orElse(terminalUtxoSnapshot)
      .orElse(finalizingUtxoSnapshot.map(run => run.snapshotHeight -> run.snapshotBlockId))
      .orElse(blockedUtxoSnapshotCatchUp)
      .orElse(state.storage.readUtxoSnapshotScanStatusTry().toOption.flatten
        .map(s => s.snapshotHeight -> s.snapshotBlockId))

  private def isCurrentUtxoSnapshotRun(run: UtxoSnapshotScanRun): Boolean =
    activeUtxoSnapshotRun.contains(run) &&
      !terminalUtxoSnapshot.contains(run.snapshotHeight -> run.snapshotBlockId)

  private def newUtxoSnapshotRun(snapshotHeight: Height,
                                 snapshotBlockId: ModifierId): UtxoSnapshotScanRun =
    UtxoSnapshotScanRun(
      UtxoSnapshotRunToken(UUID.randomUUID()), snapshotHeight, snapshotBlockId)

  private def enterUtxoSnapshotQuarantine(state: ErgoWalletState,
                                          reason: String,
                                          fence: Option[UtxoSnapshotScanInvalidation],
                                          persistFence: Boolean): Unit = {
    val fenceWrite = if (persistFence) {
      fence match {
        case Some(invalidation) => state.storage.writeUtxoSnapshotScanInvalidation(invalidation)
        case None => Failure(new IllegalStateException("A durable UTXO snapshot fence identity is unavailable"))
      }
    } else {
      Success(())
    }
    val quarantineReason = fenceWrite match {
      case Success(_) => s"UTXO snapshot wallet is quarantined: $reason"
      case Failure(t) =>
        s"UTXO snapshot wallet is quarantined: $reason; durable invalidation fence write failed: ${t.getMessage}"
    }
    val run = activeUtxoSnapshotRun
    activeUtxoSnapshotRun = None
    startingUtxoSnapshot = None
    finalizingUtxoSnapshot = None
    finalizingUtxoSnapshotStatus = None
    blockedUtxoSnapshotCatchUp = None
    deferredSnapshotBlock = None
    deferredRollbackReconciliationBlocks = Map.empty
    terminalUtxoSnapshot = fence.map(marker => marker.snapshotHeight -> marker.snapshotBlockId)
    invalidUtxoSnapshotProgress = true
    utxoSnapshotQuarantine = Some(UtxoSnapshotQuarantine(quarantineReason, fence))
    fence.foreach { marker =>
      utxoSnapshotFinalization = utxoSnapshotFinalization.invalidate(marker.snapshotBlockId)
      utxoSnapshotSourceCleanupErrors -= marker.snapshotBlockId
    }
    run.foreach(active => utxoSnapshotScanner ! AbortUtxoSnapshotScan(active))
    log.error(quarantineReason)
    context.become(loadedWallet(state.copy(error = Some(quarantineReason), rescanInProgress = false)))
  }

  private def startUtxoSnapshotScan(state: ErgoWalletState,
                                    snapshotHeight: Height,
                                    snapshotBlockId: ModifierId,
                                    forceRestart: Boolean): Option[UtxoSnapshotScanRun] = {
    val snapshot = snapshotHeight -> snapshotBlockId
    if (!settings.nodeSettings.utxoSettings.utxoBootstrap) {
      log.debug("Ignoring UTXO snapshot wallet scan request because utxoBootstrap is disabled")
      None
    } else if (!forceRestart && activeUtxoSnapshotRun.exists(_.hasSnapshot(snapshotHeight, snapshotBlockId))) {
      log.debug(s"UTXO snapshot wallet scan is already active at height $snapshotHeight")
      activeUtxoSnapshotRun
    } else if (!forceRestart && activeUtxoSnapshotRun.nonEmpty) {
      log.debug("Ignoring UTXO snapshot wallet scan request while another run is active")
      None
    } else if (forceRestart || !terminalUtxoSnapshot.contains(snapshot)) {
      if (forceRestart) terminalUtxoSnapshot = terminalUtxoSnapshot.filterNot(_ == snapshot)
      val run = newUtxoSnapshotRun(snapshotHeight, snapshotBlockId)
      activeUtxoSnapshotRun = Some(run)
      startingUtxoSnapshot = Some(run)
      context.become(loadedWallet(state))
      utxoSnapshotScanner ! StartUtxoSnapshotScan(run, forceRestart)
      Some(run)
    } else {
      None
    }
  }

  private def canScanUtxoSnapshot(state: ErgoWalletState): Boolean =
    state.walletVars.trackedBytes.nonEmpty || state.walletVars.externalScans.nonEmpty

  private final class UtxoSnapshotRecoveryPlan(
    val preparedState: ErgoWalletState,
    val expected: UtxoSnapshotScanInvalidation,
    val freshStatus: UtxoSnapshotScanStatus,
    val stateReader: UtxoStateReader)

  private def requireUtxoSnapshotRecovery(condition: Boolean,
                                          message: => String): Try[Unit] =
    if (condition) Success(()) else fail(message)

  private def prepareUtxoSnapshotRecovery(
    state: ErgoWalletState,
    snapshotHeight: Height,
    snapshotBlockId: ModifierId,
    stateReader: UtxoStateReader): Try[UtxoSnapshotRecoveryPlan] = {
    for {
      quarantine <- utxoSnapshotQuarantine match {
        case Some(current) => Success(current)
        case None => fail("UTXO snapshot recovery requires an active quarantine")
      }
      expected <- quarantine.fence match {
        case Some(fence) => Success(fence)
        case None => fail("UTXO snapshot recovery requires a durable invalidation fence identity")
      }
      _ <- requireUtxoSnapshotRecovery(
        settings.nodeSettings.utxoSettings.utxoBootstrap,
        "UTXO snapshot recovery requires utxoBootstrap")
      _ <- requireUtxoSnapshotRecovery(
        canScanUtxoSnapshot(state),
        "UTXO snapshot recovery requires initialized wallet scan variables")
      _ <- requireUtxoSnapshotRecovery(
        expected.snapshotHeight == snapshotHeight &&
          expected.snapshotBlockId == snapshotBlockId,
        "Applied UTXO snapshot does not match the durable invalidation fence")
      reader <- Option(stateReader) match {
        case Some(current) => Success(current)
        case None => fail("Applied UTXO snapshot state reader is unavailable")
      }
      stateContext <- Option(reader.stateContext) match {
        case Some(current) => Success(current)
        case None => fail("Applied UTXO snapshot state context is unavailable")
      }
      readerHeight <- Try(stateContext.currentHeight)
      readerBlockId <- Try(versionToId(reader.version))
      readerRoot <- Try(reader.rootDigest)
      _ <- requireUtxoSnapshotRecovery(
        readerHeight == snapshotHeight && readerBlockId == snapshotBlockId,
        "Applied UTXO snapshot state reader identity does not match the recovery fence")
      _ <- requireUtxoSnapshotRecovery(
        readerRoot != null,
        "Applied UTXO snapshot state root is unavailable")
      bestHeaderOpt <- Try(readBestHeaderStateAtHeight(snapshotHeight)).flatten
      bestHeader <- bestHeaderOpt match {
        case Some(current) => Success(current)
        case None => fail(s"Best header at UTXO snapshot height $snapshotHeight is unavailable")
      }
      _ <- requireUtxoSnapshotRecovery(
        bestHeader._1 == snapshotBlockId,
        "Applied UTXO snapshot does not match the best header identity")
      _ <- requireUtxoSnapshotRecovery(
        bestHeader._2 != null && java.util.Arrays.equals(readerRoot, bestHeader._2),
        "Applied UTXO snapshot state root does not match the best header")
      source <- Try(readUtxoSnapshotSourceIdentity(snapshotBlockId)).flatten
      _ <- requireUtxoSnapshotRecovery(
        source.snapshotHeight == snapshotHeight && source.snapshotBlockId == snapshotBlockId,
        "Applied UTXO snapshot does not match its immutable source identity")
      _ <- requireUtxoSnapshotRecovery(
        source.manifestDepth >= 0 && source.partCount > 0,
        s"Invalid immutable UTXO snapshot source dimensions: " +
          s"depth=${source.manifestDepth}, parts=${source.partCount}")
      durableFence <- Try(readUtxoSnapshotRecoveryFence(state)).flatten
      _ <- requireUtxoSnapshotRecovery(
        durableFence.contains(expected),
        "Durable UTXO snapshot invalidation fence changed before recovery")
      originOpt <- state.storage.readUtxoSnapshotWalletOriginTry().recoverWith { case t =>
        Failure(new IllegalStateException(
          s"Unreadable durable UTXO snapshot wallet origin during recovery preflight: ${t.getMessage}", t))
      }
      currentParameters <- Try(stateContext.currentParameters)
      updatedWalletVars <- Try(updateUtxoSnapshotRecoveryWalletVars(state, currentParameters)).flatten
      definitionState = state.copy(walletVars = updatedWalletVars)
      freshDefinition <- Try(calculateUtxoSnapshotScanDefinition(definitionState)).flatten
      _ <- requireUtxoSnapshotRecovery(
        originOpt.forall(origin =>
          origin.snapshotHeight == expected.snapshotHeight &&
            origin.snapshotBlockId == expected.snapshotBlockId &&
            origin.scanDefinition == freshDefinition),
        "Durable UTXO snapshot wallet origin identity or definition does not match recovery")
      _ <- Try(persistUtxoSnapshotRecoveryStateContext(state, stateContext)).flatten
      preparedState = state.copy(
        walletVars = updatedWalletVars,
        stateReaderOpt = Some(reader),
        utxoStateReaderOpt = Some(reader),
        parameters = currentParameters,
        rescanInProgress = false)
      freshStatus = UtxoSnapshotScanStatus(
        snapshotHeight,
        snapshotBlockId,
        source.manifestDepth,
        nextSubtreeIndex = 0,
        totalSubtrees = source.partCount,
        completed = false,
        scanDefinition = freshDefinition)
    } yield new UtxoSnapshotRecoveryPlan(preparedState, expected, freshStatus, reader)
  }

  private def remainInUtxoSnapshotQuarantine(
    state: ErgoWalletState,
    detail: String,
    cause: Throwable,
    replyTo: ActorRef): Unit = {
    val reason = s"UTXO snapshot wallet remains quarantined: $detail"
    val fence = utxoSnapshotQuarantine.flatMap(_.fence)
    utxoSnapshotQuarantine = Some(UtxoSnapshotQuarantine(reason, fence))
    invalidUtxoSnapshotProgress = true
    log.error(reason, cause)
    context.become(loadedWallet(state.copy(error = Some(reason), rescanInProgress = false)))
    replyTo ! Failure(cause)
  }

  private def installAndNormalizeUtxoSnapshotRecoveryState(
    plan: UtxoSnapshotRecoveryPlan,
    transferredState: ErgoWalletState): Try[ErgoWalletState] = {
    val quarantineReason = utxoSnapshotQuarantine.map(_.reason)
      .getOrElse("UTXO snapshot wallet is quarantined")
    val installedState = transferredState.copy(
      error = Some(quarantineReason),
      rescanInProgress = false)

    // The reset producer has consumed the input registry. Install ownership of the transferred
    // state before initializing any derived view that can fail.
    context.become(loadedWallet(installedState))
    Try {
      require(transferredState.registry != null,
        "UTXO snapshot registry recreation returned no registry")
      require(!(transferredState.registry eq plan.preparedState.registry),
        "UTXO snapshot registry recreation did not replace the consumed registry")
      installedState.copy(
        offChainRegistry = OffChainRegistry.init(transferredState.registry),
        outputsFilter = None,
        walletVars = plan.preparedState.walletVars,
        stateReaderOpt = Some(plan.stateReader),
        utxoStateReaderOpt = Some(plan.stateReader),
        parameters = plan.preparedState.parameters)
    }
  }

  private def restartAfterUnavailableRegistryReset(
    state: ErgoWalletState,
    cause: Throwable,
    replyTo: ActorRef): Unit = {
    Try(closeWalletStorageForUtxoSnapshotRecovery(state)).flatten match {
      case Failure(closeFailure)
        if (closeFailure ne cause) && !cause.getSuppressed.exists(_ eq closeFailure) =>
        cause.addSuppressed(closeFailure)
      case _ =>
    }
    replyTo ! Failure(cause)
    cause match {
      case exception: Exception => throw exception
      case other => throw new IllegalStateException(
        "UTXO snapshot registry reset became unavailable", other)
    }
  }

  private def recoverQuarantinedUtxoSnapshot(
    state: ErgoWalletState,
    snapshotHeight: Height,
    snapshotBlockId: ModifierId,
    stateReader: UtxoStateReader,
    replyTo: ActorRef): Unit = {
    prepareUtxoSnapshotRecovery(state, snapshotHeight, snapshotBlockId, stateReader) match {
      case Failure(t) =>
        remainInUtxoSnapshotQuarantine(
          state, s"recovery preflight failed: ${t.getMessage}", t, replyTo)
      case Success(plan) =>
        recreateRegistryForUtxoSnapshotRecovery(plan.preparedState) match {
          case RegistryResetUnavailable(cause) =>
            restartAfterUnavailableRegistryReset(plan.preparedState, cause, replyTo)

          case RegistryResetDeferred(transferredState, cause) =>
            installAndNormalizeUtxoSnapshotRecoveryState(plan, transferredState) match {
              case Failure(t) =>
                remainInUtxoSnapshotQuarantine(
                  transferredState, s"registry normalization failed: ${t.getMessage}", t, replyTo)
              case Success(normalizedState) =>
                context.become(loadedWallet(normalizedState))
                remainInUtxoSnapshotQuarantine(
                  normalizedState, s"registry reset deferred: ${cause.getMessage}", cause, replyTo)
            }

          case RegistryResetReady(transferredState, _) =>
            installAndNormalizeUtxoSnapshotRecoveryState(plan, transferredState) match {
              case Failure(t) =>
                remainInUtxoSnapshotQuarantine(
                  transferredState, s"registry normalization failed: ${t.getMessage}", t, replyTo)
              case Success(normalizedState) =>
                context.become(loadedWallet(normalizedState))
                normalizedState.registry.isPristineForUtxoSnapshot.flatMap {
                  case true =>
                    Try(restartUtxoSnapshotScanRecovery(
                      normalizedState, plan.expected, plan.freshStatus)).flatten
                  case false =>
                    fail("Fresh wallet registry is not pristine for UTXO snapshot recovery")
                } match {
                  case Failure(t) =>
                    remainInUtxoSnapshotQuarantine(
                      normalizedState, s"durable recovery restart failed: ${t.getMessage}", t, replyTo)
                  case Success(false) =>
                    val t = new IllegalStateException(
                      "Durable UTXO snapshot invalidation fence no longer matches recovery")
                    remainInUtxoSnapshotQuarantine(
                      normalizedState, t.getMessage, t, replyTo)
                  case Success(true) =>
                    startUtxoSnapshotScan(
                      normalizedState, snapshotHeight, snapshotBlockId, forceRestart = true) match {
                      case None =>
                        val t = new IllegalStateException(
                          "Unable to start the durable UTXO snapshot recovery obligation")
                        remainInUtxoSnapshotQuarantine(
                          normalizedState, t.getMessage, t, replyTo)
                      case Some(run) =>
                        utxoSnapshotQuarantine = None
                        invalidUtxoSnapshotProgress = false
                        terminalUtxoSnapshot = None
                        finalizingUtxoSnapshot = None
                        finalizingUtxoSnapshotStatus = None
                        blockedUtxoSnapshotCatchUp = None
                        deferredSnapshotBlock = None
                        deferredRollbackReconciliationBlocks = Map.empty
                        utxoSnapshotFinalization = UtxoSnapshotFinalizationState.empty
                        context.become(loadedWallet(normalizedState.copy(
                          error = None,
                          rescanInProgress = false)))
                        replyTo ! Success(Some(run))
                    }
                }
            }
        }
    }
  }

  private def maybeStartAvailableUtxoSnapshotScan(state: ErgoWalletState): Unit = {
    val eligible =
      settings.nodeSettings.utxoSettings.utxoBootstrap &&
        settings.nodeSettings.isFullBlocksPruned &&
        historyReader.isUtxoSnapshotApplied &&
        canScanUtxoSnapshot(state) &&
        !state.rescanInProgress &&
        state.storage.readUtxoSnapshotScanStatusTry() == Success(None) &&
        state.storage.readUtxoSnapshotWalletOriginTry() == Success(None)

    if (eligible) {
      state.utxoStateReaderOpt
        .orElse(state.stateReaderOpt.collect { case reader: UtxoStateReader => reader })
        .foreach { reader =>
          state.registry.isPristineForUtxoSnapshot match {
            case Failure(t) =>
              enterUtxoSnapshotQuarantine(
                state,
                s"Unable to verify that the wallet registry is pristine for a UTXO snapshot: ${t.getMessage}",
                fence = None,
                persistFence = false)
            case Success(false) =>
              enterUtxoSnapshotQuarantine(
                state,
                "A UTXO snapshot wallet scan requires a pristine wallet registry",
                fence = None,
                persistFence = false)
            case Success(true) =>
              readAvailableUtxoSnapshotSourceIdentity() match {
                case Failure(t) =>
                  enterUtxoSnapshotQuarantine(
                    state,
                    s"Unable to authenticate the available UTXO snapshot source: ${t.getMessage}",
                    fence = None,
                    persistFence = false)
                case Success(source) =>
                  utxoSnapshotCatchUpHeight(state, source.snapshotHeight) match {
                    case Failure(t) =>
                      enterUtxoSnapshotQuarantine(
                        state,
                        walletCatchUpFailureReason(
                          s"Available UTXO snapshot at height ${source.snapshotHeight}", t),
                        fence = None,
                        persistFence = false)
                    case Success(_) =>
                      startUtxoSnapshotScan(
                        state,
                        source.snapshotHeight,
                        source.snapshotBlockId,
                        forceRestart = false)
                  }
              }
          }
        }
    }
  }

  private def statusMatchesUtxoSnapshotOrigin(status: UtxoSnapshotScanStatus,
                                               origin: UtxoSnapshotWalletOrigin): Boolean =
    status.snapshotHeight == origin.snapshotHeight &&
      status.snapshotBlockId == origin.snapshotBlockId &&
      status.scanDefinition == origin.scanDefinition

  private def validateCompletedUtxoSnapshotOrigin(
    state: ErgoWalletState,
    origin: UtxoSnapshotWalletOrigin): Try[Unit] = for {
    currentStateTip <- readCurrentStateTip(state)
    (currentStateHeight, currentStateId) = currentStateTip
    _ <- if (currentStateHeight >= origin.snapshotHeight) Success(()) else fail(
      s"Completed UTXO snapshot wallet origin at height ${origin.snapshotHeight} is above " +
        s"the current state height $currentStateHeight")
    bestOriginId <- readBestHeaderIdAtHeight(origin.snapshotHeight)
    _ <- if (bestOriginId.contains(origin.snapshotBlockId)) Success(()) else fail(
      s"Completed UTXO snapshot wallet origin ${origin.snapshotBlockId} does not match " +
        s"the best header at height ${origin.snapshotHeight}")
    _ <- if (currentStateHeight == EmptyHistoryHeight && currentStateId.isEmpty) {
      Success(())
    } else {
      readBestHeaderIdAtHeight(currentStateHeight).flatMap { bestStateId =>
        if (currentStateId.nonEmpty && currentStateId == bestStateId) Success(()) else fail(
          s"Completed UTXO snapshot wallet origin is paired with a noncanonical current " +
            s"state tip at height $currentStateHeight")
      }
    }
    registryTip <- readWalletRegistryTip(state)
    (registryHeight, registryId) = registryTip
    _ <- if (registryHeight >= origin.snapshotHeight) {
      Success(())
    } else {
      fail(
        s"Completed UTXO snapshot wallet origin has registry height $registryHeight below " +
          s"snapshot height ${origin.snapshotHeight}")
    }
    bestRegistryId <- readBestHeaderIdAtHeight(registryHeight)
    _ <- if (registryId.nonEmpty && registryId == bestRegistryId) Success(()) else fail(
      s"Completed UTXO snapshot wallet origin is paired with a noncanonical registry tip " +
        s"at height $registryHeight")
  } yield ()

  private def validateLiveUtxoSnapshotScanDefinition(
    state: ErgoWalletState,
    status: UtxoSnapshotScanStatus): Try[Unit] =
    Try(calculateUtxoSnapshotScanDefinition(state)).flatten.flatMap { liveDefinition =>
      if (status.scanDefinition == liveDefinition) {
        Success(())
      } else {
        fail(
          s"Durable UTXO snapshot scan definition does not match the live wallet definition: " +
            s"durable=${status.scanDefinition}, live=$liveDefinition")
      }
    }

  private def validateUtxoSnapshotFinalizationDefinition(
    state: ErgoWalletState,
    status: UtxoSnapshotScanStatus): Try[Unit] =
    validateLiveUtxoSnapshotScanDefinition(state, status).flatMap { _ =>
      state.storage.readUtxoSnapshotWalletOriginTry().flatMap {
        case Some(origin) if !statusMatchesUtxoSnapshotOrigin(status, origin) =>
          fail(
            s"Durable UTXO snapshot scan status conflicts with wallet origin during finalization: " +
              s"status=$status, origin=$origin")
        case _ => Success(())
      }
    }

  private def validatePersistedUtxoSnapshotStatusAfterDefinition(
    state: ErgoWalletState,
    status: UtxoSnapshotScanStatus): Try[Unit] = {
    readUtxoSnapshotSourceIdentity(status.snapshotBlockId).flatMap { source =>
      if (source.snapshotHeight == status.snapshotHeight &&
        source.snapshotBlockId == status.snapshotBlockId &&
        source.manifestDepth == status.manifestDepth &&
        source.partCount == status.totalSubtrees) {
        Success(())
      } else {
        Failure(new IllegalStateException(
          s"Persisted UTXO snapshot progress does not match its immutable source: status=$status, source=$source"))
      }
    }.flatMap { _ =>
      if (!status.completed) {
        Success(())
      } else {
        readWalletRegistryTip(state).flatMap { case (registryHeight, lastVersionId) =>
          if (registryHeight < status.snapshotHeight) {
            Failure(new IllegalStateException(
              s"Completed UTXO snapshot registry tip height $registryHeight is below " +
                s"snapshot height ${status.snapshotHeight}"))
          } else {
            readBestHeaderIdAtHeight(registryHeight).flatMap { bestHeaderId =>
              if (lastVersionId.nonEmpty && lastVersionId == bestHeaderId) {
                Success(())
              } else {
                Failure(new IllegalStateException(
                  s"Completed UTXO snapshot registry tip does not match the best header at height $registryHeight"))
              }
            }
          }
        }
      }
    }
  }

  private def reconcilePersistedUtxoSnapshotCursor(
    state: ErgoWalletState,
    status: UtxoSnapshotScanStatus): Try[UtxoSnapshotScanStatus] = {
    if (status.completed) {
      Success(status)
    } else {
      val batchSize = UtxoSnapshotWalletScanner.SnapshotScanBatchSize
      for {
        _ <- UtxoSnapshotWalletScanner.lastCommittedBatchStatus(status)
        registryCursor <- state.registry.contiguousSnapshotCursor(
          status.snapshotBlockId,
          status.totalSubtrees,
          batchSize)
        maximumCrashCursor = Math.min(
          status.totalSubtrees.toLong,
          status.nextSubtreeIndex.toLong + batchSize.toLong).toInt
        _ <- if (registryCursor < status.nextSubtreeIndex) {
          Failure(new UtxoSnapshotChunkIntegrityException(
            s"UTXO snapshot registry marker cursor $registryCursor is behind durable status " +
              s"${status.nextSubtreeIndex} for ${status.snapshotBlockId}"))
        } else if (registryCursor > maximumCrashCursor) {
          Failure(new UtxoSnapshotChunkIntegrityException(
            s"UTXO snapshot registry marker cursor $registryCursor is more than one batch ahead " +
              s"of durable status ${status.nextSubtreeIndex} for ${status.snapshotBlockId}"))
        } else {
          Success(())
        }
      } yield status
    }
  }

  private def resumeOrStartValidatedUtxoSnapshotScan(state: ErgoWalletState,
                                                      statusOpt: Option[UtxoSnapshotScanStatus]): Unit = {
    statusOpt match {
      case Some(status) if !status.completed &&
        !settings.nodeSettings.utxoSettings.utxoBootstrap =>
        enterUtxoSnapshotQuarantine(
          state,
          "A durable UTXO snapshot scan is pending but utxoBootstrap is disabled",
          fence = Some(UtxoSnapshotScanInvalidation(
            status.snapshotHeight, status.snapshotBlockId)),
          persistFence = true)
      case Some(status) if status.completed =>
        activeUtxoSnapshotRun match {
          case Some(run) if run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId) =>
            if (!finalizingUtxoSnapshot.contains(run)) {
              finalizingUtxoSnapshot = Some(run)
              finalizingUtxoSnapshotStatus = Some(status)
              self ! FinalizeUtxoSnapshotScan(run, status)
            }
          case Some(_) =>
            log.debug("Ignoring completed UTXO snapshot status while another run is active")
          case None =>
            val run = newUtxoSnapshotRun(status.snapshotHeight, status.snapshotBlockId)
            activeUtxoSnapshotRun = Some(run)
            finalizingUtxoSnapshot = Some(run)
            finalizingUtxoSnapshotStatus = Some(status)
            self ! FinalizeUtxoSnapshotScan(run, status)
        }
      case Some(status) =>
        if (canScanUtxoSnapshot(state)) {
          utxoSnapshotCatchUpHeight(state, status.snapshotHeight) match {
            case Failure(t) =>
              enterUtxoSnapshotQuarantine(
                state,
                walletCatchUpFailureReason(
                  s"Persisted UTXO snapshot progress at height ${status.snapshotHeight}", t),
                fence = None,
                persistFence = false)
            case Success(_) =>
              startUtxoSnapshotScan(
                state, status.snapshotHeight, status.snapshotBlockId, forceRestart = false)
          }
        }
      case None =>
        if (settings.nodeSettings.utxoSettings.utxoBootstrap && canScanUtxoSnapshot(state)) {
          maybeStartAvailableUtxoSnapshotScan(state)
        }
    }
  }

  private def resumeOrStartUtxoSnapshotScan(state: ErgoWalletState): Unit = {
    state.storage.readUtxoSnapshotScanInvalidationTry() match {
      case Failure(t) =>
        enterUtxoSnapshotQuarantine(
          state,
          s"Unreadable durable UTXO snapshot invalidation fence: ${t.getMessage}",
          fence = None,
          persistFence = false)
      case Success(Some(invalidation)) =>
        enterUtxoSnapshotQuarantine(
          state,
          s"Durable UTXO snapshot invalidation fence is present for " +
            s"${invalidation.snapshotBlockId} at height ${invalidation.snapshotHeight}",
          fence = Some(invalidation),
          persistFence = false)
      case Success(None) =>
        readUtxoSnapshotWalletOrigin(state) match {
          case Failure(t) =>
            enterUtxoSnapshotQuarantine(
              state,
              s"Unreadable durable UTXO snapshot wallet origin: ${t.getMessage}",
              fence = None,
              persistFence = false)
          case Success(originOpt) =>
            val statusResult = state.storage.readUtxoSnapshotScanStatusTry()
            val originValidation = statusResult match {
              case Failure(_) => Success(())
              case Success(Some(status)) if !status.completed =>
                originOpt match {
                  case Some(origin) if statusMatchesUtxoSnapshotOrigin(status, origin) => Success(())
                  case Some(origin) => fail(
                    s"Durable UTXO snapshot scan status conflicts with completed wallet origin: " +
                      s"status=$status, origin=$origin")
                  case None => Success(())
                }
              case Success(_) =>
                originOpt
                  .map(validateCompletedUtxoSnapshotOrigin(state, _))
                  .getOrElse(Success(()))
            }
            (originValidation, statusResult) match {
          case (Failure(t), _) =>
            val origin = originOpt.get
            enterUtxoSnapshotQuarantine(
              state,
              t.getMessage,
              fence = Some(UtxoSnapshotScanInvalidation(
                origin.snapshotHeight, origin.snapshotBlockId)),
              persistFence = true)
          case (Success(_), Failure(t)) =>
            enterUtxoSnapshotQuarantine(
              state,
              s"Unreadable durable UTXO snapshot scan status: ${t.getMessage}",
              fence = None,
              persistFence = false)
          case (Success(_), Success(Some(status)))
            if activeUtxoSnapshotRun.nonEmpty &&
              !activeUtxoSnapshotRun.exists(
                ErgoWalletActor.statusBelongsToActiveRun(_, status)) =>
            enterUtxoSnapshotQuarantine(
              state,
              s"Durable UTXO snapshot progress for ${status.snapshotBlockId} " +
                s"at height ${status.snapshotHeight} does not match the active run",
              fence = Some(UtxoSnapshotScanInvalidation(
                status.snapshotHeight, status.snapshotBlockId)),
              persistFence = true)
          case (Success(_), Success(Some(status))) =>
            validateLiveUtxoSnapshotScanDefinition(state, status) match {
              case Failure(t) =>
                enterUtxoSnapshotQuarantine(
                  state,
                  t.getMessage,
                  fence = Some(UtxoSnapshotScanInvalidation(
                    status.snapshotHeight, status.snapshotBlockId)),
                  persistFence = true)
              case Success(_)
                if originOpt.exists(origin => !statusMatchesUtxoSnapshotOrigin(status, origin)) =>
                enterUtxoSnapshotQuarantine(
                  state,
                  s"Durable UTXO snapshot scan status conflicts with completed wallet origin: " +
                    s"status=$status, origin=${originOpt.get}",
                  fence = Some(UtxoSnapshotScanInvalidation(
                    status.snapshotHeight, status.snapshotBlockId)),
                  persistFence = true)
              case Success(_)
                if activeUtxoSnapshotRun.exists(
                  ErgoWalletActor.statusBelongsToActiveRun(_, status)) =>
                val activeRun = activeUtxoSnapshotRun.get
                if (ErgoWalletActor.shouldResumeCompletedActiveRun(
                  activeRun, status, finalizingUtxoSnapshot)) {
                  resumeOrStartValidatedUtxoSnapshotScan(state, Some(status))
                } else {
                  log.debug("UTXO snapshot wallet scan is already owned by the active actor run")
                }
              case Success(_) =>
                validatePersistedUtxoSnapshotStatusAfterDefinition(state, status)
                  .flatMap(_ => reconcilePersistedUtxoSnapshotCursor(state, status)) match {
                  case Failure(t) =>
                    enterUtxoSnapshotQuarantine(
                      state,
                      t.getMessage,
                      fence = Some(UtxoSnapshotScanInvalidation(
                        status.snapshotHeight, status.snapshotBlockId)),
                      persistFence = true)
                  case Success(reconciledStatus) if reconciledStatus.completed && originOpt.isEmpty =>
                    state.storage.completeUtxoSnapshotScan(reconciledStatus) match {
                      case Success(_) => resumeOrStartValidatedUtxoSnapshotScan(state, Some(reconciledStatus))
                      case Failure(t) =>
                        enterUtxoSnapshotQuarantine(
                          state,
                          s"Unable to backfill completed UTXO snapshot wallet origin: ${t.getMessage}",
                          fence = Some(UtxoSnapshotScanInvalidation(
                            status.snapshotHeight, status.snapshotBlockId)),
                          persistFence = true)
                    }
                  case Success(reconciledStatus) =>
                    resumeOrStartValidatedUtxoSnapshotScan(state, Some(reconciledStatus))
                }
              }
          case (Success(_), Success(None)) if originOpt.nonEmpty && activeUtxoSnapshotRun.isEmpty =>
            log.debug("Completed UTXO snapshot wallet origin is present; no scan resume is required")
            startUtxoSnapshotSourceCleanup(originOpt.get)
          case (Success(_), Success(None))
            if activeUtxoSnapshotRun.exists(startingUtxoSnapshot.contains) =>
            log.debug("UTXO snapshot wallet scan status is not initialized for the starting actor run")
          case (Success(_), Success(None)) if activeUtxoSnapshotRun.nonEmpty =>
            val active = activeUtxoSnapshotRun.get
            enterUtxoSnapshotQuarantine(
              state,
              "Durable UTXO snapshot progress disappeared while a run was active",
              Some(UtxoSnapshotScanInvalidation(
                active.snapshotHeight, active.snapshotBlockId)),
              persistFence = true)
          case (Success(_), Success(None)) =>
            resumeOrStartValidatedUtxoSnapshotScan(state, None)
          }
        }
    }
  }

  private def finalizeUtxoSnapshotScan(status: UtxoSnapshotScanStatus,
                                       state: ErgoWalletState,
                                       run: UtxoSnapshotScanRun): Unit = {
    validateUtxoSnapshotFinalizationDefinition(state, status) match {
      case Failure(t) =>
        enterUtxoSnapshotQuarantine(
          state,
          t.getMessage,
          fence = Some(UtxoSnapshotScanInvalidation(
            status.snapshotHeight, status.snapshotBlockId)),
          persistFence = true)
      case Success(_) =>
        startingUtxoSnapshot = None
        terminalUtxoSnapshot = None
        val completedHeight = Math.max(status.snapshotHeight, state.getWalletHeight)
        utxoSnapshotCatchUpHeight(state, completedHeight) match {
          case Failure(t: WalletCatchUpBlockUnavailable)
            if isWalletCatchUpBlockDefinitelyPruned(t) =>
            utxoSnapshotFinalization =
              utxoSnapshotFinalization.catchUpFailed(status.snapshotBlockId)
            enterUtxoSnapshotQuarantine(
              state,
              walletCatchUpPruningReason(
                s"Completed UTXO snapshot at height ${status.snapshotHeight}", t),
              fence = None,
              persistFence = false)
          case Failure(t: WalletCatchUpBlockUnavailable) =>
            blockRecoverableUtxoSnapshotCatchUp(
              state, run, "readiness check", t)
          case Failure(t) =>
            blockRecoverableUtxoSnapshotCatchUp(
              state, run, "readiness check", t)
          case Success(catchUpHeightOpt) =>
            val plan = utxoSnapshotFinalization.plan(status, catchUpReady = true)
            utxoSnapshotFinalization = plan.state
            if (plan.scheduleCatchUp) {
              catchUpHeightOpt match {
                case Some(catchUpHeight) =>
                  self ! ContinueUtxoSnapshotCatchUp(run, catchUpHeight)
                case None =>
                  completeUtxoSnapshotFinalization(state, run, status, cleanupAttempt = 0)
              }
            } else if (plan.tryCleanup) {
              completeUtxoSnapshotFinalization(state, run, status, cleanupAttempt = 0)
            }
        }
    }
  }

  private def completeUtxoSnapshotFinalization(state: ErgoWalletState,
                                                run: UtxoSnapshotScanRun,
                                                status: UtxoSnapshotScanStatus,
                                                cleanupAttempt: Int): Unit = {
    if (!finalizingUtxoSnapshot.contains(run) ||
      !run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId)) {
      log.debug("Ignoring stale UTXO snapshot cleanup completion")
    } else {
      validateUtxoSnapshotFinalizationDefinition(state, status) match {
        case Failure(t) =>
          enterUtxoSnapshotQuarantine(
            state,
            t.getMessage,
            fence = Some(UtxoSnapshotScanInvalidation(
              status.snapshotHeight, status.snapshotBlockId)),
            persistFence = true)
        case Success(_) =>
      startingUtxoSnapshot = None
      terminalUtxoSnapshot = None
      state.storage.readUtxoSnapshotScanInvalidationTry() match {
        case Failure(t) =>
          enterUtxoSnapshotQuarantine(
            state,
            s"Unreadable durable UTXO snapshot invalidation fence during finalization: ${t.getMessage}",
            fence = Some(UtxoSnapshotScanInvalidation(
              status.snapshotHeight, status.snapshotBlockId)),
            persistFence = false)
        case Success(Some(invalidation)) =>
          enterUtxoSnapshotQuarantine(
            state,
            s"Durable UTXO snapshot invalidation fence appeared during finalization for " +
              s"${invalidation.snapshotBlockId} at height ${invalidation.snapshotHeight}",
            fence = Some(invalidation),
            persistFence = false)
        case Success(None) =>
          state.storage.readUtxoSnapshotScanStatusTry() match {
            case Failure(t) =>
              enterUtxoSnapshotQuarantine(
                state,
                s"Unreadable durable UTXO snapshot scan status during finalization: ${t.getMessage}",
                fence = Some(UtxoSnapshotScanInvalidation(
                  status.snapshotHeight, status.snapshotBlockId)),
                persistFence = true)
            case Success(Some(currentStatus)) if currentStatus != status =>
              enterUtxoSnapshotQuarantine(
                state,
                "Durable UTXO snapshot scan status changed during finalization",
                fence = Some(UtxoSnapshotScanInvalidation(
                  currentStatus.snapshotHeight, currentStatus.snapshotBlockId)),
                persistFence = true)
            case Success(None) =>
              enterUtxoSnapshotQuarantine(
                state,
                "Durable UTXO snapshot scan status disappeared during finalization",
                fence = Some(UtxoSnapshotScanInvalidation(
                  status.snapshotHeight, status.snapshotBlockId)),
                persistFence = true)
            case Success(Some(_)) =>
              deferredSnapshotBlock.filter(_._1 > state.getWalletHeight) match {
                case Some((deferredHeight, _)) =>
                  requiredWalletCatchUpHeight(state.getWalletHeight, deferredHeight) match {
                    case Failure(t: WalletCatchUpBlockUnavailable)
                      if isWalletCatchUpBlockDefinitelyPruned(t) =>
                      enterUtxoSnapshotQuarantine(
                        state,
                        walletCatchUpPruningReason(
                          s"Deferred post-snapshot block at height $deferredHeight", t),
                        fence = None,
                        persistFence = false)
                    case Failure(t: WalletCatchUpBlockUnavailable) =>
                      blockRecoverableUtxoSnapshotCatchUp(
                        state, run, "deferred-block readiness check", t)
                    case Failure(t) =>
                      blockRecoverableUtxoSnapshotCatchUp(
                        state, run, "deferred-block readiness check", t)
                    case Success(Some(catchUpHeight)) =>
                      self ! ContinueUtxoSnapshotCatchUp(run, catchUpHeight, cleanupAttempt)
                    case Success(None) =>
                      persistCompletedUtxoSnapshotFinalization(
                        state, run, status, cleanupAttempt)
                  }
                case None =>
                  persistCompletedUtxoSnapshotFinalization(
                    state, run, status, cleanupAttempt)
              }
          }
      }
      }
    }
  }

  private def persistCompletedUtxoSnapshotFinalization(
    state: ErgoWalletState,
    run: UtxoSnapshotScanRun,
    status: UtxoSnapshotScanStatus,
    cleanupAttempt: Int): Unit = {
    val completionTry = for {
      reconciledState <- reconcileAfterMandatoryWalletCatchUp(state)
      _ <- reconciledState.storage.completeUtxoSnapshotScan(status)
      _ <- Try(removeUtxoSnapshotScanStatus(reconciledState)).flatten
    } yield reconciledState

    completionTry match {
      case Failure(t) =>
        val message =
          s"Unable to persist completed UTXO snapshot origin or remove completed UTXO snapshot scan status: ${t.getMessage}"
        log.error(message, t)
        context.become(loadedWallet(state.copy(error = Some(message))))
        if (cleanupAttempt < ErgoWalletActor.MaxFinalizationCleanupRetries) {
          context.system.scheduler.scheduleOnce(
            ErgoWalletActor.FinalizationCleanupRetryDelay,
            self,
            FinalizeUtxoSnapshotScan(run, status, cleanupAttempt + 1))(
            context.dispatcher, self)
        }
      case Success(reconciledState) =>
        utxoSnapshotFinalization =
          utxoSnapshotFinalization.catchUpCompleted(status.snapshotBlockId)
        finalizingUtxoSnapshot = None
        finalizingUtxoSnapshotStatus = None
        activeUtxoSnapshotRun = None
        blockedUtxoSnapshotCatchUp = None
        deferredSnapshotBlock = None
        deferredRollbackReconciliationBlocks = Map.empty
        val completedState = reconciledState.copy(error = None, rescanInProgress = false)
        val origin = UtxoSnapshotWalletOrigin(
          status.snapshotHeight, status.snapshotBlockId, status.scanDefinition)
        startUtxoSnapshotSourceCleanup(origin)
        context.become(loadedWallet(completedState))
    }
  }

  private def startUtxoSnapshotSourceCleanup(origin: UtxoSnapshotWalletOrigin): Unit = {
    val (nextState, shouldStart) =
      utxoSnapshotFinalization.claimSourceCleanup(origin.snapshotBlockId)
    utxoSnapshotFinalization = nextState
    if (shouldStart) {
      self ! RetryUtxoSnapshotSourceCleanup(origin, attempt = 0)
    }
  }

  private def validateUtxoSnapshotSourceCleanup(
    state: ErgoWalletState,
    expectedOrigin: UtxoSnapshotWalletOrigin): Try[Unit] = for {
    invalidationOpt <- state.storage.readUtxoSnapshotScanInvalidationTry()
    _ <- invalidationOpt match {
      case None => Success(())
      case Some(invalidation) => fail(
        s"Durable UTXO snapshot invalidation fence is present for " +
          s"${invalidation.snapshotBlockId} at height ${invalidation.snapshotHeight}")
    }
    statusOpt <- state.storage.readUtxoSnapshotScanStatusTry()
    _ <- statusOpt match {
      case None => Success(())
      case Some(status) => fail(
        s"Durable UTXO snapshot scan status is still present for ${status.snapshotBlockId}")
    }
    originOpt <- readUtxoSnapshotWalletOrigin(state)
    _ <- originOpt match {
      case Some(origin) if origin == expectedOrigin => Success(())
      case Some(origin) => fail(
        s"Durable UTXO snapshot wallet origin changed before source cleanup: " +
          s"expected=$expectedOrigin, current=$origin")
      case None => fail("Durable UTXO snapshot wallet origin disappeared before source cleanup")
    }
  } yield ()

  private def retryUtxoSnapshotSourceCleanup(
    state: ErgoWalletState,
    expectedOrigin: UtxoSnapshotWalletOrigin,
    attempt: Int): Unit = {
    val snapshotBlockId = expectedOrigin.snapshotBlockId
    validateUtxoSnapshotSourceCleanup(state, expectedOrigin)
      .flatMap(_ => removeUtxoSnapshotScanSource(snapshotBlockId)) match {
      case Success(_) =>
        val cleanupError = utxoSnapshotSourceCleanupErrors.get(snapshotBlockId)
        utxoSnapshotSourceCleanupErrors -= snapshotBlockId
        utxoSnapshotFinalization =
          utxoSnapshotFinalization.cleanupSucceeded(snapshotBlockId)
        val cleanedState = if (cleanupError.exists(message => state.error.contains(message))) {
          state.copy(error = None)
        } else {
          state
        }
        context.become(loadedWallet(cleanedState))
      case Failure(t) =>
        val message =
          s"Unable to remove completed UTXO snapshot scan source: ${t.getMessage}"
        utxoSnapshotSourceCleanupErrors += snapshotBlockId -> message
        log.error(message, t)
        context.become(loadedWallet(state.copy(error = Some(message))))
        if (attempt < ErgoWalletActor.MaxFinalizationCleanupRetries) {
          context.system.scheduler.scheduleOnce(
            ErgoWalletActor.FinalizationCleanupRetryDelay,
            self,
            RetryUtxoSnapshotSourceCleanup(expectedOrigin, attempt + 1))(
            context.dispatcher, self)
        } else {
          utxoSnapshotFinalization =
            utxoSnapshotFinalization.sourceCleanupRetryExhausted(snapshotBlockId)
        }
    }
  }

  private def fail[T](message: String): Failure[T] =
    Failure(new IllegalStateException(message))

  private def activeQuarantineReason(state: ErgoWalletState): String =
    utxoSnapshotQuarantine.map(_.reason)
      .orElse(walletRollbackPreparationReason)
      .orElse(rollbackReconciliationQuarantine)
      .orElse(operationalMempoolReconciliationQuarantine)
      .orElse(state.error)
      .getOrElse("Wallet is quarantined")

  private def walletRollbackIntent(version: VersionTag): Try[WalletRollbackIntent] = {
    val targetVersionId = versionToId(version)
    if (targetVersionId == PreGenesisHeader.id) {
      Success(WalletRollbackIntent(targetVersionId, EmptyHistoryHeight))
    } else {
      Try(historyReader.heightOf(targetVersionId)).flatMap {
        case Some(height) if height >= EmptyHistoryHeight =>
          Success(WalletRollbackIntent(targetVersionId, height))
        case Some(height) =>
          fail(s"Wallet rollback target $targetVersionId has invalid height $height")
        case None =>
          fail(s"Wallet rollback target $targetVersionId has unknown height")
      }
    }
  }

  private def readCanonicalWalletRollbackTargetId(intent: WalletRollbackIntent): Try[ModifierId] = {
    if (intent.targetVersionId == PreGenesisHeader.id) {
      if (intent.expectedHeight == EmptyHistoryHeight) Success(intent.targetVersionId) else fail(
        s"Pre-genesis wallet rollback target has height ${intent.expectedHeight}")
    } else {
      readBestHeaderIdAtHeight(intent.expectedHeight).flatMap {
        case Some(bestId) => Success(bestId)
        case None => fail(
          s"Wallet rollback target ${intent.targetVersionId} has no best header " +
            s"at height ${intent.expectedHeight}")
      }
    }
  }

  private def nonCanonicalWalletRollbackTargetFailure(
    intent: WalletRollbackIntent,
    bestId: ModifierId): Throwable =
    new IllegalStateException(
      s"Wallet rollback target ${intent.targetVersionId} is not the best header $bestId " +
        s"at height ${intent.expectedHeight}")

  private def validateCanonicalWalletRollbackTarget(intent: WalletRollbackIntent): Try[Unit] =
    readCanonicalWalletRollbackTargetId(intent).flatMap { bestId =>
      if (bestId == intent.targetVersionId) Success(())
      else Failure(nonCanonicalWalletRollbackTargetFailure(intent, bestId))
    }

  private def validateCanonicalWalletStateTip(
    tip: (Height, Option[ModifierId])): Try[Unit] = tip match {
    case (EmptyHistoryHeight, None) => Success(())
    case (height, None) =>
      fail(s"Current state at height $height has no header id")
    case (height, Some(_)) if height < EmptyHistoryHeight =>
      fail(s"Current state tip has invalid height $height")
    case (height, Some(stateId)) =>
      readBestHeaderIdAtHeight(height).flatMap {
        case Some(bestId) if bestId == stateId => Success(())
        case Some(bestId) => fail(
          s"Current state tip $stateId is not the best header $bestId at height $height")
        case None => fail(s"Current state tip $stateId has no best header at height $height")
      }
  }

  private def walletTipsAreAligned(
    registryTip: (Height, Option[ModifierId]),
    stateTip: (Height, Option[ModifierId])): Boolean = (registryTip, stateTip) match {
    case ((EmptyHistoryHeight, Some(registryId)), (EmptyHistoryHeight, None))
      if registryId == PreGenesisHeader.id => true
    case ((registryHeight, registryId), (stateHeight, stateId)) =>
      registryHeight == stateHeight && registryId == stateId
  }

  private def isCanonicalWalletRegistryTip(
    tip: (Height, Option[ModifierId])): Try[Boolean] = tip match {
    case (EmptyHistoryHeight, Some(registryId)) if registryId == PreGenesisHeader.id =>
      Success(true)
    case (height, None) =>
      fail(s"Wallet registry at height $height has no version id")
    case (height, Some(_)) if height < EmptyHistoryHeight =>
      fail(s"Wallet registry tip has invalid height $height")
    case (height, Some(registryId)) =>
      readBestHeaderIdAtHeight(height).flatMap {
        case Some(bestId) => Success(bestId == registryId)
        case None => fail(s"Wallet registry tip $registryId has no best header at height $height")
      }
  }

  private def verifyWalletRollbackTarget(
    state: ErgoWalletState,
    intent: WalletRollbackIntent): Try[Unit] =
    readWalletRegistryTip(state).flatMap { case (height, versionId) =>
      if (height == intent.expectedHeight && versionId.contains(intent.targetVersionId)) {
        Success(())
      } else {
        fail(
          s"Wallet registry tip ($height, $versionId) does not match durable rollback target " +
            s"(${intent.expectedHeight}, ${intent.targetVersionId})")
      }
    }

  private def canonicalRetainedWalletRollbackAncestor(
    state: ErgoWalletState,
    staleIntent: WalletRollbackIntent,
    maximumHeight: Height): Try[WalletRollbackIntent] =
    readWalletRegistryRollbackVersionIds(state).flatMap { versionIds =>
      versionIds.foldLeft(Try(Vector.empty[WalletRollbackIntent])) { (accTry, versionId) =>
        accTry.flatMap { candidates =>
          val heightTry = if (versionId == PreGenesisHeader.id) {
            Success(Some(EmptyHistoryHeight))
          } else {
            Try(historyReader.heightOf(versionId))
          }
          heightTry.flatMap {
            case Some(height) if height < EmptyHistoryHeight =>
              fail(s"Retained wallet registry version $versionId has invalid height $height")
            case Some(height)
              if height < staleIntent.expectedHeight && height <= maximumHeight =>
              if (versionId == PreGenesisHeader.id) {
                Success(candidates :+ WalletRollbackIntent(versionId, height))
              } else {
                readBestHeaderIdAtHeight(height).map {
                  case Some(bestId) if bestId == versionId =>
                    candidates :+ WalletRollbackIntent(versionId, height)
                  case _ => candidates
                }
              }
            case _ => Success(candidates)
          }
        }
      }.flatMap { candidates =>
        candidates.sortBy(_.expectedHeight).lastOption match {
          case Some(ancestor) => Success(ancestor)
          case None => fail(
            s"No retained canonical wallet registry ancestor exists below stale rollback target " +
              s"${staleIntent.targetVersionId} at height ${staleIntent.expectedHeight} and at or " +
              s"below current state height $maximumHeight")
        }
      }
    }

  private def recoverStaleDurableWalletRollback(
    state: ErgoWalletState,
    staleIntent: WalletRollbackIntent,
    canonicalFailure: Throwable): Unit =
    verifyWalletRollbackTarget(state, staleIntent) match {
      case Failure(tipFailure) =>
        enterIndeterminateRollbackQuarantine(
          state,
          staleIntent.targetVersionId,
          new IllegalStateException(
            s"${canonicalFailure.getMessage}; stale-target recovery was refused because " +
              tipFailure.getMessage,
            canonicalFailure))
      case Success(_) =>
        val preparationTry = for {
          stateTip <- readCurrentStateTip(state)
          replacement <- canonicalRetainedWalletRollbackAncestor(
            state, staleIntent, stateTip._1)
          plan <- preflightWalletRollbackCatchUp(state, replacement)
          _ <- validateWalletRollbackCatchUpPlan(state, replacement, plan)
        } yield replacement -> plan

        preparationTry match {
          case Failure(t) =>
            enterRollbackPreparationQuarantine(
              state, idToVersion(staleIntent.targetVersionId), t)
          case Success((replacement, plan)) =>
            replaceWalletRollbackIntent(state, staleIntent, replacement) match {
              case Failure(t) =>
                enterIndeterminateRollbackQuarantine(state, staleIntent.targetVersionId, t)
              case Success(false) =>
                enterIndeterminateRollbackQuarantine(
                  state,
                  staleIntent.targetVersionId,
                  new IllegalStateException(
                    "Durable rollback intent changed before stale-target recovery could replace it"))
              case Success(true) =>
                activeRollbackIntent = Some(replacement)
                val version = idToVersion(replacement.targetVersionId)
                validateWalletRollbackCatchUpPlan(state, replacement, plan) match {
                  case Failure(t) =>
                    enterRollbackPreparationQuarantine(state, version, t)
                  case Success(_) =>
                    completeWalletRollbackAttempt(
                      state, version, replacement, rollbackWalletRegistry(state, version))
                }
            }
        }
    }

  private def quarantineRollbackState(
    state: ErgoWalletState,
    reason: String,
    requiresRestart: Boolean,
    cause: Throwable,
    clearFreshMempool: Boolean): Unit = {
    pendingWalletRollbackPreparation = None
    walletRollbackPreparationReason = None
    rollbackReconciliationQuarantine = Some(reason)
    rollbackFailureRequiresRestart = requiresRestart
    if (requiresRestart) {
      rollbackRecoveryContinuation = None
      failDeferredStartupInputs(new IllegalStateException(reason, cause))
    }
    pendingWalletCatchUpTarget = None
    log.error(reason, cause)
    context.become(loadedWallet(state.copy(
      offChainRegistry = OffChainRegistry.empty,
      outputsFilter = None,
      mempoolReaderOpt = if (clearFreshMempool) None else state.mempoolReaderOpt,
      error = Some(reason),
      rescanInProgress = false)))
  }

  private def isRecoverableWalletRollbackPreparationFailure(cause: Throwable): Boolean =
    cause match {
      case _: WalletCatchUpBlockUnavailable => true
      case _: WalletRollbackEvidencePending => true
      case _ => false
    }

  private def enterWalletStartupAlignmentQuarantine(
    state: ErgoWalletState,
    cause: Throwable): Unit = {
    startupNoIntentAlignmentPending = false
    val reason =
      s"Wallet is quarantined because startup canonical alignment is indeterminate; " +
        s"a node restart is required: ${cause.getMessage}"
    quarantineRollbackState(
      state, reason, requiresRestart = true, cause, clearFreshMempool = true)
  }

  private def finishWalletStartupAlignment(state: ErgoWalletState): Unit = {
    startupNoIntentAlignmentPending = false
    resumeOrStartUtxoSnapshotScan(state)
    if (utxoSnapshotQuarantine.isEmpty) {
      context.become(loadedWallet(state))
      replayStartupDeferredMempool()
      scheduleWalletCatchUpIfNeeded(state, deferredSnapshotBlock.map(_._1))
    } else {
      replayStartupDeferredMempool()
    }
    maybeReplayStartupSnapshotEvent()
  }

  private def durableUtxoSnapshotLifecycleNeedsValidation(
    state: ErgoWalletState): Boolean = {
    def presentOrUnreadable[A](read: => Try[Option[A]]): Boolean =
      Try(read).flatten match {
        case Success(None) => false
        case _ => true
      }

    presentOrUnreadable(readUtxoSnapshotRecoveryFence(state)) ||
      presentOrUnreadable(state.storage.readUtxoSnapshotScanStatusTry()) ||
      presentOrUnreadable(readUtxoSnapshotWalletOrigin(state))
  }

  private def alignWalletAtStartup(state: ErgoWalletState): Unit = {
    val canonicalState = for {
      durableIntent <- readWalletRollbackIntent(state)
      _ <- if (durableIntent.isEmpty) Success(()) else fail(
        s"A durable rollback intent appeared during startup alignment: $durableIntent")
      stateTip <- readCurrentStateTip(state)
      _ <- validateCanonicalWalletStateTip(stateTip)
    } yield stateTip

    canonicalState match {
      case Failure(t) =>
        enterWalletStartupAlignmentQuarantine(state, t)
      case Success(stateTip) if durableUtxoSnapshotLifecycleNeedsValidation(state) =>
        startupCanonicalStateTip = Some(stateTip)
        // Snapshot batches use synthetic registry versions. Let the durable snapshot
        // lifecycle validate and resume them instead of treating them as block headers.
        finishWalletStartupAlignment(state)
      case Success(stateTip) =>
        startupCanonicalStateTip = Some(stateTip)
        val alignment = readWalletRegistryTip(state).flatMap { registryTip =>
          registryTip._2 match {
            case None => fail[(
              (Height, Option[ModifierId]), Option[WalletRollbackIntent], Boolean)](
              s"Wallet registry at height ${registryTip._1} has no version id")
            case Some(_) if walletTipsAreAligned(registryTip, stateTip) =>
              Success((registryTip, None, false))
            case Some(registryId) =>
              isCanonicalWalletRegistryTip(registryTip).map {
                case true if registryTip._1 > stateTip._1 =>
                  (registryTip, None, true)
                case true =>
                  (registryTip, None, false)
                case false =>
                  (registryTip,
                    Some(WalletRollbackIntent(registryId, registryTip._1)), false)
              }
          }
        }

        alignment match {
          case Failure(t) =>
            enterWalletStartupAlignmentQuarantine(state, t)
          case Success((registryTip, None, true)) =>
            log.info(
              s"Waiting for canonical state tip $stateTip to catch up with canonical wallet " +
                s"registry tip $registryTip before completing startup alignment")
            context.become(loadedWallet(state))
          case Success((_, None, false)) =>
            finishWalletStartupAlignment(state)
          case Success((registryTip, Some(staleIntent), false)) =>
            writeWalletRollbackIntent(state, staleIntent) match {
              case Failure(t) =>
                enterWalletStartupAlignmentQuarantine(state, t)
              case Success(_) =>
                startupNoIntentAlignmentPending = false
                activeRollbackIntent = Some(staleIntent)
                rollbackRecoveryResumeSnapshotLifecycle = true
                recoverStaleDurableWalletRollback(
                  state,
                  staleIntent,
                  new IllegalStateException(
                    s"Wallet registry tip $registryTip is not aligned with current canonical " +
                      s"state tip $stateTip"))
                replayStartupDeferredMempool()
                maybeReplayStartupSnapshotEvent()
            }
          case Success((_, Some(_), true)) =>
            enterWalletStartupAlignmentQuarantine(
              state,
              new IllegalStateException(
                "Invalid startup alignment plan combined rollback recovery with state catch-up"))
        }
    }
  }

  private def enterWalletRollbackPreparationPending(
    state: ErgoWalletState,
    request: PendingWalletRollbackPreparation,
    cause: Throwable): Unit = {
    val reason =
      s"Wallet rollback preparation is waiting for canonical state/full-block evidence before " +
        s"registry mutation: ${cause.getMessage}"
    pendingWalletRollbackPreparation = Some(request)
    walletRollbackPreparationReason = Some(reason)
    rollbackFailureRequiresRestart = false
    activeRollbackIntent = None
    pendingWalletCatchUpTarget = None
    log.warn(reason)
    context.become(loadedWallet(state.copy(
      offChainRegistry = OffChainRegistry.empty,
      outputsFilter = None,
      mempoolReaderOpt = request.freshMempoolReader,
      error = Some(reason),
      rescanInProgress = false)))
  }

  private def enterRollbackPendingFreshMempool(
    state: ErgoWalletState,
    intent: WalletRollbackIntent): Unit = {
    pendingWalletRollbackPreparation = None
    walletRollbackPreparationReason = None
    activeRollbackIntent = Some(intent)
    val quarantineReason =
      s"Wallet rollback to ${intent.targetVersionId} at height ${intent.expectedHeight} is " +
        "pending a fresh mempool reconciliation"
    rollbackReconciliationQuarantine = Some(quarantineReason)
    rollbackFailureRequiresRestart = false
    log.warn(quarantineReason)
    context.become(loadedWallet(state.copy(
      offChainRegistry = OffChainRegistry.empty,
      outputsFilter = None,
      mempoolReaderOpt = None,
      error = Some(quarantineReason),
      rescanInProgress = false)))
  }

  private def enterRollbackReconciliationQuarantine(
    state: ErgoWalletState,
    cause: Throwable): Unit = {
    val quarantineReason =
      s"Wallet is quarantined because off-chain state reconciliation failed after registry rollback: ${cause.getMessage}"
    quarantineRollbackState(
      state, quarantineReason, requiresRestart = false, cause, clearFreshMempool = false)
  }

  private def enterIndeterminateRollbackQuarantine(
    state: ErgoWalletState,
    target: Any,
    cause: Throwable): Unit = {
    val quarantineReason =
      s"Wallet is quarantined because registry rollback to $target is indeterminate; " +
        s"a node restart is required: ${cause.getMessage}"
    activeUtxoSnapshotRun.foreach(utxoSnapshotScanner ! AbortUtxoSnapshotScan(_))
    deferredRollbackReconciliationBlocks = Map.empty
    quarantineRollbackState(
      state, quarantineReason, requiresRestart = true, cause, clearFreshMempool = true)
  }

  private def enterRollbackPreparationQuarantine(
    state: ErgoWalletState,
    version: VersionTag,
    cause: Throwable): Unit = {
    val surfacedCause = cause match {
      case unavailable: WalletCatchUpBlockUnavailable
        if isWalletCatchUpBlockDefinitelyPruned(unavailable) =>
        new IllegalStateException(
          walletCatchUpPruningReason("Required wallet rollback catch-up", cause), cause)
      case _ => cause
    }
    val quarantineReason =
      s"Wallet rollback to version $version was refused before registry mutation: ${surfacedCause.getMessage}"
    deferredRollbackReconciliationBlocks = Map.empty
    quarantineRollbackState(
      state, quarantineReason, requiresRestart = true, surfacedCause, clearFreshMempool = true)
  }

  private def completeWalletRollbackAttempt(
    state: ErgoWalletState,
    version: VersionTag,
    intent: WalletRollbackIntent,
    rollbackResult: Try[Unit]): Unit =
    verifyWalletRollbackTarget(state, intent) match {
      case Success(_) =>
        rollbackResult.failed.foreach { t =>
          log.warn(
            s"Registry rollback to $version reported failure but reached the exact durable target", t)
        }
        enterRollbackPendingFreshMempool(state, intent)
      case Failure(verificationFailure) =>
        val cause = rollbackResult match {
          case Failure(rollbackFailure) =>
            new IllegalStateException(
              s"${rollbackFailure.getMessage}; post-rollback verification also failed: " +
                verificationFailure.getMessage,
              rollbackFailure)
          case Success(_) => verificationFailure
        }
        enterIndeterminateRollbackQuarantine(state, version, cause)
    }

  private def beginWalletRollback(
    state: ErgoWalletState,
    version: VersionTag,
    mutateRegistry: Boolean = true,
    continuation: Option[ErgoWalletState => Unit] = None,
    waitForEvidence: Boolean = false): Unit = {
    pendingWalletRollbackPreparation = None
    walletRollbackPreparationReason = None
    rollbackRecoveryContinuation = None
    rollbackRecoveryResumeSnapshotLifecycle = false
    deferredRollbackReconciliationBlocks =
      if (waitForEvidence) Map.empty else deferredSnapshotBlock.toMap
    deferredSnapshotBlock = None
    pendingWalletCatchUpTarget = None
    walletRollbackIntent(version) match {
      case Failure(t) =>
        enterRollbackPreparationQuarantine(state, version, t)
      case Success(intent) =>
        attemptWalletRollbackPreparation(
          state,
          PendingWalletRollbackPreparation(
            intent, version, mutateRegistry, continuation),
          waitForEvidence)
    }
  }

  private def attemptWalletRollbackPreparation(
    state: ErgoWalletState,
    request: PendingWalletRollbackPreparation,
    waitForEvidence: Boolean): Unit = {
    val prepared = for {
      _ <- validateCanonicalWalletRollbackTarget(request.intent)
      retained <- walletRegistryVersionExists(state, request.intent.targetVersionId)
      _ <- if (retained) Success(()) else fail(
        s"Wallet rollback target ${request.intent.targetVersionId} is no longer retained")
      plan <- preflightWalletRollbackCatchUp(state, request.intent)
      _ <- validateWalletRollbackCatchUpPlan(state, request.intent, plan)
      _ <- writeWalletRollbackIntent(state, request.intent)
    } yield plan

    prepared match {
      case Failure(t) if waitForEvidence &&
        isRecoverableWalletRollbackPreparationFailure(t) =>
        enterWalletRollbackPreparationPending(state, request, t)
      case Failure(t) =>
        enterRollbackPreparationQuarantine(state, request.version, t)
      case Success(plan) =>
        pendingWalletRollbackPreparation = None
        walletRollbackPreparationReason = None
        deferredRollbackReconciliationBlocks = deferredRollbackReconciliationBlocks.filter {
          case (height, _) =>
            height > request.intent.expectedHeight && height <= plan.stateTipHeight
        }
        rollbackRecoveryContinuation = request.continuation
        activeRollbackIntent = Some(request.intent)
        validateWalletRollbackCatchUpPlan(state, request.intent, plan) match {
          case Failure(t) =>
            enterRollbackPreparationQuarantine(state, request.version, t)
          case Success(_) =>
            val rollbackResult =
              if (request.mutateRegistry) rollbackWalletRegistry(state, request.version)
              else Success(())
            completeWalletRollbackAttempt(
              state, request.version, request.intent, rollbackResult)
            if (rollbackReconciliationQuarantine.nonEmpty && !rollbackFailureRequiresRestart) {
              enqueueCurrentWalletViewApplication(
                request.freshMempoolReader.toSeq.map(ApplyCurrentWalletMempool))
            }
        }
    }
  }

  private def initializeWalletRollbackRecovery(state: ErgoWalletState): Unit =
    readWalletRollbackIntent(state) match {
      case Failure(t) =>
        enterIndeterminateRollbackQuarantine(
          state, "an unreadable durable rollback intent", t)
      case Success(None) =>
        startupNoIntentAlignmentPending = true
        context.become(loadedWallet(state))
      case Success(Some(intent)) =>
        activeRollbackIntent = Some(intent)
        rollbackRecoveryResumeSnapshotLifecycle = true
        readCanonicalWalletRollbackTargetId(intent) match {
          case Failure(t) =>
            enterIndeterminateRollbackQuarantine(state, intent.targetVersionId, t)
          case Success(bestId) if bestId != intent.targetVersionId =>
            recoverStaleDurableWalletRollback(
              state, intent, nonCanonicalWalletRollbackTargetFailure(intent, bestId))
          case Success(_) =>
            verifyWalletRollbackTarget(state, intent) match {
              case Success(_) =>
                preflightWalletRollbackCatchUp(state, intent) match {
                  case Failure(t) =>
                    enterRollbackPreparationQuarantine(
                      state, idToVersion(intent.targetVersionId), t)
                  case Success(_) =>
                    enterRollbackPendingFreshMempool(state, intent)
                }
              case Failure(initialVerificationFailure) =>
                walletRegistryVersionExists(state, intent.targetVersionId) match {
                  case Success(true) =>
                    preflightWalletRollbackCatchUp(state, intent) match {
                      case Failure(t) =>
                        enterRollbackPreparationQuarantine(
                          state, idToVersion(intent.targetVersionId), t)
                      case Success(plan) =>
                        val version = idToVersion(intent.targetVersionId)
                        validateWalletRollbackCatchUpPlan(state, intent, plan) match {
                          case Failure(t) =>
                            enterRollbackPreparationQuarantine(state, version, t)
                          case Success(_) =>
                            completeWalletRollbackAttempt(
                              state, version, intent, rollbackWalletRegistry(state, version))
                        }
                    }
                  case Success(false) =>
                    enterIndeterminateRollbackQuarantine(
                      state,
                      intent.targetVersionId,
                      new IllegalStateException(
                        s"${initialVerificationFailure.getMessage}; target version is no longer retained"))
                  case Failure(t) =>
                    enterIndeterminateRollbackQuarantine(state, intent.targetVersionId, t)
                }
            }
        }
    }

  private def finishWalletRollbackRecovery(
    state: ErgoWalletState,
    intent: WalletRollbackIntent): Unit = {
    val recoveredState = ergoWalletService.updateUtxoState(state.copy(error = None))
    val pendingBlocks = deferredRollbackReconciliationBlocks
      .filter { case (height, _) => height > recoveredState.getWalletHeight }
    val highestPendingBlock = pendingBlocks.toSeq.sortBy(_._1).lastOption
    val continuation = rollbackRecoveryContinuation
    val resumeSnapshotLifecycle = rollbackRecoveryResumeSnapshotLifecycle
    rollbackReconciliationQuarantine = None
    rollbackFailureRequiresRestart = false
    activeRollbackIntent = None
    rollbackRecoveryContinuation = None
    rollbackRecoveryResumeSnapshotLifecycle = false
    deferredRollbackReconciliationBlocks = pendingBlocks
    highestPendingBlock.foreach { block =>
      deferredSnapshotBlock = ErgoWalletActor.latestDeferredSnapshotValue(
        deferredSnapshotBlock, block)
    }
    continuation match {
      case Some(continue) => continue(recoveredState)
      case None =>
        if (resumeSnapshotLifecycle) resumeOrStartUtxoSnapshotScan(recoveredState)
        if (utxoSnapshotQuarantine.isEmpty) {
          scheduleWalletCatchUpIfNeeded(recoveredState, highestPendingBlock.map(_._1))
          if (!hasPendingWalletRecovery(recoveredState) && rollbackReconciliationQuarantine.isEmpty) {
            context.become(loadedWallet(recoveredState))
          }
        }
    }
    maybeReplayStartupSnapshotEvent()
    log.info(
      s"Completed durable wallet rollback recovery to ${intent.targetVersionId} at height ${intent.expectedHeight}")
  }

  private def scheduleWalletCatchUpIfNeeded(
    state: ErgoWalletState,
    additionalTarget: Option[Height]): Unit = {
    val targetHeight = additionalTarget.fold(state.fullHeight)(Math.max(state.fullHeight, _))
    if (!hasPendingUtxoSnapshotScan(state) &&
      state.secretIsSet(settings.walletSettings.testMnemonic) &&
      state.getWalletHeight < targetHeight) {
      requiredWalletCatchUpHeight(state.getWalletHeight, targetHeight) match {
        case Failure(t) =>
          pendingWalletCatchUpTarget = None
          val reason = walletCatchUpFailureReason(
            s"Wallet state at height ${state.getWalletHeight}", t)
          quarantineRollbackState(
            state, s"Wallet is quarantined: $reason", requiresRestart = true, t, clearFreshMempool = true)
        case Success(Some(catchUpHeight)) =>
          pendingWalletCatchUpTarget = Some(targetHeight)
          context.become(loadedWallet(state))
          self ! ScanInThePast(catchUpHeight, rescan = false)
        case Success(None) =>
      }
    }
  }

  private def recoverRollbackReconciliationQuarantine(state: ErgoWalletState): Unit = {
    val verifiedIntent = readWalletRollbackIntent(state).flatMap {
      case Some(intent) if activeRollbackIntent.contains(intent) =>
        validateCanonicalWalletRollbackTarget(intent)
          .flatMap(_ => verifyWalletRollbackTarget(state, intent))
          .map(_ => intent)
      case Some(intent) =>
        fail(s"Durable rollback intent changed unexpectedly to $intent")
      case None =>
        fail("Durable rollback intent disappeared before recovery completed")
    }

    verifiedIntent match {
      case Failure(t) =>
        enterIndeterminateRollbackQuarantine(
          state, activeRollbackIntent.map(_.targetVersionId).getOrElse("unknown target"), t)
      case Success(intent) =>
        reconcileAfterWalletRollback(state) match {
          case Failure(t) =>
            enterRollbackReconciliationQuarantine(state, t)
          case Success(reconciledState) =>
            verifyWalletRollbackTarget(reconciledState, intent) match {
              case Failure(t) =>
                enterIndeterminateRollbackQuarantine(reconciledState, intent.targetVersionId, t)
              case Success(_) =>
                preflightWalletRollbackCatchUp(reconciledState, intent) match {
                  case Failure(t) =>
                    enterRollbackPreparationQuarantine(
                      reconciledState, idToVersion(intent.targetVersionId), t)
                  case Success(_) =>
                    clearWalletRollbackIntent(reconciledState, intent) match {
                      case Success(true) =>
                        finishWalletRollbackRecovery(reconciledState, intent)
                      case Success(false) =>
                        enterIndeterminateRollbackQuarantine(
                          reconciledState,
                          intent.targetVersionId,
                          new IllegalStateException(
                            "Durable rollback intent changed before it could be cleared"))
                      case Failure(t) =>
                        enterRollbackReconciliationQuarantine(reconciledState, t)
                    }
                }
            }
        }
    }
  }

  private def currentWalletViewHandshake(state: ErgoWalletState): Receive = {
    case RetryCurrentWalletViewRequest(requestId) =>
      requestCurrentWalletView(requestId)

    case response@CurrentWalletView(requestId, stateReader, mempoolReader, appliedSnapshot)
      if pendingCurrentWalletViewRequest.contains(requestId) =>
      val structurallyValid = stateReader != null && mempoolReader != null &&
        appliedSnapshot.forall(snapshot => snapshot.stateReader eq stateReader)
      if (structurallyValid) {
        pendingCurrentWalletViewRequest = None
        timers.cancel(CurrentWalletViewRetryTimerKey)
        val capturedSteps = Vector[CurrentWalletViewApplicationStep](
          ApplyCurrentWalletState(stateReader),
          ApplyCurrentWalletMempool(mempoolReader)) ++
          appliedSnapshot.toVector.map(event =>
            ApplyCurrentWalletSnapshot(event, ActorRef.noSender))
        // Keep the holder's captured state/mempool/snapshot tuple contiguous. Messages already
        // queued behind this response are stashed until every captured component has been applied.
        enqueueCurrentWalletViewApplication(capturedSteps)
      } else {
        log.error(
          s"Ignoring malformed current wallet view response for request $requestId: $response")
      }

    case CurrentWalletView(requestId, _, _, _) =>
      log.debug(s"Ignoring stale current wallet view response for request $requestId")
  }

  private def clearOperationalMempoolError(state: ErgoWalletState): ErgoWalletState =
    operationalMempoolReconciliationQuarantine match {
      case Some(reason) if state.error.contains(reason) => state.copy(error = None)
      case _ => state
    }

  private def reconcileOperationalMempool(
    state: ErgoWalletState,
    mempoolReader: ErgoMemPoolReader): Unit = {
    val candidate = clearOperationalMempoolError(state)
      .copy(mempoolReaderOpt = Some(mempoolReader))
    Try {
      val reconciled = ergoWalletService.reconcileOffChainRegistry(
        candidate, settings.walletSettings.dustLimit)
      ergoWalletService.updateUtxoState(reconciled)
    } match {
      case Success(reconciled) =>
        operationalMempoolReconciliationQuarantine = None
        context.become(loadedWallet(reconciled))
      case Failure(t) =>
        val reason =
          s"Wallet is quarantined because operational mempool reconciliation failed: ${t.getMessage}"
        operationalMempoolReconciliationQuarantine = Some(reason)
        log.error(reason, t)
        context.become(loadedWallet(state.copy(
          offChainRegistry = OffChainRegistry.empty,
          outputsFilter = None,
          mempoolReaderOpt = Some(mempoolReader),
          utxoStateReaderOpt = None,
          error = Some(reason))))
    }
  }

  private def operationalMempoolReconciliationQuarantinedWallet(
    state: ErgoWalletState): Receive = ({
    case ChangedMempool(mempoolReader: ErgoMemPoolReader@unchecked) =>
      reconcileOperationalMempool(state, mempoolReader)

    case message: ChangedState =>
      operationalWallet(state)(message)

    case message: ScanOnChain =>
      operationalWallet(state)(message)

    case message: ScanInThePast =>
      operationalWallet(state)(message)

    case message: UtxoSnapshotAppliedToState =>
      operationalWallet(state)(message)

    case message: Rollback =>
      val cleared = clearOperationalMempoolError(state)
      operationalMempoolReconciliationQuarantine = None
      operationalWallet(cleared)(message)

    case ScanOffChain(_) =>
      log.debug("Ignoring incremental off-chain scan until mempool reconciliation recovers")
  }: Receive).orElse(quarantinedWallet(state))

  private def walletMode(state: ErgoWalletState): Receive = {
    if (startupNoIntentAlignmentPending) startupAlignmentPendingWallet(state)
    else if (utxoSnapshotQuarantine.nonEmpty) quarantinedWallet(state)
    else if (pendingWalletRollbackPreparation.nonEmpty) walletRollbackPreparationPendingWallet(state)
    else if (rollbackReconciliationQuarantine.nonEmpty) rollbackReconciliationQuarantinedWallet(state)
    else if (hasPendingWalletRecovery(state)) pendingUtxoSnapshotWallet(state)
    else if (operationalMempoolReconciliationQuarantine.nonEmpty) {
      operationalMempoolReconciliationQuarantinedWallet(state)
    } else operationalWallet(state)
  }

  private def currentWalletViewApplication(state: ErgoWalletState): Receive = {
    case ContinueCurrentWalletViewApplication if currentWalletViewApplicationInProgress =>
      currentWalletViewApplicationSteps.headOption match {
        case None =>
          currentWalletViewApplicationInProgress = false
          unstashAll()
        case Some(step) =>
          currentWalletViewApplicationSteps = currentWalletViewApplicationSteps.tail
          step match {
            case ApplyCurrentWalletState(stateReader) =>
              self ! ContinueCurrentWalletViewApplication
              walletMode(state)(ChangedState(stateReader))
            case ApplyCurrentWalletMempool(mempoolReader) =>
              self ! ContinueCurrentWalletViewApplication
              walletMode(state)(ChangedMempool(mempoolReader))
            case ApplyCurrentWalletSnapshot(event, replyTo) =>
              self.tell(ExecuteCurrentWalletSnapshot(event), replyTo)
          }
      }

    case ExecuteCurrentWalletSnapshot(event) if currentWalletViewApplicationInProgress =>
      self ! ContinueCurrentWalletViewApplication
      walletMode(state)(event)

    case ContinueCurrentWalletViewApplication | ExecuteCurrentWalletSnapshot(_) =>
      log.debug("Ignoring a stale current wallet view application step")

    case _ if currentWalletViewApplicationInProgress =>
      stash()
  }

  private def loadedWallet(state: ErgoWalletState): Receive =
    currentWalletViewApplication(state)
      .orElse(currentWalletViewHandshake(state))
      .orElse(walletMode(state))

  private def startupAlignmentPendingWallet(state: ErgoWalletState): Receive = ({
    case GetWalletStatus =>
      sender() ! WalletStatus(
        state.secretIsSet(settings.walletSettings.testMnemonic),
        state.walletVars.proverOpt.isDefined,
        state.getChangeAddress(ergoAddressEncoder),
        state.getWalletHeight,
        Some(startupAlignmentPendingReason))

    case message: ChangedState if pendingCurrentWalletViewRequest.isEmpty =>
      operationalWallet(state)(message)

    case _: ChangedState =>
      log.debug("Ignoring uncorrelated state update until the startup view handshake completes")

    case ChangedMempool(reader: ErgoMemPoolReader@unchecked)
      if pendingCurrentWalletViewRequest.isEmpty =>
      startupDeferredMempool = Some(reader -> sender())

    case _: ChangedMempool =>
      log.debug("Ignoring uncorrelated mempool update until the startup view handshake completes")

    case event: UtxoSnapshotAppliedToState if pendingCurrentWalletViewRequest.isEmpty =>
      deferStartupUtxoSnapshotEvent(event, sender())

    case _: UtxoSnapshotAppliedToState =>
      sender() ! fail(startupAlignmentPendingReason)

    case ScanOnChain(newBlock) =>
      deferredRollbackReconciliationBlocks =
        deferredRollbackReconciliationBlocks.updated(newBlock.height, newBlock)
      deferredSnapshotBlock = ErgoWalletActor.latestDeferredSnapshotValue(
        deferredSnapshotBlock, newBlock.height -> newBlock)
      log.debug(
        s"Retaining wallet block ${newBlock.id} until startup canonical alignment completes")

    case Rollback(version: VersionTag) =>
      log.debug(
        s"Absorbing wallet rollback to $version until the startup canonical state is received")

    case _: GetOrInitUtxoSnapshotScanStatus | _: ApplyUtxoSnapshotScanBatch =>
      sender() ! fail(startupAlignmentPendingReason)

    case _: UtxoSnapshotScanTerminated | _: FinalizeUtxoSnapshotScan |
         _: UtxoSnapshotCatchUpFailed | _: UtxoSnapshotCleanupFailed |
         _: RetryUtxoSnapshotSourceCleanup | _: ContinueUtxoSnapshotCatchUp |
         _: ScanInThePast =>
      log.debug("Ignoring stale wallet lifecycle message during startup canonical alignment")
  }: Receive).orElse(quarantinedWallet(
    state.copy(error = state.error.orElse(Some(startupAlignmentPendingReason)))))

  private def retryWalletRollbackPreparation(state: ErgoWalletState): Unit =
    pendingWalletRollbackPreparation match {
      case Some(request) =>
        attemptWalletRollbackPreparation(state, request, waitForEvidence = true)
      case None =>
        context.become(loadedWallet(state))
    }

  private def walletRollbackPreparationPendingWallet(state: ErgoWalletState): Receive = ({
    case ScanOnChain(newBlock) =>
      pendingWalletRollbackPreparation.foreach { request =>
        if (newBlock.height > request.intent.expectedHeight) {
          deferredRollbackReconciliationBlocks =
            deferredRollbackReconciliationBlocks.updated(newBlock.height, newBlock)
        }
      }
      retryWalletRollbackPreparation(state)

    case ChangedState(s: ErgoStateReader@unchecked) =>
      pendingWalletRollbackPreparation match {
        case Some(request) =>
          state.storage.updateStateContext(s.stateContext) match {
            case Success(_) =>
              val cp = s.stateContext.currentParameters
              val newWalletVars = state.walletVars.withParameters(cp).getOrElse(state.walletVars)
              val updatedState = ergoWalletService.updateUtxoState(
                state.copy(stateReaderOpt = Some(s), parameters = cp, walletVars = newWalletVars))
              attemptWalletRollbackPreparation(
                updatedState, request, waitForEvidence = false)
            case Failure(t) =>
              enterRollbackPreparationQuarantine(state, request.version, t)
          }
        case None =>
          context.become(loadedWallet(state))
      }

    case ChangedMempool(mr: ErgoMemPoolReader@unchecked) =>
      pendingWalletRollbackPreparation match {
        case Some(request) =>
          val updatedRequest = request.copy(freshMempoolReader = Some(mr))
          pendingWalletRollbackPreparation = Some(updatedRequest)
          attemptWalletRollbackPreparation(
            state.copy(mempoolReaderOpt = Some(mr)),
            updatedRequest,
            waitForEvidence = true)
        case None =>
          context.become(loadedWallet(state))
      }

    case Rollback(version: VersionTag) =>
      pendingWalletRollbackPreparation match {
        case Some(request) if request.version == version =>
          log.debug(s"Ignoring duplicate wallet rollback preparation for version $version")
        case Some(request) =>
          log.info(
            s"Replacing pending wallet rollback preparation for ${request.version} with $version")
          pendingWalletRollbackPreparation = None
          walletRollbackPreparationReason = None
          deferredRollbackReconciliationBlocks = Map.empty
          operationalWallet(state)(Rollback(version))
        case None =>
          context.become(loadedWallet(state))
      }
  }: Receive).orElse(quarantinedWallet(state))

  private def pendingUtxoSnapshotWallet(state: ErgoWalletState): Receive = ({
    case GetWalletStatus => operationalWallet(state)(GetWalletStatus)
    case message: ChangedState => operationalWallet(state)(message)
    case message: ChangedMempool => operationalWallet(state)(message)
    case message: Rollback => operationalWallet(state)(message)
    case message: UtxoSnapshotAppliedToState => operationalWallet(state)(message)
    case message: GetOrInitUtxoSnapshotScanStatus => operationalWallet(state)(message)
    case message: ApplyUtxoSnapshotScanBatch => operationalWallet(state)(message)
    case message: UtxoSnapshotScanTerminated => operationalWallet(state)(message)
    case message: FinalizeUtxoSnapshotScan => operationalWallet(state)(message)
    case message: UtxoSnapshotCatchUpFailed => operationalWallet(state)(message)
    case message: UtxoSnapshotCleanupFailed => operationalWallet(state)(message)
    case message: RetryUtxoSnapshotSourceCleanup => operationalWallet(state)(message)
    case message: ContinueUtxoSnapshotCatchUp => operationalWallet(state)(message)
    case message: ScanInThePast
      if pendingWalletCatchUpTarget.nonEmpty && !hasPendingUtxoSnapshotScan(state) =>
      operationalWallet(state)(message)
    case ScanOnChain(newBlock) =>
      deferredSnapshotBlock = ErgoWalletActor.latestDeferredSnapshotValue(
        deferredSnapshotBlock,
        newBlock.height -> newBlock)
      if (pendingWalletCatchUpTarget.nonEmpty) {
        pendingWalletCatchUpTarget = Some(
          Math.max(pendingWalletCatchUpTarget.get, newBlock.height))
      }
      log.debug(
        s"Deferring wallet scan of block ${newBlock.id} until mandatory wallet recovery completes")
  }: Receive).orElse(quarantinedWallet(state.copy(error = state.error.orElse(Some(
    "Wallet operations are unavailable while UTXO snapshot progress is unresolved")))))

  private def rollbackReconciliationQuarantinedWallet(state: ErgoWalletState): Receive = ({
    case ChangedMempool(mr: ErgoMemPoolReader@unchecked) =>
      val updatedState = state.copy(mempoolReaderOpt = Some(mr))
      if (rollbackFailureRequiresRestart) {
        context.become(loadedWallet(updatedState))
      } else {
        recoverRollbackReconciliationQuarantine(updatedState)
      }

    case ScanOnChain(newBlock) =>
      deferredRollbackReconciliationBlocks =
        deferredRollbackReconciliationBlocks.updated(newBlock.height, newBlock)
      log.debug(
        s"Deferring wallet scan of block ${newBlock.id} until rollback reconciliation recovers")

    case ScanOffChain(_) =>
      log.debug("Ignoring incremental off-chain scan until rollback reconciliation recovers")

    case _: ScanInThePast =>
      log.debug("Ignoring stale wallet catch-up until rollback reconciliation recovers")

    case Rollback(version: VersionTag) =>
      if (rollbackFailureRequiresRestart) {
        log.error(
          s"Ignoring rollback to version $version because wallet registry state is indeterminate")
      } else {
        beginWalletRollback(state, version)
      }
  }: Receive).orElse(quarantinedWallet(state))

  private def quarantinedWallet(state: ErgoWalletState): Receive = {
    case GetWalletStatus =>
      sender() ! WalletStatus(
        state.secretIsSet(settings.walletSettings.testMnemonic),
        state.walletVars.proverOpt.isDefined,
        state.getChangeAddress(ergoAddressEncoder),
        state.getWalletHeight,
        Some(activeQuarantineReason(state)))

    case CloseWallet =>
      log.info("Closing quarantined wallet actor")
      state.storage.close()
      state.registry.close()
      context stop self

    case LockWallet =>
      log.info("Locking quarantined wallet")
      context.become(loadedWallet(ergoWalletService.lockWallet(state)))

    case ChangedState(s: ErgoStateReader@unchecked) =>
      state.storage.updateStateContext(s.stateContext) match {
        case Success(_) =>
          val cp = s.stateContext.currentParameters
          val newWalletVars = state.walletVars.withParameters(cp).getOrElse(state.walletVars)
          val updated = ergoWalletService.updateUtxoState(
            state.copy(stateReaderOpt = Some(s), parameters = cp, walletVars = newWalletVars))
          context.become(loadedWallet(updated))
        case Failure(t) =>
          log.error("Updating quarantined wallet state context failed", t)
          context.become(loadedWallet(state.copy(error = Some(activeQuarantineReason(state)))))
      }

    case ChangedMempool(mr: ErgoMemPoolReader@unchecked) =>
      val updated = ergoWalletService.updateUtxoState(
        state.copy(mempoolReaderOpt = Some(mr)))
      context.become(loadedWallet(updated))

    case UtxoSnapshotAppliedToState(height, blockId, stateReader)
      if utxoSnapshotQuarantine.nonEmpty =>
      recoverQuarantinedUtxoSnapshot(state, height, blockId, stateReader, sender())

    case UtxoSnapshotAppliedToState(_, _, _) =>
      sender() ! fail(activeQuarantineReason(state))

    case GetOrInitUtxoSnapshotScanStatus(_, _, _) | ApplyUtxoSnapshotScanBatch(_, _, _, _, _) =>
      sender() ! fail(activeQuarantineReason(state))

    case _: UtxoSnapshotScanTerminated | _: FinalizeUtxoSnapshotScan |
         _: UtxoSnapshotCatchUpFailed | _: UtxoSnapshotCleanupFailed |
         _: RetryUtxoSnapshotSourceCleanup |
         _: ContinueUtxoSnapshotCatchUp =>
      log.debug("Ignoring stale UTXO snapshot lifecycle message while the wallet is quarantined")

    case InitWallet(walletPass, mnemonicPassOpt) =>
      walletPass.erase()
      mnemonicPassOpt.foreach(_.erase())
      sender() ! fail(activeQuarantineReason(state))

    case RestoreWallet(mnemonic, mnemonicPassOpt, walletPass, _) =>
      mnemonic.erase()
      mnemonicPassOpt.foreach(_.erase())
      walletPass.erase()
      sender() ! fail(activeQuarantineReason(state))

    case UnlockWallet(walletPass) =>
      walletPass.erase()
      sender() ! fail(activeQuarantineReason(state))

    case CheckSeed(mnemonic, passOpt) =>
      mnemonic.erase()
      passOpt.foreach(_.erase())
      sender() ! fail(activeQuarantineReason(state))

    case GetPrivateKeyFromPath(_) | GenerateTransaction(_, _, _, _) |
         SignTransaction(_, _, _, _, _) | RescanWallet(_) | DeriveKey(_) =>
      sender() ! fail(activeQuarantineReason(state))

    case GenerateCommitmentsFor(_, _, _, _) =>
      sender() ! GenerateCommitmentsResponse(fail(activeQuarantineReason(state)))

    case DeriveNextKey =>
      sender() ! DeriveNextKeyResult(fail(activeQuarantineReason(state)))

    case GetFirstSecret =>
      sender() ! FirstSecretResponse(fail(activeQuarantineReason(state)))

    case CollectWalletBoxes(_, _) =>
      sender() ! ReqBoxesResponse(fail(activeQuarantineReason(state)))

    case AddScan(_) =>
      sender() ! AddScanResponse(fail(activeQuarantineReason(state)))

    case RemoveScan(_) =>
      sender() ! RemoveScanResponse(fail(activeQuarantineReason(state)))

    case AddBox(_, _) =>
      sender() ! AddBoxResponse(fail(activeQuarantineReason(state)))

    case StopTracking(_, _) =>
      sender() ! StopTrackingResponse(fail(activeQuarantineReason(state)))

    case _ if sender() == self =>
      log.debug("Ignoring stale internal wallet message while the wallet is quarantined")

    case _ =>
      sender() ! akka.actor.Status.Failure(new IllegalStateException(activeQuarantineReason(state)))
  }

  private def operationalWallet(state: ErgoWalletState): Receive = {
    case InitWallet(walletPass, mnemonicPassOpt) if hasPendingUtxoSnapshotScan(state) =>
      walletPass.erase()
      mnemonicPassOpt.foreach(_.erase())
      sender() ! fail("Wallet initialization is unavailable while UTXO snapshot progress is unresolved")

    case RestoreWallet(mnemonic, mnemonicPassOpt, walletPass, _) if hasPendingUtxoSnapshotScan(state) =>
      mnemonic.erase()
      mnemonicPassOpt.foreach(_.erase())
      walletPass.erase()
      sender() ! fail("Wallet restoration is unavailable while UTXO snapshot progress is unresolved")

    // Init wallet (w. mnemonic generation) if secret is not set yet
    case InitWallet(walletPass, mnemonicPassOpt) if !state.secretIsSet(settings.walletSettings.testMnemonic) =>
      ergoWalletService.initWallet(state, settings, walletPass, mnemonicPassOpt) match {
        case Success((mnemonic, newState)) =>
          log.info("Wallet is initialized")
          context.become(loadedWallet(newState))
          self ! UnlockWallet(walletPass)
          sender() ! Success(mnemonic)
        case Failure(t) =>
          walletPass.erase()
          val f = wrapLegalExc(t) // getting nicer message for illegal key size exception
          log.error(s"Wallet initialization is failed, details: ${f.exception.getMessage}")
          sender() ! f
      }

    // Restore wallet with mnemonic if secret is not set yet
    case RestoreWallet(mnemonic, mnemonicPassOpt, walletPass, usePre1627KeyDerivation) if !state.secretIsSet(settings.walletSettings.testMnemonic) =>
      ergoWalletService.restoreWallet(state, settings, mnemonic, mnemonicPassOpt, walletPass, usePre1627KeyDerivation) match {
        case Success(newState) =>
          log.info("Wallet is restored")
          context.become(loadedWallet(newState))
          self ! UnlockWallet(walletPass)
          sender() ! Success(())
        case Failure(t) =>
          walletPass.erase()
          val f = wrapLegalExc(t) //getting nicer message for illegal key size exception
          log.error(s"Wallet restoration is failed, details: ${f.exception.getMessage}")
          sender() ! f
      }

    // branch for key already being set
    case _: RestoreWallet | _: InitWallet =>
      sender() ! Failure(new Exception("Wallet is already initialized or testMnemonic is set. Clear current secret to re-init it."))

    /* READERS */
    case ReadBalances(chainStatus) =>
      val walletDigest = if (chainStatus.onChain) {
        state.registry.fetchDigest()
      } else {
        state.offChainRegistry.digest
      }
      val res = if (settings.walletSettings.checkEIP27) {
        // If re-emission token in the wallet, subtract it from ERG balance
        val reemissionAmt = walletDigest.walletAssetBalances
          .find(_._1 == settings.chainSettings.reemission.reemissionTokenId)
          .map(_._2)
          .getOrElse(0L)
        if (reemissionAmt == 0) {
          walletDigest
        } else {
          walletDigest.copy(walletBalance = walletDigest.walletBalance - reemissionAmt)
        }
      } else {
        walletDigest
      }
      sender() ! res

    case ReadPublicKeys(from, until) =>
      sender() ! state.walletVars.publicKeyAddresses.slice(from, until)

    case ReadExtendedPublicKeys() =>
      sender() ! state.storage.readAllKeys()

    case GetPrivateKeyFromPath(path: DerivationPath) =>
      sender() ! ergoWalletService.getPrivateKeyFromPath(state, path)

    case GetMiningPubKey =>
      state.walletVars.trackedPubKeys.headOption match {
        case Some(pk) =>
          log.info(s"Loading pubkey for miner from cache")
          sender() ! MiningPubKeyResponse(Some(pk.key))
        case None =>
          val pubKeyOpt = state.storage.readAllKeys().headOption.map(_.key)
          pubKeyOpt.foreach(_ => log.info(s"Loading pubkey for miner from storage"))
          sender() ! MiningPubKeyResponse(state.storage.readAllKeys().headOption.map(_.key))
      }

    // read first wallet secret (used in miner only)
    case GetFirstSecret =>
      if (state.walletVars.proverOpt.nonEmpty) {
        state.walletVars.proverOpt.foreach(_.hdKeys.headOption.foreach { secret =>
          sender() ! FirstSecretResponse(Success(secret.privateInput))
        })
      } else {
        sender() ! FirstSecretResponse(Failure(new Exception("Wallet is locked")))
      }

    /*
     * Read wallet boxes, unspent only (if corresponding flag is set), or all (both spent and unspent).
     * If considerUnconfirmed flag is set, mempool contents is considered as well.
     */
    case GetWalletBoxes(unspent, considerUnconfirmed) =>
      val boxes = ergoWalletService.getWalletBoxes(state, unspent, considerUnconfirmed)
      sender() ! boxes

    case GetScanUnspentBoxes(scanId, considerUnconfirmed, minHeight, maxHeight) =>
      val boxes = ergoWalletService.getScanUnspentBoxes(state, scanId, considerUnconfirmed, minHeight, maxHeight)
      sender() ! boxes

    case GetScanSpentBoxes(scanId) =>
      val boxes = ergoWalletService.getScanSpentBoxes(state, scanId)
      sender() ! boxes

    case GetTransactions =>
      sender() ! ergoWalletService.getTransactions(state.registry, state.fullHeight)

    case GetTransaction(txId) =>
      sender() ! ergoWalletService.getTransactionsByTxId(txId, state.registry, state.fullHeight)

    case ReadScans =>
      sender() ! ReadScansResponse(state.walletVars.externalScans)

    /* STATE CHANGE */
    case ChangedMempool(mr: ErgoMemPoolReader@unchecked)
      if state.mempoolReaderOpt.exists(_ eq mr) =>
      log.debug("Ignoring an unchanged immutable mempool reader")

    case ChangedMempool(mr: ErgoMemPoolReader@unchecked) =>
      reconcileOperationalMempool(state, mr)

    case ChangedState(s: ErgoStateReader@unchecked) =>
      state.storage.updateStateContext(s.stateContext) match {
        case Success(_) =>
          val cp = s.stateContext.currentParameters

          val newWalletVars = state.walletVars.withParameters(cp) match {
            case Success(res) => res
            case Failure(t) =>
              log.warn("Can not update wallet vars: ", t)
              state.walletVars
          }
          val updState = state.copy(stateReaderOpt = Some(s), parameters = cp, walletVars = newWalletVars)
          val newState = ergoWalletService.updateUtxoState(updState)
          if (startupNoIntentAlignmentPending) {
            alignWalletAtStartup(newState)
          } else {
            resumeOrStartUtxoSnapshotScan(newState)
            if (utxoSnapshotQuarantine.isEmpty) context.become(loadedWallet(newState))
          }
        case Failure(t) =>
          if (startupNoIntentAlignmentPending) {
            enterWalletStartupAlignmentQuarantine(state, t)
          } else {
            val errorMsg = s"Updating wallet state context failed : ${t.getMessage}"
            log.error(errorMsg, t)
            context.become(loadedWallet(state.copy(error = Some(errorMsg))))
          }
      }

    case UtxoSnapshotAppliedToState(height, blockId, _) =>
      val replyTo = sender()
      readUtxoSnapshotWalletOrigin(state) match {
        case Failure(t) =>
          enterUtxoSnapshotQuarantine(
            state,
            s"Unreadable durable UTXO snapshot wallet origin: ${t.getMessage}",
            fence = None,
            persistFence = false)
          replyTo ! Failure(t)
        case Success(Some(origin))
          if origin.snapshotHeight != height || origin.snapshotBlockId != blockId =>
          val t = new IllegalStateException(
            s"Applied UTXO snapshot at height $height with id $blockId conflicts with " +
              s"completed wallet origin at height ${origin.snapshotHeight} with id ${origin.snapshotBlockId}")
          enterUtxoSnapshotQuarantine(
            state,
            t.getMessage,
            fence = Some(UtxoSnapshotScanInvalidation(
              origin.snapshotHeight, origin.snapshotBlockId)),
            persistFence = true)
          replyTo ! Failure(t)
        case Success(originOpt) if canScanUtxoSnapshot(state) =>
          state.storage.readUtxoSnapshotScanStatusTry() match {
            case Failure(t) =>
              enterUtxoSnapshotQuarantine(
                state,
                s"Unreadable durable UTXO snapshot scan status: ${t.getMessage}",
                fence = None,
                persistFence = false)
              replyTo ! Failure(t)
            case Success(Some(status))
              if originOpt.exists(origin => !statusMatchesUtxoSnapshotOrigin(status, origin)) =>
              val t = new IllegalStateException(
                s"Durable UTXO snapshot scan status conflicts with completed wallet origin: " +
                  s"status=$status, origin=${originOpt.get}")
              enterUtxoSnapshotQuarantine(
                state,
                t.getMessage,
                fence = Some(UtxoSnapshotScanInvalidation(
                  status.snapshotHeight, status.snapshotBlockId)),
                persistFence = true)
              replyTo ! Failure(t)
            case Success(Some(status)) =>
              validateLiveUtxoSnapshotScanDefinition(state, status) match {
                case Failure(t) =>
                  enterUtxoSnapshotQuarantine(
                    state,
                    t.getMessage,
                    fence = Some(UtxoSnapshotScanInvalidation(
                      status.snapshotHeight, status.snapshotBlockId)),
                    persistFence = true)
                  replyTo ! Failure(t)
                case Success(_) if originOpt.nonEmpty =>
                  log.debug("Skipping UTXO snapshot wallet scan because completed origin is present")
                  replyTo ! Success(None)
                case Success(_) if UtxoSnapshotScanStartPolicy.shouldStartApplied(
                  height, blockId, registryPristine = false,
                  state.rescanInProgress, Some(status)) =>
                  replyTo ! Success(startUtxoSnapshotScan(
                    state, height, blockId, forceRestart = false))
                case Success(_) =>
                  log.debug(s"Skipping ineligible UTXO snapshot wallet scan at height $height")
                  replyTo ! Success(None)
              }
            case Success(None) if originOpt.nonEmpty =>
              log.debug("Skipping UTXO snapshot wallet scan because completed origin is present")
              replyTo ! Success(None)
            case Success(None) if activeUtxoSnapshotRun.exists(
              _.hasSnapshot(height, blockId)) =>
              replyTo ! Success(activeUtxoSnapshotRun)
            case Success(None) =>
              Try(calculateUtxoSnapshotScanDefinition(state)).flatten match {
                case Failure(t) =>
                  enterUtxoSnapshotQuarantine(
                    state,
                    s"Unable to calculate the live UTXO snapshot scan definition: ${t.getMessage}",
                    fence = None,
                    persistFence = false)
                  replyTo ! Failure(t)
                case Success(_) =>
                  state.registry.isPristineForUtxoSnapshot match {
                    case Failure(t) =>
                      val wrapped = new IllegalStateException(
                        "Unable to verify that the wallet registry is pristine for a UTXO snapshot",
                        t)
                      enterUtxoSnapshotQuarantine(
                        state, wrapped.getMessage, fence = None, persistFence = false)
                      replyTo ! Failure(wrapped)
                    case Success(false) =>
                      val t = new IllegalStateException(
                        "A UTXO snapshot wallet scan requires a pristine wallet registry")
                      enterUtxoSnapshotQuarantine(
                        state, t.getMessage, fence = None, persistFence = false)
                      replyTo ! Failure(t)
                    case Success(true) if UtxoSnapshotScanStartPolicy.shouldStartApplied(
                      height, blockId, registryPristine = true,
                      state.rescanInProgress, None) =>
                      replyTo ! Success(startUtxoSnapshotScan(
                        state, height, blockId, forceRestart = false))
                    case Success(true) =>
                      log.debug(s"Skipping ineligible UTXO snapshot wallet scan at height $height")
                      replyTo ! Success(None)
                  }
              }
          }
        case Success(_) =>
          log.debug("Skipping UTXO snapshot wallet scan because wallet scan variables are not initialized")
          replyTo ! Success(None)
      }

    /* SCAN COMMANDS */
    case GetOrInitUtxoSnapshotScanStatus(run, _, _)
      if !isCurrentUtxoSnapshotRun(run) =>
      sender() ! fail("UTXO snapshot wallet scan run is no longer active")

    case GetOrInitUtxoSnapshotScanStatus(run, manifestDepth, totalSubtrees) =>
      val statusTry = calculateUtxoSnapshotScanDefinition(state).flatMap { liveDefinition =>
        state.storage.readUtxoSnapshotScanStatusTry().flatMap {
        case Some(status)
          if status.snapshotHeight == run.snapshotHeight &&
            status.snapshotBlockId == run.snapshotBlockId &&
            status.manifestDepth == manifestDepth &&
            status.totalSubtrees == totalSubtrees &&
            status.scanDefinition == liveDefinition => Success(status)
        case Some(status)
          if status.snapshotHeight == run.snapshotHeight &&
            status.snapshotBlockId == run.snapshotBlockId &&
            status.manifestDepth == manifestDepth &&
            status.totalSubtrees == totalSubtrees =>
          fail(
            s"Stored UTXO snapshot scan definition does not match the live wallet definition: " +
              s"stored=${status.scanDefinition}, live=$liveDefinition")
        case Some(status) =>
          fail(s"A different UTXO snapshot scan is already stored at height ${status.snapshotHeight}")
        case None if totalSubtrees <= 0 || manifestDepth < 0 =>
          fail(s"Invalid UTXO snapshot scan dimensions: depth=$manifestDepth, parts=$totalSubtrees")
        case None =>
          val status = UtxoSnapshotScanStatus(run.snapshotHeight, run.snapshotBlockId, manifestDepth,
            nextSubtreeIndex = 0, totalSubtrees = totalSubtrees, completed = false,
            scanDefinition = liveDefinition)
          state.storage.writeUtxoSnapshotScanStatus(status).map(_ => status)
        }
      }
      if (statusTry.isSuccess && isCurrentUtxoSnapshotRun(run) && startingUtxoSnapshot.contains(run)) {
        startingUtxoSnapshot = None
      }
      sender() ! statusTry

    case ApplyUtxoSnapshotScanBatch(run, _, _, _, _)
      if !isCurrentUtxoSnapshotRun(run) =>
      sender() ! fail("UTXO snapshot wallet scan run is no longer active")

    case ApplyUtxoSnapshotScanBatch(run, subtreeIndex, nextSubtreeIndex, completed, boxes) =>
      val replyTo = sender()
      val updateTry = state.storage.readUtxoSnapshotScanStatusTry().flatMap {
        case Some(status)
          if status.snapshotHeight == run.snapshotHeight && status.snapshotBlockId == run.snapshotBlockId &&
            subtreeIndex >= 0 && subtreeIndex < status.totalSubtrees &&
            nextSubtreeIndex == UtxoSnapshotWalletScanner.nextBatchCursor(
              status.totalSubtrees, subtreeIndex) &&
            (status.nextSubtreeIndex == subtreeIndex ||
              status.nextSubtreeIndex == nextSubtreeIndex) &&
            completed == (nextSubtreeIndex == status.totalSubtrees) =>
          validateLiveUtxoSnapshotScanDefinition(state, status).flatMap { _ =>
            if (status.nextSubtreeIndex == nextSubtreeIndex) {
              val scanResults = WalletScanLogic.scanSnapshotBoxes(
                boxes,
                run.snapshotHeight,
                state.walletVars,
                settings.walletSettings.dustLimit)
              state.registry.validateSnapshotChunk(
                scanResults,
                run.snapshotBlockId,
                run.snapshotHeight,
                subtreeIndex,
                nextSubtreeIndex,
                finalChunk = completed).map(_ => state -> status)
            } else {
              val updatedStatus =
                status.copy(nextSubtreeIndex = nextSubtreeIndex, completed = completed)
              ergoWalletService
                .scanUtxoSnapshotChunk(
                  state,
                  boxes,
                  run.snapshotBlockId,
                  run.snapshotHeight,
                  subtreeIndex,
                  nextSubtreeIndex,
                  finalChunk = completed,
                  settings.walletSettings.dustLimit
                )
                .flatMap { updatedState =>
                  val persistStatus = if (updatedStatus.completed) {
                    updatedState.storage.completeUtxoSnapshotScan(updatedStatus)
                  } else {
                    updatedState.storage.writeUtxoSnapshotScanStatus(updatedStatus)
                  }
                  persistStatus.map { _ =>
                    updatedState -> updatedStatus
                  }
                }
            }
          }
        case Some(status) if status.completed =>
          fail(s"UTXO snapshot wallet scan is already completed at height ${status.snapshotHeight}")
        case Some(status) =>
          fail(s"Unexpected UTXO snapshot wallet scan batch, expected subtree ${status.nextSubtreeIndex}")
        case None =>
          fail("UTXO snapshot wallet scan status is missing")
      }

      updateTry match {
        case Success((updatedState, updatedStatus)) =>
          val finalState = if (updatedStatus.completed) {
            updatedState.copy(rescanInProgress = false)
          } else {
            updatedState
          }
          if (updatedStatus.completed) {
            finalizingUtxoSnapshot = Some(run)
            finalizingUtxoSnapshotStatus = Some(updatedStatus)
          }
          context.become(loadedWallet(finalState))
          replyTo ! Success(updatedStatus)
          if (updatedStatus.completed) {
            self ! FinalizeUtxoSnapshotScan(run, updatedStatus)
          }
        case Failure(t: UtxoSnapshotChunkIntegrityException) =>
          enterUtxoSnapshotQuarantine(
            state,
            s"Durable UTXO snapshot registry frontier failed validation: ${t.getMessage}",
            fence = Some(UtxoSnapshotScanInvalidation(
              run.snapshotHeight, run.snapshotBlockId)),
            persistFence = true)
          replyTo ! Failure(t)
        case Failure(t) =>
          val errorMsg = s"UTXO snapshot wallet scan batch failed : ${t.getMessage}"
          log.error(errorMsg, t)
          context.become(loadedWallet(state.copy(error = Some(errorMsg))))
          replyTo ! Failure(t)
      }

    case UtxoSnapshotScanTerminated(run, message) if isCurrentUtxoSnapshotRun(run) =>
      startingUtxoSnapshot = None
      activeUtxoSnapshotRun = None
      terminalUtxoSnapshot = Some(run.snapshotHeight -> run.snapshotBlockId)
      deferredSnapshotBlock = None
      deferredRollbackReconciliationBlocks = Map.empty
      log.error(message)
      context.become(loadedWallet(state.copy(error = Some(message), rescanInProgress = false)))

    case _: UtxoSnapshotScanTerminated =>
      log.debug("Ignoring stale UTXO snapshot scan termination")

    case FinalizeUtxoSnapshotScan(run, status, cleanupAttempt)
      if isCurrentUtxoSnapshotRun(run) && run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId) =>
      if (cleanupAttempt == 0) {
        finalizeUtxoSnapshotScan(status, state, run)
      } else {
        completeUtxoSnapshotFinalization(state, run, status, cleanupAttempt)
      }

    case _: FinalizeUtxoSnapshotScan =>
      log.debug("Ignoring stale UTXO snapshot finalization")

    case UtxoSnapshotCatchUpFailed(run, status, message)
      if isCurrentUtxoSnapshotRun(run) && run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId) =>
      blockRecoverableUtxoSnapshotCatchUp(
        state, run, "scanner", new IllegalStateException(message))

    case _: UtxoSnapshotCatchUpFailed =>
      log.debug("Ignoring stale UTXO snapshot catch-up failure")

    case UtxoSnapshotCleanupFailed(run, message) if activeUtxoSnapshotRun.contains(run) =>
      log.error(message)
      context.become(loadedWallet(state.copy(error = Some(message))))

    case _: UtxoSnapshotCleanupFailed =>
      log.debug("Ignoring stale UTXO snapshot cleanup failure")

    case RetryUtxoSnapshotSourceCleanup(expectedOrigin, attempt)
      if utxoSnapshotFinalization.isSourceCleanupStarted(expectedOrigin.snapshotBlockId) =>
      retryUtxoSnapshotSourceCleanup(state, expectedOrigin, attempt)

    case _: RetryUtxoSnapshotSourceCleanup =>
      log.debug("Ignoring stale UTXO snapshot source cleanup retry")

    //scan mempool transaction
    case ScanOffChain(tx) =>
      val dustLimit = settings.walletSettings.dustLimit
      val newWalletBoxes = WalletScanLogic.extractWalletOutputs(tx, None, state.walletVars, dustLimit)
      val inputs = WalletScanLogic.extractInputBoxes(tx)
      val newState = state.copy(offChainRegistry =
        state.offChainRegistry.updateOnTransaction(newWalletBoxes, inputs, state.walletVars.externalScans)
      )
      context.become(loadedWallet(newState))

    // rescan=true means we serve a user request for rescan from arbitrary height
    case ContinueUtxoSnapshotCatchUp(run, blockHeight, cleanupAttempt)
      if activeUtxoSnapshotRun.contains(run) && finalizingUtxoSnapshot.contains(run) &&
        blockHeight > run.snapshotHeight && blockHeight == state.getWalletHeight + 1 =>
      val definitionValidation = finalizingUtxoSnapshotStatus
        .filter(status => run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId))
        .map(status => validateUtxoSnapshotFinalizationDefinition(state, status))
        .getOrElse(fail("UTXO snapshot finalization status is unavailable during catch-up"))
      definitionValidation match {
        case Failure(t) =>
          enterUtxoSnapshotQuarantine(
            state,
            t.getMessage,
            fence = Some(UtxoSnapshotScanInvalidation(
              run.snapshotHeight, run.snapshotBlockId)),
            persistFence = true)
        case Success(_) =>
      val updateTry = scanUtxoSnapshotCatchUpHeight(state, blockHeight)
      updateTry match {
        case Success(updatedState) =>
          deferredSnapshotBlock = deferredSnapshotBlock.filter(_._1 > blockHeight)
          deferredRollbackReconciliationBlocks =
            deferredRollbackReconciliationBlocks.filter { case (height, _) => height > blockHeight }
          context.become(loadedWallet(updatedState))
          if (blockHeight < utxoSnapshotFullHeight(updatedState)) {
            self ! ContinueUtxoSnapshotCatchUp(run, blockHeight + 1, cleanupAttempt)
          } else {
            finalizingUtxoSnapshotStatus
              .filter(status => run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId))
              .foreach(status =>
                completeUtxoSnapshotFinalization(updatedState, run, status, cleanupAttempt))
          }
        case Failure(ex: WalletCatchUpBlockUnavailable)
          if isWalletCatchUpBlockDefinitelyPruned(ex) =>
          val snapshot = finalizingUtxoSnapshot
          snapshot.foreach { current =>
            val failure = new IllegalStateException(
              s"Mandatory post-snapshot catch-up failed at height $blockHeight: ${ex.getMessage}", ex)
            utxoSnapshotFinalization =
              utxoSnapshotFinalization.catchUpFailed(current.snapshotBlockId)
            enterUtxoSnapshotQuarantine(
              state,
              walletCatchUpPruningReason(
                s"UTXO snapshot at height ${current.snapshotHeight}", failure),
              fence = None,
              persistFence = false)
          }
        case Failure(ex: WalletCatchUpBlockUnavailable) =>
          blockRecoverableUtxoSnapshotCatchUp(
            state, run, s"at height $blockHeight", ex)
        case Failure(ex) =>
          blockRecoverableUtxoSnapshotCatchUp(
            state, run, s"at height $blockHeight", ex)
      }
      }

    case _: ContinueUtxoSnapshotCatchUp =>
      log.debug("Ignoring stale UTXO snapshot catch-up")

    case ScanInThePast(blockHeight, rescan) =>
      if (hasPendingUtxoSnapshotScan(state)) {
        log.debug(s"Delaying wallet scan from height $blockHeight until UTXO snapshot scan is completed")
      } else {
        val nextBlockHeight = state.expectedNextBlockHeight(blockHeight, settings.nodeSettings.isFullBlocksPruned)
        if (nextBlockHeight == blockHeight || rescan) {
          val blockRead =
            if (rescan) Try(historyReader.bestFullBlockAt(blockHeight).map(_ -> false))
            else readMandatoryWalletCatchUpBlock(blockHeight)
          val scanResult = blockRead.flatMap {
              case Some((block, retainedInActor)) =>
                val operation = if (rescan) "rescanning" else "scanning"
                val validation =
                  if (rescan) Success(())
                  else validateMandatoryWalletCatchUpBlock(state, blockHeight, block).flatMap { _ =>
                    if (retainedInActor) validateRetainedWalletCatchUpBlock(blockHeight, block)
                    else Success(())
                  }
                validation.flatMap { _ =>
                  log.info(s"Wallet is $operation a block ${block.id} in the past at height ${block.height}")
                  Try(ergoWalletService.scanBlockUpdate(
                    state, block, settings.walletSettings.dustLimit)).flatten.flatMap { updatedState =>
                    if (rescan || updatedState.getWalletHeight >= blockHeight) Success(updatedState)
                    else fail(
                      s"Mandatory wallet catch-up at height $blockHeight left the wallet at " +
                        s"height ${updatedState.getWalletHeight}")
                  }
                }
              case None if !rescan =>
                Failure(WalletCatchUpBlockUnavailable(blockHeight))
              case None =>
                Success(state)
            }
          scanResult match {
            case Failure(ex: WalletCatchUpBlockUnavailable)
              if isWalletCatchUpBlockDefinitelyPruned(ex) =>
              val reason = walletCatchUpPruningReason(
                s"Wallet state at height ${state.getWalletHeight}", ex)
              quarantineRollbackState(
                state, s"Wallet is quarantined: $reason", requiresRestart = true,
                ex, clearFreshMempool = true)
            case Failure(ex) if !rescan =>
              val reason = mandatoryWalletCatchUpFailureReason(blockHeight, ex)
              quarantineRollbackState(
                state, s"Wallet is quarantined: $reason", requiresRestart = true,
                ex, clearFreshMempool = true)
            case result =>
              val newState = result match {
                case Success(updatedState) =>
                  if (!rescan) {
                    deferredSnapshotBlock = deferredSnapshotBlock.filter(_._1 > blockHeight)
                    deferredRollbackReconciliationBlocks =
                      deferredRollbackReconciliationBlocks.filter {
                        case (height, _) => height > blockHeight
                      }
                  }
                  updatedState
                case Failure(ex) =>
                  val errorMsg = s"Block rescan at height $blockHeight failed : ${ex.getMessage}"
                  log.error(errorMsg, ex)
                  state.copy(error = Some(errorMsg))
              }
              if (rescan && blockHeight < newState.fullHeight) {
                context.become(loadedWallet(newState))
                self ! ScanInThePast(Math.addExact(blockHeight, 1), rescan = true)
              } else if (rescan) {
                log.info(s"Rescanning finished at height $blockHeight")
                context.become(loadedWallet(newState.copy(rescanInProgress = false)))
              } else if (finalizingUtxoSnapshot.nonEmpty) {
                context.become(loadedWallet(newState))
                for {
                  run <- activeUtxoSnapshotRun
                  status <- finalizingUtxoSnapshotStatus
                  if run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId)
                } completeUtxoSnapshotFinalization(newState, run, status, cleanupAttempt = 0)
              } else {
                val requiredTarget = Seq(
                  blockHeight,
                  newState.fullHeight,
                  pendingWalletCatchUpTarget.getOrElse(blockHeight),
                  deferredSnapshotBlock.map(_._1).getOrElse(blockHeight)).max
                if (blockHeight < requiredTarget) {
                  pendingWalletCatchUpTarget = Some(requiredTarget)
                  context.become(loadedWallet(newState))
                  self ! ScanInThePast(Math.addExact(blockHeight, 1), rescan = false)
                } else {
                  reconcileAfterMandatoryWalletCatchUp(newState) match {
                    case Failure(ex) =>
                      val reason =
                        s"Mandatory wallet catch-up final mempool reconciliation failed: ${ex.getMessage}"
                      quarantineRollbackState(
                        newState,
                        s"Wallet is quarantined: $reason",
                        requiresRestart = true,
                        ex,
                        clearFreshMempool = false)
                    case Success(reconciledState) =>
                      pendingWalletCatchUpTarget = None
                      deferredSnapshotBlock = deferredSnapshotBlock
                        .filter(_._1 > reconciledState.getWalletHeight)
                      deferredRollbackReconciliationBlocks =
                        deferredRollbackReconciliationBlocks.filter {
                          case (height, _) => height > reconciledState.getWalletHeight
                        }
                      context.become(loadedWallet(reconciledState))
                      maybeReplayStartupSnapshotEvent()
                  }
                }
              }
          }
        }
      }

    //scan block transactions
    case ScanOnChain(newBlock) =>
      if (state.secretIsSet(settings.walletSettings.testMnemonic)) { // scan blocks only if wallet is initialized
        if (shouldDeferSnapshotBlock(state) && terminalUtxoSnapshot.isEmpty &&
          invalidUtxoSnapshotProgress == false && blockedUtxoSnapshotCatchUp.isEmpty) {
          deferredSnapshotBlock = ErgoWalletActor.latestDeferredSnapshotValue(
            deferredSnapshotBlock,
            newBlock.height -> newBlock)
          log.debug(s"Deferring wallet scan of block ${newBlock.id} until UTXO snapshot finalization completes")
        } else if (hasPendingUtxoSnapshotScan(state)) {
          log.debug(s"Delaying wallet scan of block ${newBlock.id} until UTXO snapshot scan is completed")
        } else {
          val nextBlockHeight = state.expectedNextBlockHeight(newBlock.height, settings.nodeSettings.isFullBlocksPruned)
          if (nextBlockHeight == newBlock.height) {
            log.info(s"Wallet is going to scan a block ${newBlock.id} on chain at height ${newBlock.height}")
            val newState =
              ergoWalletService.scanBlockUpdate(state, newBlock, settings.walletSettings.dustLimit) match {
                case Failure(ex) =>
                  val errorMsg = s"Scanning new block ${newBlock.id} on chain at height ${newBlock.height} failed : ${ex.getMessage}"
                  log.error(errorMsg, ex)
                  state.copy(error = Some(errorMsg))
                case Success(updatedState) =>
                  updatedState
              }
            context.become(loadedWallet(newState))
          } else if (nextBlockHeight < newBlock.height) {
            log.warn(s"Wallet: skipped blocks found starting from $nextBlockHeight, going back to scan them")
            deferredSnapshotBlock = ErgoWalletActor.latestDeferredSnapshotValue(
              deferredSnapshotBlock,
              newBlock.height -> newBlock)
            pendingWalletCatchUpTarget = Some(Math.max(state.fullHeight, newBlock.height))
            context.become(loadedWallet(state))
            self ! ScanInThePast(nextBlockHeight, false)
          } else {
            log.warn(s"Wallet: block in the past reported at ${newBlock.height}, blockId: ${newBlock.id}")
          }
        }
      }

    case Rollback(version: VersionTag) if hasPendingUtxoSnapshotScan(state) =>
      currentUtxoSnapshotBoundary(state) match {
        case Some((snapshotHeight, snapshotBlockId)) =>
          val branchId = versionToId(version)
          historyReader.heightOf(branchId) match {
            case Some(branchHeight) if branchHeight > snapshotHeight ||
              (branchHeight == snapshotHeight && branchId == snapshotBlockId) =>
              state.storage.readUtxoSnapshotScanStatusTry() match {
                case Failure(t) =>
                  enterUtxoSnapshotQuarantine(
                    state,
                    s"Unable to read durable UTXO snapshot progress during rollback: ${t.getMessage}",
                    fence = Some(UtxoSnapshotScanInvalidation(snapshotHeight, snapshotBlockId)),
                    persistFence = true)
                case Success(Some(status)) if status.completed =>
                  activeUtxoSnapshotRun match {
                    case Some(oldRun)
                      if oldRun.hasSnapshot(status.snapshotHeight, status.snapshotBlockId) &&
                        oldRun.hasSnapshot(snapshotHeight, snapshotBlockId) =>
                      readWalletRegistryTip(state) match {
                        case Failure(t) =>
                          enterIndeterminateRollbackQuarantine(state, version, t)
                        case Success((walletHeight, walletVersion)) =>
                          val rollbackRequired = walletHeight > branchHeight ||
                            (walletHeight == branchHeight && walletVersion != Some(branchId))
                          val rollbackTargetVersionOpt =
                            if (walletHeight < branchHeight) {
                              walletVersion.map(versionId => idToVersion(versionId))
                            } else {
                              Some(version)
                            }
                          val continuation: ErgoWalletState => Unit = resumedState => {
                           utxoSnapshotScanner ! AbortUtxoSnapshotScan(oldRun)
                           utxoSnapshotFinalization =
                             utxoSnapshotFinalization.invalidate(snapshotBlockId)
                          utxoSnapshotSourceCleanupErrors -= snapshotBlockId
                          val resumedRun = newUtxoSnapshotRun(snapshotHeight, snapshotBlockId)
                          activeUtxoSnapshotRun = Some(resumedRun)
                          startingUtxoSnapshot = None
                          terminalUtxoSnapshot = None
                          finalizingUtxoSnapshot = Some(resumedRun)
                          finalizingUtxoSnapshotStatus = Some(status)
                           context.become(loadedWallet(resumedState.copy(rescanInProgress = false)))
                           finalizeUtxoSnapshotScan(status, resumedState, resumedRun)
                          }
                          rollbackTargetVersionOpt match {
                            case Some(rollbackTargetVersion) =>
                              beginWalletRollback(
                                state,
                                rollbackTargetVersion,
                                mutateRegistry = rollbackRequired,
                                continuation = Some(continuation),
                                waitForEvidence = true)
                            case None =>
                              enterRollbackPreparationQuarantine(
                                state,
                                version,
                                new IllegalStateException(
                                  s"Wallet registry at height $walletHeight has no version id " +
                                    s"while reconciling completed snapshot rollback to branch " +
                                    s"point $branchHeight"))
                          }
                      }
                    case _ =>
                      enterUtxoSnapshotQuarantine(
                        state,
                        "Completed UTXO snapshot progress has no matching active rollback run",
                        fence = Some(UtxoSnapshotScanInvalidation(snapshotHeight, snapshotBlockId)),
                        persistFence = true)
                  }
                case Success(_) =>
                  log.info(
                    s"Ignoring wallet rollback to height $branchHeight while UTXO snapshot scan " +
                      s"at height $snapshotHeight is pending")
              }
            case branchHeightOpt =>
              val renderedHeight = branchHeightOpt.map(_.toString).getOrElse("unknown")
              val message =
                s"Wallet rollback to height $renderedHeight invalidates UTXO snapshot scan at height $snapshotHeight; a new bootstrap is required"
              enterUtxoSnapshotQuarantine(
                state,
                message,
                fence = Some(UtxoSnapshotScanInvalidation(snapshotHeight, snapshotBlockId)),
                persistFence = true)
          }
        case None =>
          val message = "Wallet rollback refused because UTXO snapshot progress is invalid"
          enterUtxoSnapshotQuarantine(
            state,
            message,
            fence = None,
            persistFence = false)
      }

    case Rollback(version: VersionTag) =>
      // wallet must be initialized for wallet registry rollback
      if (state.secretStorageOpt.isDefined || settings.walletSettings.testMnemonic.isDefined) {
        readUtxoSnapshotWalletOrigin(state) match {
          case Failure(t) =>
            enterUtxoSnapshotQuarantine(
              state,
              s"Unable to read completed UTXO snapshot origin during rollback: ${t.getMessage}",
              fence = None,
              persistFence = false)
          case Success(None) =>
            beginWalletRollback(state, version, waitForEvidence = true)
          case Success(Some(origin)) =>
            val branchId = versionToId(version)
            val branchHeightOpt = historyReader.heightOf(branchId)
            val invalidatesOrigin = branchHeightOpt.forall(_ < origin.snapshotHeight) ||
              branchHeightOpt.contains(origin.snapshotHeight) && branchId != origin.snapshotBlockId
            if (invalidatesOrigin) {
              val renderedHeight = branchHeightOpt.map(_.toString).getOrElse("unknown")
              enterUtxoSnapshotQuarantine(
                state,
                s"Wallet rollback to height $renderedHeight invalidates completed UTXO snapshot " +
                  s"origin at height ${origin.snapshotHeight}",
                fence = Some(UtxoSnapshotScanInvalidation(
                  origin.snapshotHeight, origin.snapshotBlockId)),
                persistFence = true)
            } else {
              beginWalletRollback(state, version, waitForEvidence = true)
            }
        }
      } else {
        log.warn("Avoiding rollback as wallet is not initialized yet")
      }

    /* WALLET COMMANDS */
    case CheckSeed(mnemonic, passOpt) =>
      state.secretStorageOpt match {
        case Some(secretStorage) =>
          val checkResult = secretStorage.checkSeed(mnemonic, passOpt)
          sender() ! checkResult
        case None =>
          sender() ! Failure(new Exception("Wallet not initialized"))
      }

    case UnlockWallet(walletPass) =>
      log.info("Unlocking wallet")
      ergoWalletService.unlockWallet(state, walletPass, settings.walletSettings.usePreEip3Derivation) match {
        case Success(newState) =>
          log.info("Wallet successfully unlocked")
          walletPass.erase()
          resumeOrStartUtxoSnapshotScan(newState)
          if (utxoSnapshotQuarantine.isEmpty) context.become(loadedWallet(newState))
          sender() ! Success(())
        case f@Failure(t) =>
          walletPass.erase()
          log.warn("Wallet unlock failed with: ", t)
          sender() ! f
      }

    case LockWallet =>
      log.info("Locking wallet")
      context.become(loadedWallet(ergoWalletService.lockWallet(state)))

    case CloseWallet =>
      log.info("Closing wallet actor")
      state.storage.close()
      state.registry.close()
      context stop self

    // We do wallet rescan by closing the wallet's database, deleting it from the disk, then reopening it and sending a rescan signal.
    case RescanWallet(fromHeight) =>
      val replyTo = sender()
      readUtxoSnapshotWalletOrigin(state) match {
        case Failure(t) =>
          val err = new IllegalStateException(
            s"Wallet rescan is unavailable because completed UTXO snapshot origin is unreadable: ${t.getMessage}", t)
          log.warn(err.getMessage)
          replyTo ! Failure(err)
        case Success(Some(origin)) =>
          val err = new IllegalStateException(
            s"Wallet rescan is unavailable because this wallet was completed from UTXO snapshot " +
              s"${origin.snapshotBlockId} at height ${origin.snapshotHeight}; a new bootstrap or genesis rebuild is required")
          log.warn(err.getMessage)
          replyTo ! Failure(err)
        case Success(None) if hasPendingUtxoSnapshotScan(state) =>
          val err = new IllegalStateException(
            "Wallet rescan is unavailable while a UTXO snapshot scan is pending")
          log.warn(err.getMessage)
          replyTo ! Failure(err)
        case Success(None) if settings.nodeSettings.utxoSettings.utxoBootstrap &&
          settings.nodeSettings.isFullBlocksPruned =>
          val err = new IllegalStateException(
            "Wallet rescan on a pruned UTXO-bootstrap node requires a new bootstrap")
          log.warn(err.getMessage)
          replyTo ! Failure(err)
        case Success(None) if !state.rescanInProgress =>
          log.info(s"Rescanning the wallet from height: $fromHeight")
          ergoWalletService.recreateRegistry(state, settings) match {
            case Success(newState) =>
              val resetState = newState.copy(
                offChainRegistry = OffChainRegistry.init(newState.registry),
                outputsFilter = None,
                rescanInProgress = true
              )
              context.become(loadedWallet(resetState))
              val heightToScanFrom = Math.min(resetState.fullHeight, fromHeight)
              self ! ScanInThePast(heightToScanFrom, rescan = true)
              replyTo ! Success(())
            case f@Failure(t) =>
              log.error("Error during rescan attempt: ", t)
              replyTo ! f
          }
        case Success(None) =>
          log.info(s"Skipping rescan request from height: $fromHeight as one is already in progress")
          replyTo ! Failure(new IllegalStateException("Rescan already in progress"))
      }

    case GetWalletStatus =>
      val isSecretSet = state.secretIsSet(settings.walletSettings.testMnemonic)
      val isUnlocked = state.walletVars.proverOpt.isDefined
      val changeAddress = state.getChangeAddress(ergoAddressEncoder)
      val height = state.getWalletHeight
      val lastError = state.error
      val status = WalletStatus(isSecretSet, isUnlocked, changeAddress, height, lastError)
      sender() ! status

    case GenerateTransaction(requests, inputsRaw, dataInputsRaw, sign) =>
      val txTry = ergoWalletService.generateTransaction(state, boxSelector, requests, inputsRaw, dataInputsRaw, sign)
      sender() ! txTry

    case GenerateCommitmentsFor(unsignedTx, externalSecretsOpt, externalInputsOpt, externalDataInputsOpt) =>
      val resultTry = ergoWalletService.generateCommitments(state, unsignedTx, externalSecretsOpt, externalInputsOpt, externalDataInputsOpt)
      sender() ! GenerateCommitmentsResponse(resultTry)

    case SignTransaction(tx, secrets, hints, boxesToSpendOpt, dataBoxesOpt) =>
      val txTry =
        ergoWalletService.signTransaction(
          state.walletVars.proverOpt,
          tx,
          secrets,
          hints,
          boxesToSpendOpt,
          dataBoxesOpt,
          state.parameters,
          state.stateContext
        )(state.readBoxFromUtxoWithWalletFallback)
      sender() ! txTry

    case ExtractHints(tx, real, simulated, boxesToSpendOpt, dataBoxesOpt) =>
      val bag = ergoWalletService.extractHints(state, tx, real, simulated, boxesToSpendOpt, dataBoxesOpt)
      sender() ! ExtractHintsResult(bag)

    case DeriveKey(_) if hasPendingUtxoSnapshotScan(state) =>
      sender() ! fail("Key derivation is unavailable while UTXO snapshot progress is unresolved")

    case DeriveKey(encodedPath) =>
      ergoWalletService.deriveKeyFromPath(state, encodedPath, ergoAddressEncoder) match {
        case Success((p2pkAddress, newState)) =>
          context.become(loadedWallet(newState))
          sender() ! Success(p2pkAddress)
        case f@Failure(_) =>
          sender() ! f
      }

    case DeriveNextKey if hasPendingUtxoSnapshotScan(state) =>
      sender() ! DeriveNextKeyResult(
        fail("Key derivation is unavailable while UTXO snapshot progress is unresolved"))

    case DeriveNextKey =>
      ergoWalletService.deriveNextKey(state, settings.walletSettings.usePreEip3Derivation) match {
        case Success((derivationResult, newState)) =>
          context.become(loadedWallet(newState))
          sender() ! derivationResult
        case Failure(t) =>
          sender() ! DeriveNextKeyResult(Failure(t))
      }

    case UpdateChangeAddress(address) =>
      state.storage.updateChangeAddress(address) match {
        case Success(_) =>
          sender() ! StatusReply.success(())
        case Failure(t) =>
          log.error(s"Unable to update change address", t)
          sender() ! StatusReply.error(s"Unable to update change address : ${t.getMessage}")
      }

    case RemoveScan(_) if hasPendingUtxoSnapshotScan(state) =>
      sender() ! RemoveScanResponse(
        fail("Scan removal is unavailable while UTXO snapshot progress is unresolved"))

    case RemoveScan(scanId) =>
      ergoWalletService.removeScan(state, scanId) match {
        case Success(newState) =>
          context.become(loadedWallet(newState))
          sender() ! RemoveScanResponse(Success(()))
        case Failure(t) =>
          log.warn(s"Unable to remove scanId: $scanId", t)
          sender() ! RemoveScanResponse(Failure(t))
      }

    case AddScan(_) if hasPendingUtxoSnapshotScan(state) =>
      sender() ! AddScanResponse(
        fail("Scan registration is unavailable while UTXO snapshot progress is unresolved"))

    case AddScan(appRequest) =>
      ergoWalletService.addScan(state, appRequest) match {
        case Success((scan, newState)) =>
          context.become(loadedWallet(newState))
          sender() ! AddScanResponse(Success(scan))
        case Failure(t) =>
          log.warn(s"Unable to add scan: $appRequest", t)
          sender() ! AddScanResponse(Failure(t))
      }

    case AddBox(_, _) if hasPendingUtxoSnapshotScan(state) =>
      sender() ! AddBoxResponse(
        fail("Box tracking is unavailable while UTXO snapshot progress is unresolved"))

    case AddBox(box: ErgoBox, scanIds: Set[ScanId]) =>
      state.registry.updateScans(scanIds, box)
      sender() ! AddBoxResponse(Success(())) // todo: what is the reasoning behind returning always success?

    case StopTracking(_, _) if hasPendingUtxoSnapshotScan(state) =>
      sender() ! StopTrackingResponse(
        fail("Box tracking is unavailable while UTXO snapshot progress is unresolved"))

    case StopTracking(scanId: ScanId, boxId: BoxId) =>
      sender() ! StopTrackingResponse(state.registry.removeScan(boxId, scanId))

    case CollectWalletBoxes(targetBalance: Long, targetAssets: Map[ErgoBox.TokenId, Long]) =>
      sender() ! ReqBoxesResponse(ergoWalletService.collectBoxes(state, boxSelector, targetBalance, targetAssets))

    case GetScanTransactions(scanId: ScanId, includeUnconfirmed) =>
      val scanTxs = ergoWalletService.getScanTransactions(state, scanId, state.fullHeight, includeUnconfirmed)
      sender() ! ScanRelatedTxsResponse(scanTxs)

    case GetFilteredScanTxs(scanIds, minHeight, maxHeight, minConfNum, maxConfNum, includeUnconfirmed)  =>
      readFiltered(state, scanIds, minHeight, maxHeight, minConfNum, maxConfNum, includeUnconfirmed)

  }

  def readFiltered(state: ErgoWalletState,
                   scanIds: List[ScanId],
                   minHeight: Int,
                   maxHeight: Int,
                   minConfNum: Int,
                   maxConfNum: Int,
                   includeUnconfirmed: Boolean): Unit = {
    val heightFrom = if (maxConfNum == Int.MaxValue) {
      minHeight
    } else {
      Math.max(minHeight, state.fullHeight - maxConfNum)
    }
    val heightTo = if (minConfNum == 0) {
      maxHeight
    } else {
      Math.min(maxHeight,  - minConfNum)
    }
    log.debug("Starting to read wallet transactions")
    val ts0 = System.currentTimeMillis()
    val txs = scanIds.flatMap(scan => state.registry.walletTxsBetween(scan, heightFrom, heightTo))
      .sortBy(-_.inclusionHeight)
      .map(tx => AugWalletTransaction(tx, state.fullHeight - tx.inclusionHeight))
    val ts = System.currentTimeMillis()
    val txsToSend =
      if (includeUnconfirmed && heightTo > state.fullHeight) {
        // in order to include unconfirmed txs, heightTo should be grater than current height
        txs ++ scanIds.flatMap( scanId => ergoWalletService.getUnconfirmedTransactions(state, scanId) )
      } else {
        txs
      }
    log.debug(s"Wallet: ${txsToSend.size} read in ${ts-ts0} ms")
    sender() ! txsToSend
  }

  override def receive: Receive = emptyWallet

  private def wrapLegalExc[T](e: Throwable): Failure[T] =
    if (e.getMessage.startsWith("Illegal key size")) {
      val dkLen = settings.walletSettings.secretStorage.encryption.dkLen
      Failure[T](new Exception(s"Key of length $dkLen is not allowed on your JVM version." +
        s"Set `ergo.wallet.secretStorage.encryption.dkLen = 128` or update JVM"))
    } else {
      Failure[T](e)
    }
}

object ErgoWalletActor extends ScorexLogging {

  private sealed trait CurrentWalletViewApplicationStep
  private final case class ApplyCurrentWalletState(stateReader: ErgoStateReader)
    extends CurrentWalletViewApplicationStep
  private final case class ApplyCurrentWalletMempool(mempoolReader: ErgoMemPoolReader)
    extends CurrentWalletViewApplicationStep
  private final case class ApplyCurrentWalletSnapshot(
    event: UtxoSnapshotAppliedToState,
    replyTo: ActorRef) extends CurrentWalletViewApplicationStep
  private case object ContinueCurrentWalletViewApplication
  private final case class ExecuteCurrentWalletSnapshot(event: UtxoSnapshotAppliedToState)

  private case object CurrentWalletViewRetryTimerKey
  private final case class RetryCurrentWalletViewRequest(requestId: UUID)

  private[wallet] val CurrentWalletViewRetryDelay: FiniteDuration = 1.second

  private final case class WalletCatchUpBlockUnavailable(height: Height)
    extends IllegalStateException(
      s"Required wallet catch-up full block at height $height is unavailable")

  private final case class WalletRollbackEvidencePending(message: String)
    extends IllegalStateException(message)

  private final case class PendingWalletRollbackPreparation(
    intent: WalletRollbackIntent,
    version: VersionTag,
    mutateRegistry: Boolean,
    continuation: Option[ErgoWalletState => Unit],
    freshMempoolReader: Option[ErgoMemPoolReader] = None)

  private[wallet] val MaxFinalizationCleanupRetries: Int = 3
  private[wallet] val FinalizationCleanupRetryDelay: FiniteDuration = 1.second

  private[wallet] def statusBelongsToActiveRun(run: UtxoSnapshotScanRun,
                                                status: UtxoSnapshotScanStatus): Boolean =
    run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId)

  private[wallet] def shouldResumeCompletedActiveRun(
    run: UtxoSnapshotScanRun,
    status: UtxoSnapshotScanStatus,
    finalizingRun: Option[UtxoSnapshotScanRun]): Boolean =
    status.completed && statusBelongsToActiveRun(run, status) && finalizingRun.isEmpty

  private[wallet] def latestDeferredSnapshotValue[A](
    current: Option[(Height, A)],
    incoming: (Height, A)): Option[(Height, A)] =
    current match {
      case Some((height, _)) if height > incoming._1 => current
      case _ => Some(incoming)
    }

  private[wallet] def isWalletCatchUpBlockDefinitelyPruned(
    fullBlocksPruned: Boolean,
    minFullBlockAvailable: Height,
    requestedHeight: Height): Boolean =
    fullBlocksPruned && requestedHeight < minFullBlockAvailable

  /** Start actor and register its proper closing into coordinated shutdown */
  def apply(settings: ErgoSettings,
            parameters: Parameters,
            service: ErgoWalletService,
            boxSelector: BoxSelector,
            historyReader: ErgoHistoryReader)(implicit actorSystem: ActorSystem): ActorRef = {
    val props = Props(classOf[ErgoWalletActor], settings, parameters, service, boxSelector, historyReader)
      .withDispatcher(GlobalConstants.ApiDispatcher)
    val walletActorRef = actorSystem.actorOf(props)
    CoordinatedShutdown(actorSystem).addActorTerminationTask(
      CoordinatedShutdown.PhaseBeforeServiceUnbind,
      s"closing-wallet",
      walletActorRef,
      Some(CloseWallet)
    )
    walletActorRef
  }
}
