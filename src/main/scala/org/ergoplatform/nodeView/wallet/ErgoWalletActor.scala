package org.ergoplatform.nodeView.wallet

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor._
import akka.pattern.StatusReply
import org.ergoplatform.ErgoBox._
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedMempool, ChangedState, UtxoSnapshotAppliedToState}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Height
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletService._
import org.ergoplatform.nodeView.wallet.ErgoWalletServiceUtils.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, UtxoSnapshotScanInvalidation, UtxoSnapshotScanStatus, UtxoSnapshotWalletOrigin}
import org.ergoplatform.sdk.wallet.secrets.DerivationPath
import org.ergoplatform.settings._
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.BoxSelector
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform._
import org.ergoplatform.core.{VersionTag, versionToId}
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
  extends Actor with Stash with ScorexLogging with ScorexEncoding {

  private val ergoAddressEncoder: ErgoAddressEncoder = settings.addressEncoder
  private var utxoSnapshotScanner: ActorRef = _
  private var startingUtxoSnapshot: Option[UtxoSnapshotScanRun] = None
  private var terminalUtxoSnapshot: Option[(Height, ModifierId)] = None
  private var finalizingUtxoSnapshot: Option[UtxoSnapshotScanRun] = None
  private var finalizingUtxoSnapshotStatus: Option[UtxoSnapshotScanStatus] = None
  private var activeUtxoSnapshotRun: Option[UtxoSnapshotScanRun] = None
  private var invalidUtxoSnapshotProgress: Boolean = false
  private var utxoSnapshotQuarantine: Option[UtxoSnapshotQuarantine] = None
  private var blockedUtxoSnapshotCatchUp: Option[(Height, ModifierId)] = None
  private var deferredSnapshotBlock: Option[(Height, ErgoFullBlock)] = None
  private var utxoSnapshotFinalization: UtxoSnapshotFinalizationState =
    UtxoSnapshotFinalizationState.empty

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

  protected[wallet] def readWalletRegistryTip(
    state: ErgoWalletState): Try[(Height, Option[ModifierId])] =
    Try(state.getWalletHeight -> state.registry.lastVersionId)

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
    historyReader.bestFullBlockAt(height).isDefined

  protected[wallet] def scanUtxoSnapshotCatchUpHeight(
    state: ErgoWalletState,
    height: Height): Try[ErgoWalletState] =
    historyReader.bestFullBlockAt(height) match {
      case Some(block) =>
        log.info(s"Wallet is scanning mandatory post-snapshot block ${block.id} at height ${block.height}")
        ergoWalletService.scanBlockUpdate(state, block, settings.walletSettings.dustLimit)
      case None =>
        Failure(new IllegalStateException(
          s"Required wallet catch-up block at height $height is unavailable"))
    }

  protected[wallet] def removeUtxoSnapshotScanStatus(state: ErgoWalletState): Try[Unit] =
    state.storage.removeUtxoSnapshotScanStatus()

  protected[wallet] def removeUtxoSnapshotScanSource(snapshotBlockId: ModifierId): Try[Unit] =
    Try(historyReader.removeUtxoSnapshotScanSource(snapshotBlockId)).flatten

  override def preStart(): Unit = {
    log.info("Initializing wallet actor")
    ErgoWalletState.initial(settings, parameters) match {
      case Success(state) =>
        utxoSnapshotScanner = createUtxoSnapshotScanner()
        context.system.eventStream.subscribe(self, classOf[ChangedState])
        context.system.eventStream.subscribe(self, classOf[ChangedMempool])
        context.system.eventStream.subscribe(self, classOf[UtxoSnapshotAppliedToState])
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
      context.become(loadedWallet(newState))
      resumeOrStartUtxoSnapshotScan(newState)
      unstashAll()
    case _ => // stashing all messages until wallet is setup
      stash()
  }

  private def hasPendingUtxoSnapshotScan(state: ErgoWalletState): Boolean =
    utxoSnapshotQuarantine.nonEmpty || startingUtxoSnapshot.nonEmpty ||
      terminalUtxoSnapshot.nonEmpty || finalizingUtxoSnapshot.nonEmpty ||
      invalidUtxoSnapshotProgress || blockedUtxoSnapshotCatchUp.nonEmpty ||
      (state.storage.readUtxoSnapshotScanStatusTry() match {
      case Success(status) => status.exists(!_.completed)
      case Failure(_) => true
    })

  private def shouldDeferSnapshotBlock(state: ErgoWalletState): Boolean =
    startingUtxoSnapshot.nonEmpty || finalizingUtxoSnapshot.nonEmpty ||
      state.storage.readUtxoSnapshotScanStatusTry().toOption.flatten.exists(!_.completed)

  private def currentUtxoSnapshotBoundary(state: ErgoWalletState): Option[(Height, ModifierId)] =
    startingUtxoSnapshot.map(run => run.snapshotHeight -> run.snapshotBlockId)
      .orElse(terminalUtxoSnapshot)
      .orElse(finalizingUtxoSnapshot.map(run => run.snapshotHeight -> run.snapshotBlockId))
      .orElse(blockedUtxoSnapshotCatchUp)
      .orElse(state.storage.readUtxoSnapshotScanStatusTry().toOption.flatten
        .filterNot(_.completed).map(s => s.snapshotHeight -> s.snapshotBlockId))

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
    terminalUtxoSnapshot = fence.map(marker => marker.snapshotHeight -> marker.snapshotBlockId)
    invalidUtxoSnapshotProgress = true
    utxoSnapshotQuarantine = Some(UtxoSnapshotQuarantine(quarantineReason, fence))
    fence.foreach(marker =>
      utxoSnapshotFinalization = utxoSnapshotFinalization.invalidate(marker.snapshotBlockId))
    run.foreach(active => utxoSnapshotScanner ! AbortUtxoSnapshotScan(active))
    log.error(quarantineReason)
    context.become(loadedWallet(state.copy(error = Some(quarantineReason), rescanInProgress = false)))
  }

  private def startUtxoSnapshotScan(snapshotHeight: Height,
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
                Try(restartUtxoSnapshotScanRecovery(
                  normalizedState, plan.expected, plan.freshStatus)).flatten match {
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
                      snapshotHeight, snapshotBlockId, forceRestart = true) match {
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
    val shouldStart =
      settings.nodeSettings.utxoSettings.utxoBootstrap &&
        settings.nodeSettings.isFullBlocksPruned &&
        canScanUtxoSnapshot(state) &&
        state.getWalletHeight == 0 &&
        !state.rescanInProgress &&
        state.storage.readUtxoSnapshotScanStatusTry() == Success(None) &&
        state.storage.readUtxoSnapshotWalletOriginTry() == Success(None)

    if (shouldStart) {
      state.utxoStateReaderOpt
        .orElse(state.stateReaderOpt.collect { case reader: UtxoStateReader => reader })
        .foreach { reader =>
          startUtxoSnapshotScan(reader.stateContext.currentHeight, versionToId(reader.version), forceRestart = false)
        }
    }
  }

  private def statusMatchesUtxoSnapshotOrigin(status: UtxoSnapshotScanStatus,
                                               origin: UtxoSnapshotWalletOrigin): Boolean =
    status.snapshotHeight == origin.snapshotHeight &&
      status.snapshotBlockId == origin.snapshotBlockId &&
      status.scanDefinition == origin.scanDefinition

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
          val fullHeight = utxoSnapshotFullHeight(state)
          if (registryHeight < status.snapshotHeight || registryHeight > fullHeight) {
            Failure(new IllegalStateException(
              s"Completed UTXO snapshot registry tip height $registryHeight is outside " +
                s"[${status.snapshotHeight}, $fullHeight]"))
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
          startUtxoSnapshotScan(status.snapshotHeight, status.snapshotBlockId, forceRestart = false)
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
          case Success(originOpt) => state.storage.readUtxoSnapshotScanStatusTry() match {
          case Failure(t) =>
            enterUtxoSnapshotQuarantine(
              state,
              s"Unreadable durable UTXO snapshot scan status: ${t.getMessage}",
              fence = None,
              persistFence = false)
          case Success(Some(status))
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
          case Success(Some(status)) =>
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
                validatePersistedUtxoSnapshotStatusAfterDefinition(state, status) match {
                  case Failure(t) =>
                    enterUtxoSnapshotQuarantine(
                      state,
                      t.getMessage,
                      fence = Some(UtxoSnapshotScanInvalidation(
                        status.snapshotHeight, status.snapshotBlockId)),
                      persistFence = true)
                  case Success(_) if status.completed && originOpt.isEmpty =>
                    state.storage.completeUtxoSnapshotScan(status) match {
                      case Success(_) => resumeOrStartValidatedUtxoSnapshotScan(state, Some(status))
                      case Failure(t) =>
                        enterUtxoSnapshotQuarantine(
                          state,
                          s"Unable to backfill completed UTXO snapshot wallet origin: ${t.getMessage}",
                          fence = Some(UtxoSnapshotScanInvalidation(
                            status.snapshotHeight, status.snapshotBlockId)),
                          persistFence = true)
                    }
                  case Success(_) =>
                    resumeOrStartValidatedUtxoSnapshotScan(state, Some(status))
                }
              }
          case Success(None) if originOpt.nonEmpty && activeUtxoSnapshotRun.isEmpty =>
            log.debug("Completed UTXO snapshot wallet origin is present; no scan resume is required")
          case Success(None)
            if activeUtxoSnapshotRun.exists(startingUtxoSnapshot.contains) =>
            log.debug("UTXO snapshot wallet scan status is not initialized for the starting actor run")
          case Success(None) if activeUtxoSnapshotRun.nonEmpty =>
            val active = activeUtxoSnapshotRun.get
            enterUtxoSnapshotQuarantine(
              state,
              "Durable UTXO snapshot progress disappeared while a run was active",
              Some(UtxoSnapshotScanInvalidation(
                active.snapshotHeight, active.snapshotBlockId)),
              persistFence = true)
          case Success(None) =>
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
    val catchUpHeight = Math.max(status.snapshotHeight + 1, state.getWalletHeight + 1)
    val catchUpRequired = catchUpHeight <= utxoSnapshotFullHeight(state)
    // Full blocks retained after the snapshot form a contiguous suffix. Checking its
    // first mandatory height therefore proves that the whole catch-up interval is readable.
    val catchUpReady = !catchUpRequired || isUtxoSnapshotCatchUpReady(catchUpHeight)
    val plan = utxoSnapshotFinalization.plan(status, catchUpReady)
    utxoSnapshotFinalization = plan.state
    if (plan.scheduleCatchUp) {
      blockedUtxoSnapshotCatchUp = None
      if (catchUpRequired) {
        self ! ContinueUtxoSnapshotCatchUp(run, catchUpHeight)
      } else {
        completeUtxoSnapshotFinalization(state, run, status, cleanupAttempt = 0)
      }
    } else if (!catchUpReady) {
      utxoSnapshotFinalization =
        utxoSnapshotFinalization.catchUpFailed(status.snapshotBlockId)
      finalizingUtxoSnapshot = None
      finalizingUtxoSnapshotStatus = None
      self ! UtxoSnapshotCatchUpFailed(run, status,
        s"Required wallet catch-up block at height $catchUpHeight is unavailable")
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
              state.storage.completeUtxoSnapshotScan(status)
                .flatMap(_ => Try(removeUtxoSnapshotScanStatus(state)).flatten) match {
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
                case Success(_) =>
                  utxoSnapshotFinalization =
                    utxoSnapshotFinalization.catchUpCompleted(status.snapshotBlockId)
                  finalizingUtxoSnapshot = None
                  finalizingUtxoSnapshotStatus = None
                  activeUtxoSnapshotRun = None
                  blockedUtxoSnapshotCatchUp = None
                  val pendingBlock = deferredSnapshotBlock
                    .filter { case (height, _) => height > state.getWalletHeight }
                    .map(_._2)
                  deferredSnapshotBlock = None
                  val completedState = state.copy(error = None, rescanInProgress = false)
                  removeUtxoSnapshotScanSource(status.snapshotBlockId) match {
                    case Success(_) =>
                      utxoSnapshotFinalization =
                        utxoSnapshotFinalization.cleanupSucceeded(status.snapshotBlockId)
                      context.become(loadedWallet(completedState))
                    case Failure(t) =>
                      val message =
                        s"Unable to remove completed UTXO snapshot scan source: ${t.getMessage}"
                      log.error(message, t)
                      context.become(loadedWallet(completedState.copy(error = Some(message))))
                  }
                  pendingBlock.foreach(self ! ScanOnChain(_))
              }
          }
      }
      }
    }
  }

  private def fail[T](message: String): Failure[T] =
    Failure(new IllegalStateException(message))

  private def loadedWallet(state: ErgoWalletState): Receive =
    if (utxoSnapshotQuarantine.nonEmpty) quarantinedWallet(state) else operationalWallet(state)

  private def quarantinedWallet(state: ErgoWalletState): Receive = {
    case GetWalletStatus =>
      val quarantineReason = utxoSnapshotQuarantine.map(_.reason).orElse(state.error)
      sender() ! WalletStatus(
        state.secretIsSet(settings.walletSettings.testMnemonic),
        state.walletVars.proverOpt.isDefined,
        state.getChangeAddress(ergoAddressEncoder),
        state.getWalletHeight,
        quarantineReason)

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
          context.become(loadedWallet(state.copy(error = Some(utxoSnapshotQuarantine.get.reason))))
      }

    case UtxoSnapshotAppliedToState(height, blockId, stateReader) =>
      recoverQuarantinedUtxoSnapshot(state, height, blockId, stateReader, sender())

    case GetOrInitUtxoSnapshotScanStatus(_, _, _) | ApplyUtxoSnapshotScanBatch(_, _, _, _, _) =>
      sender() ! fail(utxoSnapshotQuarantine.get.reason)

    case _: UtxoSnapshotScanTerminated | _: FinalizeUtxoSnapshotScan |
         _: UtxoSnapshotCatchUpFailed | _: UtxoSnapshotCleanupFailed |
         _: ContinueUtxoSnapshotCatchUp =>
      log.debug("Ignoring stale UTXO snapshot lifecycle message while the wallet is quarantined")

    case InitWallet(walletPass, mnemonicPassOpt) =>
      walletPass.erase()
      mnemonicPassOpt.foreach(_.erase())
      sender() ! fail(utxoSnapshotQuarantine.get.reason)

    case RestoreWallet(mnemonic, mnemonicPassOpt, walletPass, _) =>
      mnemonic.erase()
      mnemonicPassOpt.foreach(_.erase())
      walletPass.erase()
      sender() ! fail(utxoSnapshotQuarantine.get.reason)

    case UnlockWallet(walletPass) =>
      walletPass.erase()
      sender() ! fail(utxoSnapshotQuarantine.get.reason)

    case CheckSeed(mnemonic, passOpt) =>
      mnemonic.erase()
      passOpt.foreach(_.erase())
      sender() ! fail(utxoSnapshotQuarantine.get.reason)

    case GetPrivateKeyFromPath(_) | GenerateTransaction(_, _, _, _) |
         SignTransaction(_, _, _, _, _) | RescanWallet(_) | DeriveKey(_) =>
      sender() ! fail(utxoSnapshotQuarantine.get.reason)

    case GenerateCommitmentsFor(_, _, _, _) =>
      sender() ! GenerateCommitmentsResponse(fail(utxoSnapshotQuarantine.get.reason))

    case DeriveNextKey =>
      sender() ! DeriveNextKeyResult(fail(utxoSnapshotQuarantine.get.reason))

    case GetFirstSecret =>
      sender() ! FirstSecretResponse(fail(utxoSnapshotQuarantine.get.reason))

    case CollectWalletBoxes(_, _) =>
      sender() ! ReqBoxesResponse(fail(utxoSnapshotQuarantine.get.reason))

    case AddScan(_) =>
      sender() ! AddScanResponse(fail(utxoSnapshotQuarantine.get.reason))

    case RemoveScan(_) =>
      sender() ! RemoveScanResponse(fail(utxoSnapshotQuarantine.get.reason))

    case AddBox(_, _) =>
      sender() ! AddBoxResponse(fail(utxoSnapshotQuarantine.get.reason))

    case StopTracking(_, _) =>
      sender() ! StopTrackingResponse(fail(utxoSnapshotQuarantine.get.reason))

    case _ =>
      sender() ! akka.actor.Status.Failure(new IllegalStateException(utxoSnapshotQuarantine.get.reason))
  }

  private def operationalWallet(state: ErgoWalletState): Receive = {
    case InitWallet(walletPass, _) if hasPendingUtxoSnapshotScan(state) =>
      walletPass.erase()
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
    case ChangedMempool(mr: ErgoMemPoolReader@unchecked) =>
      val newState = ergoWalletService.updateUtxoState(state.copy(mempoolReaderOpt = Some(mr)))
      context.become(loadedWallet(newState))

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
          context.become(loadedWallet(newState))
          resumeOrStartUtxoSnapshotScan(newState)
        case Failure(t) =>
          val errorMsg = s"Updating wallet state context failed : ${t.getMessage}"
          log.error(errorMsg, t)
          context.become(loadedWallet(state.copy(error = Some(errorMsg))))
      }

    case UtxoSnapshotAppliedToState(height, blockId, _) =>
      val replyTo = sender()
      if (canScanUtxoSnapshot(state)) {
        state.storage.readUtxoSnapshotWalletOriginTry() match {
          case Success(originOpt) => state.storage.readUtxoSnapshotScanStatusTry() match {
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
                  height, blockId, state.getWalletHeight, state.rescanInProgress, Some(status)) =>
                  replyTo ! Success(startUtxoSnapshotScan(height, blockId, forceRestart = false))
                case Success(_) =>
                  log.debug(s"Skipping ineligible UTXO snapshot wallet scan at height $height")
                  replyTo ! Success(None)
              }
            case Success(None) if originOpt.nonEmpty =>
              log.debug("Skipping UTXO snapshot wallet scan because completed origin is present")
              replyTo ! Success(None)
            case Success(None) =>
              Try(calculateUtxoSnapshotScanDefinition(state)).flatten match {
                case Failure(t) =>
                  enterUtxoSnapshotQuarantine(
                    state,
                    s"Unable to calculate the live UTXO snapshot scan definition: ${t.getMessage}",
                    fence = None,
                    persistFence = false)
                  replyTo ! Failure(t)
                case Success(_) if UtxoSnapshotScanStartPolicy.shouldStartApplied(
                  height, blockId, state.getWalletHeight, state.rescanInProgress, None) =>
                  replyTo ! Success(startUtxoSnapshotScan(height, blockId, forceRestart = false))
                case Success(_) =>
                  log.debug(s"Skipping ineligible UTXO snapshot wallet scan at height $height")
                  replyTo ! Success(None)
              }
          }
          case Failure(t) =>
            enterUtxoSnapshotQuarantine(
              state,
              s"Unreadable durable UTXO snapshot wallet origin: ${t.getMessage}",
              fence = None,
              persistFence = false)
            replyTo ! Failure(t)
        }
      } else {
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
            nextSubtreeIndex == Math.min(status.totalSubtrees,
              subtreeIndex + UtxoSnapshotWalletScanner.SnapshotScanBatchSize) &&
            (status.nextSubtreeIndex == subtreeIndex || status.nextSubtreeIndex >= nextSubtreeIndex) &&
            completed == (nextSubtreeIndex == status.totalSubtrees) =>
          validateLiveUtxoSnapshotScanDefinition(state, status).flatMap { _ =>
            val updatedStatus = if (status.nextSubtreeIndex == subtreeIndex) {
              status.copy(nextSubtreeIndex = nextSubtreeIndex, completed = completed)
            } else {
              status
            }
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
          context.become(loadedWallet(finalState))
          if (updatedStatus.completed) {
            finalizingUtxoSnapshot = Some(run)
            finalizingUtxoSnapshotStatus = Some(updatedStatus)
          }
          replyTo ! Success(updatedStatus)
          if (updatedStatus.completed) {
            self ! FinalizeUtxoSnapshotScan(run, updatedStatus)
          }
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
      blockedUtxoSnapshotCatchUp = Some(status.snapshotHeight -> status.snapshotBlockId)
      log.error(message)
      context.become(loadedWallet(state.copy(error = Some(message), rescanInProgress = false)))

    case _: UtxoSnapshotCatchUpFailed =>
      log.debug("Ignoring stale UTXO snapshot catch-up failure")

    case UtxoSnapshotCleanupFailed(run, message) if activeUtxoSnapshotRun.contains(run) =>
      log.error(message)
      context.become(loadedWallet(state.copy(error = Some(message))))

    case _: UtxoSnapshotCleanupFailed =>
      log.debug("Ignoring stale UTXO snapshot cleanup failure")

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
    case ContinueUtxoSnapshotCatchUp(run, blockHeight)
      if activeUtxoSnapshotRun.contains(run) && finalizingUtxoSnapshot.contains(run) =>
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
          context.become(loadedWallet(updatedState))
          if (blockHeight < utxoSnapshotFullHeight(updatedState)) {
            self ! ContinueUtxoSnapshotCatchUp(run, blockHeight + 1)
          } else {
            finalizingUtxoSnapshotStatus
              .filter(status => run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId))
              .foreach(status =>
                completeUtxoSnapshotFinalization(updatedState, run, status, cleanupAttempt = 0))
          }
        case Failure(ex) =>
          val snapshot = finalizingUtxoSnapshot.map(current =>
            current.snapshotHeight -> current.snapshotBlockId)
          val errorMsg = s"Mandatory post-snapshot catch-up failed at height $blockHeight: ${ex.getMessage}"
          snapshot.foreach { case (_, snapshotBlockId) =>
            utxoSnapshotFinalization = utxoSnapshotFinalization.catchUpFailed(snapshotBlockId)
          }
          blockedUtxoSnapshotCatchUp = snapshot
          finalizingUtxoSnapshot = None
          finalizingUtxoSnapshotStatus = None
          log.error(errorMsg, ex)
          context.become(loadedWallet(state.copy(error = Some(errorMsg))))
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
          val newState =
            historyReader.bestFullBlockAt(blockHeight) match {
              case Some(block) =>
                val operation = if (rescan) "rescanning" else "scanning"
                log.info(s"Wallet is $operation a block ${block.id} in the past at height ${block.height}")
                ergoWalletService.scanBlockUpdate(state, block, settings.walletSettings.dustLimit) match {
                  case Failure(ex) =>
                    val errorMsg = s"Block ${block.id} $operation at height $blockHeight failed : ${ex.getMessage}"
                    log.error(errorMsg, ex)
                    state.copy(error = Some(errorMsg))
                  case Success(updatedState) =>
                    updatedState
                }
              case None =>
                state // We may do not have a block if, for example, the blockchain is pruned. This is okay, just skip it.
            }
          context.become(loadedWallet(newState))
          val mandatorySnapshotCatchUpFailed = finalizingUtxoSnapshot.nonEmpty && newState.error != state.error
          if (mandatorySnapshotCatchUpFailed) {
            log.error(s"Stopping mandatory UTXO snapshot catch-up at height $blockHeight")
          } else if (blockHeight < newState.fullHeight) {
            self ! ScanInThePast(blockHeight + 1, rescan)
          } else if (rescan) {
            log.info(s"Rescanning finished at height $blockHeight")
            context.become(loadedWallet(newState.copy(rescanInProgress = false)))
          } else if (finalizingUtxoSnapshot.nonEmpty) {
            for {
              run <- activeUtxoSnapshotRun
              status <- finalizingUtxoSnapshotStatus
              if run.hasSnapshot(status.snapshotHeight, status.snapshotBlockId)
            } completeUtxoSnapshotFinalization(newState, run, status, cleanupAttempt = 0)
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
            self ! ScanInThePast(nextBlockHeight, false)
          } else {
            log.warn(s"Wallet: block in the past reported at ${newBlock.height}, blockId: ${newBlock.id}")
          }
        }
      }

    case Rollback(version: VersionTag) if hasPendingUtxoSnapshotScan(state) =>
      currentUtxoSnapshotBoundary(state) match {
        case Some((snapshotHeight, snapshotBlockId)) =>
          historyReader.heightOf(versionToId(version)) match {
            case Some(branchHeight) if branchHeight >= snapshotHeight =>
              if (finalizingUtxoSnapshot.nonEmpty && state.getWalletHeight > branchHeight) {
                state.registry.rollback(version) match {
                  case Success(_) =>
                    deferredSnapshotBlock = None
                    blockedUtxoSnapshotCatchUp = None
                    val resumedState = state.copy(
                      offChainRegistry = OffChainRegistry.init(state.registry),
                      outputsFilter = None,
                      error = None)
                    context.become(loadedWallet(resumedState))
                    activeUtxoSnapshotRun.foreach(run =>
                      self ! ContinueUtxoSnapshotCatchUp(run, branchHeight + 1))
                  case Failure(t) =>
                    val message = s"Wallet rollback to height $branchHeight failed during UTXO snapshot catch-up: ${t.getMessage}"
                    log.error(message, t)
                    context.become(loadedWallet(state.copy(error = Some(message))))
                }
              } else {
                log.info(s"Ignoring wallet rollback to height $branchHeight while UTXO snapshot scan at height $snapshotHeight is pending")
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
        state.registry.rollback(version) match {
          case Failure(t) =>
            val errorMsg = s"Failed to rollback wallet registry to version $version due to: ${t.getMessage}"
            log.error(errorMsg, t)
            context.become(loadedWallet(state.copy(error = Some(errorMsg))))
          case _: Success[Unit] =>
            // Reset outputs Bloom filter to have it initialized again on next block scanned
            // todo: for offchain registry, refresh is also needed, https://github.com/ergoplatform/ergo/issues/1180
            context.become(loadedWallet(state.copy(outputsFilter = None)))
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
          context.become(loadedWallet(newState))
          resumeOrStartUtxoSnapshotScan(newState)
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
