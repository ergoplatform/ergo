package org.ergoplatform.nodeView.wallet

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor._
import akka.pattern.StatusReply
import org.ergoplatform.ErgoBox._
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, ChangedMempool, ChangedState}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.ErgoHistoryReader._
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.modifiers.history.header.PreGenesisHeader
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.wallet.persistence.OffChainRegistry
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, ErgoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletService.ChangeAddressValidationException
import org.ergoplatform.nodeView.wallet.ErgoWalletServiceUtils.DeriveNextKeyResult
import org.ergoplatform.sdk.wallet.secrets.DerivationPath
import org.ergoplatform.settings._
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.BoxSelector
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.nodeView.wallet.ErgoWalletActor.RescanCurrentView
import org.ergoplatform._
import org.ergoplatform.core.VersionTag
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.utils.ScorexEncoding
import scorex.util.ScorexLogging
import scorex.util.ModifierId

import scala.concurrent.duration._
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class ErgoWalletActor(protected val settings: ErgoSettings,
                      parameters: Parameters,
                      ergoWalletService: ErgoWalletService,
                      boxSelector: BoxSelector,
                      protected val historyReader: ErgoHistoryReader,
                      nodeViewHolderRef: Option[ActorRef] = None)
  extends Actor with Stash with ScorexLogging with ScorexEncoding with WalletForkRecovery {

  private val ergoAddressEncoder: ErgoAddressEncoder = settings.addressEncoder
  protected[wallet] case object ContinueFullChainProbe
  protected[wallet] case object RetryFullChainProbe
  protected[wallet] case object CompleteRescanRecovery
  protected[wallet] var pendingChainMessages = 0
  private var probeRetryScheduled = false
  protected[wallet] var supersedingRollback: Option[VersionTag] = None
  // A rollback can leave the two registry databases temporarily inconsistent.
  // Status must use the last checked height until the durable intent is cleared.
  protected[wallet] var retainedRollbackSourceHeight: Option[Int] = None
  private[wallet] case class ScanSelectedInThePast(height: Int, epoch: Long)
  private case class SelectedScanPlan(tip: ModifierId, tipHeight: Int, startHeight: Int,
                                      anchors: Vector[(ModifierId, Int)], anchorIndex: Int,
                                      batch: Vector[(Int, ModifierId)], nextHeight: Int, epoch: Long)
  private var completedBodyProbeAnchors = Vector.empty[FullChainCursor]
  private var selectedScanPlan: Option[SelectedScanPlan] = None
  private var selectedScanEpoch = 0L
  private var rescanRecoveryActive = false
  private var pendingRescanCompletion: Option[(ModifierId, Int)] = None
  private var rescanSnapshotEpoch = 0L
  private var awaitingRescanSnapshot = false
  private var deferredRescanOffChain = Vector.empty[ErgoTransaction]
  private var deferredRescanOffChainOverflowed = false

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

  override def preStart(): Unit = {
    log.info("Initializing wallet actor")
    ErgoWalletState.initial(settings, parameters) match {
      case Success(state) =>
        context.system.eventStream.subscribe(self, classOf[ChangedState])
        context.system.eventStream.subscribe(self, classOf[ChangedMempool])
        context.system.eventStream.subscribe(self, classOf[ChangedHistory])
        self ! ReadWallet(state)
      case Failure(ex) =>
        log.error("Unable to initialize wallet", ex)
        ErgoApp.shutdownSystem()(context.system)
    }
  }

  private def emptyWallet: Receive = {
    case ReadWallet(state) =>
      if (!handlePendingRetainedRollbackOnStart(state)) {
      state.storage.rescanRecoveryIntent match {
        case Success(true) =>
          context.become(quarantinedWallet(state,
            new IllegalStateException("Wallet rescan recovery is incomplete")))
          unstashAll()
        case Failure(t) =>
          context.become(quarantinedWallet(state,
            new IllegalStateException("Wallet rescan-recovery intent is unreadable", t)))
          unstashAll()
        case Success(false) => state.storage.deepForkQuarantine match {
        case Success(false) =>
          registryCheckpoint(state) match {
            case Success((tip, _)) if tip == PreGenesisHeader.id =>
              loadWallet(state)
            case Success((tip, height)) =>
              beginFullChainProbe(state, tip, height)(
                selectedTip => loadWallet(state, Some(selectedTip)),
                otherTip => historyReader.ifHolderAppliedFullTip(otherTip) {
                  persistDeepForkQuarantine(state)
                } match {
                  case Some(persisted) =>
                    finishDeepForkQuarantine(state,
                      s"registry tip $tip is not on the selected full chain", persisted)
                  case None => scheduleFullChainProbeRetry()
                }
              )
            case Failure(t) =>
              enterDeepForkQuarantine(state, s"registry checkpoint is inconsistent: ${t.getMessage}")
          }
        case Success(true) =>
          registryCheckpoint(state) match {
            case Success((tip, height)) if tip != PreGenesisHeader.id =>
              beginFullChainProbe(state, tip, height, requireBodies = true)(
                selectedTip => {
                  val checkpointAtTip = registryCheckpoint(state).toOption.exists(_._1 == selectedTip)
                  if (!checkpointAtTip) {
                    context.become(quarantinedWallet(state,
                      new IllegalStateException("Wallet deep-fork quarantine: registry catch-up is incomplete")))
                    unstashAll()
                  } else {
                  val prepared = readWalletState(state)
                  historyReader.ifHolderAppliedFullTip(selectedTip) {
                    state.storage.clearDeepForkQuarantine().map { _ =>
                      activateWallet(prepared)
                    }
                  } match {
                  case Some(Success(_)) => ()
                  case Some(Failure(t)) =>
                    context.become(quarantinedWallet(state,
                      new IllegalStateException("Wallet deep-fork quarantine could not be cleared", t)))
                    unstashAll()
                  case None => scheduleFullChainProbeRetry()
                  }
                  }
                },
                _ => {
                  context.become(quarantinedWallet(state,
                    new IllegalStateException("Wallet deep-fork quarantine is active")))
                  unstashAll()
                },
                (_, missingHeight) => {
                  context.become(quarantinedWallet(state,
                    new IllegalStateException(s"Wallet deep-fork quarantine: missing body at $missingHeight")))
                  unstashAll()
                }
              )
            case _ =>
              context.become(quarantinedWallet(state,
                new IllegalStateException("Wallet deep-fork quarantine is active")))
          }
        case Failure(t) =>
          context.become(quarantinedWallet(state,
            new IllegalStateException("Wallet deep-fork quarantine marker is unreadable", t)))
        }
      }
      }
      unstashAll()
    case _ => // stashing all messages until wallet is setup
      stash()
  }

  /** A completed rollback may be reopened, but no wallet message can pass the
    * startup fence until the target is proved on the holder's applied chain.
    */
  private def handlePendingRetainedRollbackOnStart(state: ErgoWalletState): Boolean =
    state.storage.retainedRollbackIntent match {
      case Success(None) => false
      case Failure(t) =>
        context.become(quarantinedWallet(state,
          new IllegalStateException("Wallet retained-rollback intent is unreadable", t)))
        true
      case Success(Some(intent)) =>
        if (intent.source == intent.target) {
          context.become(quarantinedWallet(state,
            new IllegalStateException("Pending same-version wallet rollback cannot be resumed")))
          return true
        }
        registryCheckpoint(state) match {
          case Success((tip, height)) if tip == intent.target && tip == PreGenesisHeader.id =>
            completeStartupRetainedRollback(state, intent.source, intent.target, height, None)
          case Success((tip, height)) if tip == intent.target =>
            // Defer holder rollback messages until the old, completed intent is
            // cleared. Normal startup will then inspect the current applied tip.
            retainedRollbackSourceHeight = Some(
              Try(historyReader.heightOf(intent.source)).toOption.flatten.getOrElse(height))
            beginFullChainProbe(state, tip, height)(
              selectedTip => completeStartupRetainedRollback(state,
                intent.source, intent.target, height, Some(selectedTip)),
              otherTip => completeStartupRetainedRollback(state,
                intent.source, intent.target, height, Some(otherTip))
            )
          case _ =>
            context.become(quarantinedWallet(state,
              new IllegalStateException("Wallet retained-rollback checkpoint differs from its target")))
        }
        true
    }

  private def completeStartupRetainedRollback(state: ErgoWalletState,
                                              source: ModifierId,
                                              target: ModifierId,
                                              targetHeight: Int,
                                              selectedTip: Option[ModifierId]): Unit = {
    registryCheckpoint(state) match {
      case Success((`target`, `targetHeight`)) =>
        val clearResult = selectedTip match {
          case Some(tip) => historyReader.ifHolderAppliedFullTip(tip) {
            syncRetainedRollbackCheckpoint(state, target, targetHeight)
              .flatMap(_ => clearRetainedRollbackIntent(state, source, target))
          }
          case None => Some(syncRetainedRollbackCheckpoint(state, target, targetHeight)
            .flatMap(_ => clearRetainedRollbackIntent(state, source, target)))
        }
        clearResult match {
          case Some(Success(_)) =>
            // Recheck the current applied chain from normal startup. A holder
            // rollback arrives before the holder installs its new state, so a
            // queued supersedingRollback must survive this handoff.
            retainedRollbackSourceHeight = None
            context.become(emptyWallet)
            self ! ReadWallet(state)
          case Some(Failure(t)) =>
            context.become(quarantinedWallet(state,
              new IllegalStateException("Wallet retained-rollback intent could not be cleared", t)))
            unstashAll()
          case None =>
            beginFullChainProbe(state, target, targetHeight)(
              tip => completeStartupRetainedRollback(state, source, target, targetHeight, Some(tip)),
              tip => completeStartupRetainedRollback(state, source, target, targetHeight, Some(tip))
            )
        }
      case _ =>
        context.become(quarantinedWallet(state,
          new IllegalStateException("Wallet retained-rollback checkpoint changed before intent clear")))
        unstashAll()
    }
  }

  protected[wallet] def loadedWallet(state: ErgoWalletState): Receive =
    state.storage.rescanRecoveryIntent match {
      case Success(true) =>
        quarantinedWallet(state, new IllegalStateException("Wallet rescan recovery is incomplete"))
      case Failure(t) =>
        quarantinedWallet(state, new IllegalStateException("Wallet rescan-recovery intent is unreadable", t))
      case Success(false) => state.storage.deepForkQuarantine match {
        case Success(false) => activeWallet(state)
        case Success(true) =>
          quarantinedWallet(state, new IllegalStateException("Wallet deep-fork quarantine is active"))
        case Failure(t) =>
          quarantinedWallet(state, new IllegalStateException("Wallet deep-fork quarantine marker is unreadable", t))
      }
    }

  protected def persistDeepForkQuarantine(state: ErgoWalletState): scala.util.Try[Unit] =
    state.storage.quarantineDeepFork()

  protected def rollbackRetainedRegistry(state: ErgoWalletState, version: VersionTag): Try[Unit] =
    state.registry.rollbackDurably(version)

  protected def clearRetainedRollbackIntent(state: ErgoWalletState,
                                            source: ModifierId,
                                            target: ModifierId): Try[Unit] =
    state.storage.clearRetainedRollback(source, target)

  protected def syncRetainedRollbackCheckpoint(state: ErgoWalletState,
                                               target: ModifierId,
                                               height: Int): Try[Unit] =
    state.registry.syncCommittedCheckpoint(target, height)

  private def readWalletState(state: ErgoWalletState): ErgoWalletState = {
    val ws = settings.walletSettings
    ergoWalletService.readWallet(
      state, ws.testMnemonic.map(SecretString.create(_)), ws.testKeysQty, ws.secretStorage
    )
  }

  protected[wallet] def activateWallet(newState: ErgoWalletState): Unit = {
    context.become(loadedWallet(newState))
    pendingChainMessages = 0
    unstashAll()
  }

  private def loadWallet(state: ErgoWalletState,
                         requiredTip: Option[ModifierId] = None): Unit = {
    val prepared = readWalletState(state)
    requiredTip match {
      case Some(tip) =>
        if (historyReader.ifHolderAppliedFullTip(tip) {
          activateWallet(prepared)
        }.isEmpty) scheduleFullChainProbeRetry()
      case None => activateWallet(prepared)
    }
  }

  /** The digest height must describe its exact committed version, even before chain selection. */
  protected[wallet] def registryCheckpoint(state: ErgoWalletState): Try[(ModifierId, Int)] =
    state.registry.committedVersionAndDigest.flatMap { case (tip, digest) => Try {
      if (tip == PreGenesisHeader.id) {
        require(digest.height == WalletDigest.empty.height, "Pre-genesis wallet digest height is not empty")
      } else {
        require(digest.height > 0, "Wallet registry digest has no block height")
        historyReader.heightOf(tip).foreach { tipHeight =>
          require(digest.height == tipHeight, "Wallet registry digest height differs from its committed tip")
        }
      }
      tip -> digest.height
    }}

  protected def probeSelectedFullChain(targetId: ModifierId,
                                       targetHeight: Int,
                                       cursor: Option[FullChainCursor]): FullChainProbe =
    historyReader.appliedFullChainProbe(targetId, targetHeight, cursor)

  protected def probeSelectedFullChainBodies(targetId: ModifierId,
                                             targetHeight: Int,
                                             cursor: Option[FullChainCursor]): FullChainProbe =
    historyReader.appliedFullChainBodyProbe(targetId, targetHeight, cursor)

  /** The body probe's sparse ancestry anchors let catch-up expand at most one
    * fixed-size section at a time while walking the selected full branch forward.
    */
  protected[wallet] def armSelectedCatchUpScan(tip: ModifierId, startHeight: Int): Boolean = {
    historyReader.ifHolderAppliedFullTip(tip) {
      historyReader.heightOf(tip).exists { tipHeight =>
        val anchors = (completedBodyProbeAnchors
          .filter(_.fullTipId == tip)
          .map(c => c.nextId -> c.nextHeight) :+ (tip -> tipHeight))
          .distinct.sortBy(_._2)
        val firstAnchor = anchors.indexWhere(_._2 >= startHeight)
        if (firstAnchor < 0 || startHeight > tipHeight) false
        else {
          selectedScanEpoch += 1
          selectedScanPlan = Some(SelectedScanPlan(tip, tipHeight, startHeight,
            anchors, firstAnchor, Vector.empty, startHeight, selectedScanEpoch))
          self ! ScanSelectedInThePast(startHeight, selectedScanEpoch)
          true
        }
      }
    }.contains(true)
  }

  protected[wallet] def cancelSelectedCatchUpScan(): Unit = {
    selectedScanPlan = None
    completedBodyProbeAnchors = Vector.empty
  }

  /** Accept a durable replay request promptly, then prove and scan its selected
    * applied suffix. A fork quarantine without a height-bound intent remains
    * genesis-only; a loaded wallet may explicitly request a shorter suffix.
    */
  protected[wallet] def beginQuarantinedRescan(state: ErgoWalletState,
                                               fromHeight: Int,
                                                replyTo: ActorRef,
                                                allowFreshSuffix: Boolean = false): Unit = {
    val startHeight = math.max(1, fromHeight)
    val pendingStart = state.storage.pendingRescanStartHeight
    val retainedIntent = state.storage.retainedRollbackIntent
    val appliedTip = Try(historyReader.bestFullBlockIdOpt.flatMap { tip =>
      historyReader.ifHolderAppliedFullTip(tip) {
        historyReader.heightOf(tip).map(height => tip -> height)
      }.flatten
    })
    if (fromHeight < 0) {
      replyTo ! Failure(RescanStartInvalid("Wallet rescan height cannot be negative"))
    } else if (rescanRecoveryActive) {
      replyTo ! Failure(RescanStartConflict("Wallet rescan recovery is already in progress"))
    } else if (retainedIntent.isFailure) {
      val reason = new IllegalStateException("Wallet retained-rollback intent is unreadable",
        retainedIntent.failed.get)
      context.become(quarantinedWallet(state, reason))
      replyTo ! Failure(reason)
    } else if (retainedRollbackSourceHeight.nonEmpty || retainedIntent.get.nonEmpty) {
      replyTo ! Failure(RescanStartConflict(
        "Wallet retained-rollback intent must be resolved before rescan recovery"))
    } else if (pendingStart.isFailure) {
      val reason = new IllegalStateException("Wallet rescan-recovery intent is unreadable",
        pendingStart.failed.get)
      context.become(quarantinedWallet(state, reason))
      replyTo ! Failure(reason)
    } else if (pendingStart.get.exists(_ < startHeight)) {
      replyTo ! Failure(RescanStartInvalid(
        s"Wallet rescan recovery is pending from height ${pendingStart.get.get}; a retry cannot start later"))
    } else if (pendingStart.get.isEmpty && !allowFreshSuffix && startHeight > 1) {
      replyTo ! Failure(RescanStartInvalid(
        "Quarantined wallet recovery without a suffix intent must rescan from genesis"))
    } else if (appliedTip.isFailure) {
      replyTo ! Failure(new IllegalStateException(
        "Selected applied full tip could not be read for wallet rescan", appliedTip.failed.get))
    } else if (appliedTip.get.isEmpty) {
      replyTo ! Failure(RescanStartUnavailable("Selected applied full tip is unavailable for wallet rescan"))
    } else if (startHeight > appliedTip.get.get._2) {
      replyTo ! Failure(RescanStartInvalid(
        s"Wallet rescan height $fromHeight is above selected full tip ${appliedTip.get.get._2}"))
    } else {
      def fail(t: Throwable, recoveryState: ErgoWalletState = state): Unit = {
        rescanRecoveryActive = false
        pendingRescanCompletion = None
        awaitingRescanSnapshot = false
        cancelSelectedCatchUpScan()
        context.become(quarantinedWallet(recoveryState, t))
        pendingChainMessages = 0
        unstashAll()
      }
      val durableStart = pendingStart.get match {
        case Some(old) if startHeight < old => state.storage.restartRescanRecoveryEarlier(fromHeight)
        case _ => state.storage.beginRescanRecovery(fromHeight)
      }
      val markers = durableStart
        .flatMap(_ => state.storage.quarantineDeepFork())
      if (markers.isFailure) {
        val reason = new IllegalStateException("Wallet rescan recovery markers could not be persisted",
          markers.failed.get)
        log.error(reason.getMessage, reason)
        context.become(quarantinedWallet(state, reason))
        replyTo ! Failure(reason)
        ErgoApp.shutdownSystem()(context.system)
        return
      }
      // This durable explicit replay supersedes any rollback queued by the
      // ordinary proof it interrupted. A new holder rollback during replay
      // is handled separately and leaves the typed intent in place.
      supersedingRollback = None
      def beginReplay(selectedTip: ModifierId): Unit = {
        if (!historyReader.ifHolderAppliedFullTip(selectedTip)(true).contains(true)) {
          fail(new IllegalStateException("Selected full tip changed before wallet rescan recovery"))
        } else {
          // The durable intent already fences reads. Registry I/O must not hold
          // the history monitor; the scan and completion recheck this exact tip.
          Try(ergoWalletService.recreateRegistry(state, settings)).flatten match {
            case Failure(t) =>
              // Recreation may already have closed and removed the old registry.
              // Keep the durable fences and restart rather than retaining that handle.
              fail(new IllegalStateException("Wallet rescan registry could not be recreated", t))
              ErgoApp.shutdownSystem()(context.system)
            case Success(rebuilt) =>
              Try(readWalletState(rebuilt)) match {
                case Failure(t) =>
                  // The replacement is now the only live registry. Preserve it
                  // for a retry, even if wallet secret loading failed.
                  fail(new IllegalStateException("Wallet rescan recovery could not start", t), rebuilt)
                case Success(prepared) =>
                  if (armSelectedCatchUpScan(selectedTip, startHeight)) {
                    context.become(quarantinedWallet(prepared,
                      new IllegalStateException("Wallet rescan recovery is in progress")))
                    pendingChainMessages = 0
                    unstashAll()
                  } else {
                    rescanRecoveryActive = false
                    context.become(quarantinedWallet(prepared,
                      new IllegalStateException("Selected full tip changed before wallet rescan replay")))
                    pendingChainMessages = 0
                    unstashAll()
                  }
              }
          }
        }
      }
      rescanRecoveryActive = true
      pendingRescanCompletion = None
      awaitingRescanSnapshot = false
      context.become(quarantinedWallet(state,
        new IllegalStateException("Wallet rescan recovery is in progress")))
      replyTo ! Success(())
      beginFullChainProbe(state, PreGenesisHeader.id,
        if (startHeight == 1) 0 else startHeight - 1, requireBodies = true)(
        selectedTip => beginReplay(selectedTip),
        selectedTip => if (startHeight > 1) beginReplay(selectedTip)
          else fail(new IllegalStateException("Selected full chain is unavailable for wallet rescan recovery")),
        (_, missingHeight) => fail(new IllegalStateException(
          s"Selected full block body is missing at height $missingHeight; wallet rescan recovery requires replay from height $startHeight"))
      )
    }
  }

  protected[wallet] def stopIncompleteRescanRecovery(state: ErgoWalletState,
                                                     detail: String): Unit = {
    rescanRecoveryActive = false
    pendingRescanCompletion = None
    awaitingRescanSnapshot = false
    cancelSelectedCatchUpScan()
    val reason = new IllegalStateException(s"Wallet rescan recovery is incomplete: $detail")
    log.error(reason.getMessage)
    context.become(quarantinedWallet(state, reason))
    pendingChainMessages = 0
    unstashAll()
  }

  protected[wallet] def selectedRecoveryScanInProgress: Boolean = rescanRecoveryActive

  protected[wallet] def rescanIntentPending(state: ErgoWalletState): Boolean =
    state.storage.pendingRescanStartHeight.toOption.flatten.isDefined

  protected[wallet] def runSelectedRecoveryScan(state: ErgoWalletState,
                                                 message: ScanSelectedInThePast): Unit =
    activeWallet(state)(message)

  protected[wallet] def syncRescanRegistryCheckpoint(state: ErgoWalletState,
                                                     tip: ModifierId,
                                                     height: Int): Try[Unit] =
    state.registry.syncCommittedCheckpoint(tip, height)

  /** Capture node-view events during replay without advancing the persisted
    * signing context ahead of the still-fenced wallet registry.
    */
  protected[wallet] def captureRescanStateReader(state: ErgoWalletState,
                                                 reader: ErgoStateReader): Try[ErgoWalletState] =
    Try(reader.stateContext).flatMap(captureRescanStateReader(state, reader, _))

  protected[wallet] def captureRescanStateReader(state: ErgoWalletState,
                                                 reader: ErgoStateReader,
                                                 current: ErgoStateContext): Try[ErgoWalletState] =
    state.walletVars.withParameters(current.currentParameters).flatMap { vars =>
      Try(ergoWalletService.updateUtxoState(state.copy(
        stateReaderOpt = Some(reader), utxoStateReaderOpt = None,
        parameters = current.currentParameters, walletVars = vars
      )))
    }

  protected[wallet] def captureRescanMempoolReader(state: ErgoWalletState,
                                                    reader: ErgoMemPoolReader): Try[ErgoWalletState] =
    Try(ergoWalletService.updateUtxoState(state.copy(mempoolReaderOpt = Some(reader))))

  /** Reconstruct unconfirmed wallet state from the holder's final pool view,
    * or from the direct wallet's pool reader and deferred ScanOffChain notices.
    * The durable rescan fence remains in place until this succeeds.
    */
  private def rebuildRescanOffChain(state: ErgoWalletState,
                                   transactions: Seq[ErgoTransaction]): Try[ErgoWalletState] = Try {
    val byId = transactions.map(tx => tx.id -> tx).toMap
    require(byId.size == transactions.size, "Duplicate transaction in wallet rescan mempool snapshot")
    val producerByBox = transactions.iterator.flatMap { tx =>
      tx.outputs.iterator.map(box => IdUtils.encodedBoxId(box.id) -> tx.id)
    }.toMap
    val remaining = mutable.Map.empty[ModifierId, Int]
    val children = mutable.Map.empty[ModifierId, mutable.ArrayBuffer[ModifierId]]
    val ready = mutable.Queue.empty[ModifierId]
    transactions.foreach { tx =>
      val parents = tx.inputs.flatMap(input =>
        producerByBox.get(IdUtils.encodedBoxId(input.boxId))).toSet
      remaining(tx.id) = parents.size
      if (parents.isEmpty) ready.enqueue(tx.id)
      parents.foreach { parent =>
        children.getOrElseUpdate(parent, mutable.ArrayBuffer.empty[ModifierId]) += tx.id
      }
    }
    var registry = OffChainRegistry.init(state.registry)
    var processed = 0
    while (ready.nonEmpty) {
      val id = ready.dequeue()
      val tx = byId(id)
      val boxes = WalletScanLogic.extractWalletOutputs(tx, None,
        state.walletVars, settings.walletSettings.dustLimit)
      registry = registry.updateOnTransaction(boxes,
        WalletScanLogic.extractInputBoxes(tx), state.walletVars.externalScans)
      processed += 1
      children.get(id).foreach(_.foreach { child =>
        val count = remaining(child) - 1
        remaining(child) = count
        if (count == 0) ready.enqueue(child)
      })
    }
    require(processed == transactions.size, "Wallet rescan mempool snapshot has cyclic dependencies")
    state.copy(offChainRegistry = registry)
  }

  private def rebuildRescanOffChain(state: ErgoWalletState,
                                   pool: ErgoMemPoolReader): Try[ErgoWalletState] =
    Try(pool.getAll.map(_.transaction)).flatMap(rebuildRescanOffChain(state, _))

  protected[wallet] def deferRescanOffChain(state: ErgoWalletState,
                                            tx: ErgoTransaction): Unit = {
    if (nodeViewHolderRef.isEmpty && !deferredRescanOffChainOverflowed &&
        !deferredRescanOffChain.exists(_.id == tx.id)) {
      if (deferredRescanOffChain.size >= settings.nodeSettings.mempoolCapacity) {
        deferredRescanOffChainOverflowed = true
        stopIncompleteRescanRecovery(state, "deferred off-chain transaction capacity exceeded")
      } else deferredRescanOffChain :+= tx
    }
  }

  protected[wallet] def prepareRescanOffChainForCompletion(
      state: ErgoWalletState): Try[ErgoWalletState] = {
    if (nodeViewHolderRef.nonEmpty) Success(state)
    else if (deferredRescanOffChainOverflowed)
      Failure(new IllegalStateException("deferred off-chain transaction capacity exceeded"))
    else Try {
      val fromReader = state.mempoolReaderOpt.toSeq.flatMap(_.getAll.map(_.transaction))
      val known = fromReader.map(_.id).toSet
      (fromReader ++ deferredRescanOffChain.filterNot(tx => known.contains(tx.id)))
        .filterNot(tx => state.registry.getTx(tx.id).isDefined)
    }.flatMap(rebuildRescanOffChain(state, _))
  }

  private def requestRescanCurrentView(tip: ModifierId): Unit = nodeViewHolderRef.foreach { holder =>
    rescanSnapshotEpoch += 1
    val epoch = rescanSnapshotEpoch
    awaitingRescanSnapshot = true
    // The callback executes in the holder mailbox after its current view is
    // installed. Materialize the context there, and never throw in that actor.
    holder.tell(GetDataFromCurrentView[ErgoState[_], RescanCurrentView] { view =>
      RescanCurrentView(epoch, tip, Try {
        val reader = view.state.getReader
        val applied = view.history.ifHolderAppliedFullTip(tip)(true).contains(true)
        (reader, reader.stateContext, view.pool.getReader, applied)
      })
    }, self)
  }

  protected[wallet] def acceptRescanCurrentView(state: ErgoWalletState,
                                                snapshot: RescanCurrentView): Unit = {
    if (rescanRecoveryActive && awaitingRescanSnapshot &&
        snapshot.epoch == rescanSnapshotEpoch &&
        pendingRescanCompletion.exists(_._1 == snapshot.tip)) {
      awaitingRescanSnapshot = false
      snapshot.readers match {
        case Failure(t) => stopIncompleteRescanRecovery(state,
          s"current node view could not be captured: ${t.getMessage}")
        case Success((_, _, _, false)) => continueRescanAfterTipChange(state)
        case Success((reader, current, pool, true)) =>
          captureRescanStateReader(state, reader, current)
            .flatMap(captureRescanMempoolReader(_, pool))
            .flatMap(rebuildRescanOffChain(_, pool)) match {
            case Failure(t) => stopIncompleteRescanRecovery(state,
              s"current node view could not be installed: ${t.getMessage}")
            case Success(updated) =>
              context.become(quarantinedWallet(updated,
                new IllegalStateException("Wallet rescan recovery is awaiting completion")))
              completePendingRescanRecovery(updated)
          }
      }
    }
  }

  private def scheduleRescanCompletion(state: ErgoWalletState,
                                       tip: ModifierId,
                                       height: Int): Unit = {
    pendingRescanCompletion = Some(tip -> height)
    context.become(quarantinedWallet(state,
      new IllegalStateException("Wallet rescan recovery is awaiting completion")))
    pendingChainMessages = 0
    unstashAll()
    if (nodeViewHolderRef.nonEmpty) requestRescanCurrentView(tip)
    else self ! CompleteRescanRecovery
  }

  protected[wallet] def rescanCompletionIsPending: Boolean = pendingRescanCompletion.nonEmpty

  protected[wallet] def retryRescanCompletion(): Unit =
    if (rescanRecoveryActive && rescanCompletionIsPending && !awaitingRescanSnapshot)
      self ! CompleteRescanRecovery

  private def selectedStateContext(state: ErgoWalletState,
                                   tip: ModifierId,
                                   height: Int): Try[Option[ErgoStateContext]] = Try {
    state.stateReaderOpt.flatMap { reader =>
      val current = reader.stateContext
      if (org.ergoplatform.core.versionToId(reader.version) == tip &&
          current.currentHeight == height &&
          current.lastHeaderOpt.exists(_.id == tip)) Some(current)
      else None
    }
  }

  /** Retain the intent while proving that the committed checkpoint is still
    * on the selected applied chain; never replay the already committed prefix.
    */
  private def continueRescanAfterTipChange(state: ErgoWalletState): Unit = {
    pendingRescanCompletion = None
    awaitingRescanSnapshot = false
    registryCheckpoint(state) match {
      case Failure(t) => stopIncompleteRescanRecovery(state,
        s"replayed wallet checkpoint is inconsistent: ${t.getMessage}")
      case Success((checkpoint, checkpointHeight)) =>
        val selectedAppliedHeight = historyReader.bestFullBlockIdOpt.flatMap { selected =>
          historyReader.ifHolderAppliedFullTip(selected)(historyReader.heightOf(selected)).flatten
        }
        if (selectedAppliedHeight.exists(_ < checkpointHeight)) {
          stopIncompleteRescanRecovery(state,
            "selected applied full tip is below the replayed wallet checkpoint")
        } else beginFullChainProbe(state, checkpoint, checkpointHeight, requireBodies = true)(
          selectedTip => historyReader.heightOf(selectedTip) match {
            case Some(height) if height == checkpointHeight && selectedTip == checkpoint =>
              scheduleRescanCompletion(state, selectedTip, height)
            case Some(height) if height > checkpointHeight &&
                armSelectedCatchUpScan(selectedTip, checkpointHeight + 1) =>
              context.become(quarantinedWallet(state,
                new IllegalStateException("Wallet rescan recovery is scanning the selected suffix")))
              pendingChainMessages = 0
              unstashAll()
            case _ => stopIncompleteRescanRecovery(state,
              "selected suffix could not be scheduled after wallet replay")
          },
          _ => stopIncompleteRescanRecovery(state,
            "replayed wallet checkpoint is off the selected full chain"),
          (_, missingHeight) => stopIncompleteRescanRecovery(state,
            s"selected full block body is missing at height $missingHeight")
        )
    }
  }

  protected[wallet] def completePendingRescanRecovery(state: ErgoWalletState): Unit =
    if (!awaitingRescanSnapshot) pendingRescanCompletion.foreach { case (tip, height) =>
      if (!historyReader.ifHolderAppliedFullTip(tip)(true).contains(true)) {
        continueRescanAfterTipChange(state)
      } else selectedStateContext(state, tip, height) match {
        case Failure(t) => stopIncompleteRescanRecovery(state,
          s"selected state context could not be read: ${t.getMessage}")
        case Success(None) =>
          // A later holder snapshot or ChangedState retries completion. The
          // durable intent remains while the reader is stale or unavailable.
          context.become(quarantinedWallet(state,
            new IllegalStateException("Wallet rescan recovery is awaiting selected state context")))
        case Success(Some(currentContext)) =>
          // Registry main and undo writes may be asynchronous. Sync them before
          // clearing either marker. The signing context needs the same fence.
          val prepared = state.storage.syncStateContext(currentContext)
            .flatMap(_ => syncRescanRegistryCheckpoint(state, tip, height))
          prepared match {
            case Failure(t) => stopIncompleteRescanRecovery(state,
              s"replayed wallet context or checkpoint could not be synced: ${t.getMessage}")
            case Success(_) =>
              // The intent is the last fence. Keep the exact selected applied
              // tip stable while clearing both small durable markers.
              Try(historyReader.ifHolderAppliedFullTip(tip) {
                selectedStateContext(state, tip, height).flatMap {
                  case Some(rechecked) if state.stateContext.bytes.sameElements(rechecked.bytes) =>
                    registryCheckpoint(state).flatMap {
                      case (committed, committedHeight) if committed == tip && committedHeight == height =>
                        state.storage.clearDeepForkQuarantine()
                          .flatMap(_ => state.storage.clearRescanRecovery())
                      case _ => Failure(new IllegalStateException(
                        "Rebuilt wallet registry did not commit the selected full tip"))
                    }
                  case _ => Failure(new IllegalStateException(
                    "Selected state context changed before wallet rescan completion"))
                }
              }) match {
                case Failure(t) => stopIncompleteRescanRecovery(state,
                  s"wallet rescan completion failed: ${t.getMessage}")
                case Success(None) => continueRescanAfterTipChange(state)
                case Success(Some(Failure(t))) => stopIncompleteRescanRecovery(state,
                  s"wallet rescan markers could not be cleared: ${t.getMessage}")
                case Success(Some(Success(_))) =>
                  rescanRecoveryActive = false
                  pendingRescanCompletion = None
                  awaitingRescanSnapshot = false
                  deferredRescanOffChain = Vector.empty
                  cancelSelectedCatchUpScan()
                  activateWallet(state.copy(rescanInProgress = false, error = None))
              }
          }
      }
    }

  private def selectedScanBatch(plan: SelectedScanPlan): Option[Vector[(Int, ModifierId)]] = {
    if (plan.anchorIndex >= plan.anchors.size) return None
    val (upperId, upperHeight) = plan.anchors(plan.anchorIndex)
    val lowerHeight = if (plan.anchorIndex == 0) plan.startHeight
      else math.max(plan.startHeight, plan.anchors(plan.anchorIndex - 1)._2 + 1)
    if (lowerHeight > upperHeight || upperHeight - lowerHeight >= 128) return None
    val descending = Vector.newBuilder[(Int, ModifierId)]
    var nextId = upperId
    var nextHeight = upperHeight
    while (nextHeight >= lowerHeight) {
      historyReader.typedModifierById[Header](nextId) match {
        case Some(header) if header.height == nextHeight =>
          descending += nextHeight -> header.id
          nextId = header.parentId
          nextHeight -= 1
        case _ => return None
      }
    }
    if (plan.anchorIndex > 0 && lowerHeight == plan.anchors(plan.anchorIndex - 1)._2 + 1 &&
      nextId != plan.anchors(plan.anchorIndex - 1)._1) None
    else Some(descending.result().reverse)
  }

  /** Keep one history lock for at most one fixed-size ancestor batch. */
  protected[wallet] def beginFullChainProbe(state: ErgoWalletState,
                                  targetId: ModifierId,
                                  targetHeight: Int,
                                  requireBodies: Boolean = false)
                                 (onSelected: ModifierId => Unit,
                                  onOther: ModifierId => Unit,
                                  onMissing: (ModifierId, Int) => Unit = (_, _) => ()): Unit = {
    cancelSelectedCatchUpScan()
    context.become(provingFullChain(state, targetId, targetHeight,
      None, onSelected, onOther, requireBodies, onMissing, Vector.empty))
    self ! ContinueFullChainProbe
  }

  protected[wallet] def scheduleFullChainProbeRetry(): Unit = {
    if (!probeRetryScheduled) {
      probeRetryScheduled = true
      context.system.scheduler.scheduleOnce(2.seconds, self, RetryFullChainProbe)(
        context.dispatcher, self
      )
    }
  }

  /** A later holder rollback supersedes a branch-point proof not yet committed. */
  protected[wallet] def startSupersedingRollback(state: ErgoWalletState): Boolean =
    if (retainedRollbackSourceHeight.nonEmpty) false
    else supersedingRollback match {
      case None => false
      case Some(version) =>
        supersedingRollback = None
        if (state.registry.hasVersion(version)) verifyRetainedRollback(state, version)
        else verifyMissingRollback(state, version)
        true
    }

  private def provingFullChain(state: ErgoWalletState,
                               targetId: ModifierId,
                               targetHeight: Int,
                               cursor: Option[FullChainCursor],
                               onSelected: ModifierId => Unit,
                               onOther: ModifierId => Unit,
                               requireBodies: Boolean,
                               onMissing: (ModifierId, Int) => Unit,
                               anchors: Vector[FullChainCursor]): Receive = {
    case RetryFullChainProbe =>
      probeRetryScheduled = false
      self ! ContinueFullChainProbe
    case ContinueFullChainProbe =>
      Try(if (requireBodies) probeSelectedFullChainBodies(targetId, targetHeight, cursor)
      else probeSelectedFullChain(targetId, targetHeight, cursor)) match {
        case Success(FullChainSelected(tip)) if historyReader.bestFullBlockIdOpt.contains(tip) =>
          completedBodyProbeAnchors = if (requireBodies) anchors else Vector.empty
          if (!startSupersedingRollback(state)) onSelected(tip)
        case Success(FullChainOther(tip)) if historyReader.bestFullBlockIdOpt.contains(tip) =>
          completedBodyProbeAnchors = if (requireBodies) anchors else Vector.empty
          if (!startSupersedingRollback(state)) onOther(tip)
        case Success(FullChainBodyMissing(tip, height)) =>
          if (!startSupersedingRollback(state)) onMissing(tip, height)
        case Success(FullChainPending(next)) =>
          context.become(provingFullChain(state, targetId, targetHeight,
            Some(next), onSelected, onOther, requireBodies, onMissing,
            if (requireBodies) anchors.filter(_.fullTipId == next.fullTipId) :+ next else anchors))
          self ! ContinueFullChainProbe
        case Success(FullChainUnknown) =>
          // A selected tip shorter than the checkpoint cannot answer this
          // ancestry probe. An actual holder rollback can still resolve it.
          if (!startSupersedingRollback(state)) {
            context.become(provingFullChain(state, targetId, targetHeight,
              None, onSelected, onOther, requireBodies, onMissing, Vector.empty))
            scheduleFullChainProbeRetry()
          }
        case Failure(t) =>
          log.warn("Selected full-chain proof is temporarily unavailable", t)
          scheduleFullChainProbeRetry()
        case _ =>
          context.become(provingFullChain(state, targetId, targetHeight,
            None, onSelected, onOther, requireBodies, onMissing, Vector.empty))
          self ! ContinueFullChainProbe
      }
    case _: ChangedHistory =>
      // A header-only update does not invalidate ancestry below the same full tip.
      val retainedCursor = cursor.filter { current =>
        Try(historyReader.bestFullBlockIdOpt.contains(current.fullTipId)).getOrElse(false)
      }
      context.become(provingFullChain(state, targetId, targetHeight,
        retainedCursor, onSelected, onOther, requireBodies, onMissing,
        if (retainedCursor.isDefined) anchors else Vector.empty))
      self ! ContinueFullChainProbe
    case Rollback(version) if rescanRecoveryActive =>
      stopIncompleteRescanRecovery(state,
        s"holder rollback to $version interrupted wallet rescan recovery")
    case Rollback(version) =>
      supersedingRollback = Some(version)
    case _: ScanSelectedInThePast => () // a stale queued scan cannot bypass the new proof
    case _: RescanWallet if rescanRecoveryActive =>
      sender() ! Failure(RescanStartConflict("Wallet rescan recovery is already in progress"))
    case RescanWallet(fromHeight) =>
      // This receive also serves startup and quarantine probes. Until the
      // checkpoint is proven, a fresh suffix has no trusted prefix to retain.
      beginQuarantinedRescan(state, fromHeight, sender())
    case _: ChangedState | _: ChangedMempool | _: ScanOffChain | _: ScanOnChain |
         _: ScanInThePast =>
      deferChainUpdate(state)
    case msg =>
      quarantinedWallet(state,
        new IllegalStateException("Wallet is waiting for selected full-chain proof"))(msg)
  }

  private def deferChainUpdate(state: ErgoWalletState): Unit = {
    if (pendingChainMessages < 256) {
      pendingChainMessages += 1
      stash()
    } else {
      log.error("Wallet full-chain proof has deferred too many chain updates")
      context.become(quarantinedWallet(state,
        new IllegalStateException("Wallet full-chain proof could not keep up with chain updates")))
      pendingChainMessages = 0
      unstashAll()
      ErgoApp.shutdownSystem()(context.system)
    }
  }

  /** A rollback target can be superseded before its ancestry proof completes.
    * Keep the wallet inaccessible without writing an irreversible fork marker;
    * the holder's next rollback supplies the current branch point.
    */
  protected[wallet] def awaitSupersedingRollback(state: ErgoWalletState, detail: String): Unit = {
    cancelSelectedCatchUpScan()
    log.warn(detail)
    val reason = new IllegalStateException(detail)
    context.become(waitingForSelectedRollback(state, reason))
  }

  private def waitingForSelectedRollback(state: ErgoWalletState, reason: Throwable): Receive = {
    case Rollback(version) =>
      if (state.registry.hasVersion(version)) verifyRetainedRollback(state, version)
      else verifyMissingRollback(state, version)
    case _: ScanSelectedInThePast => ()
    case _: ChangedState | _: ChangedMempool | _: ScanOffChain | _: ScanOnChain |
         _: ScanInThePast => deferChainUpdate(state)
    case _: ChangedHistory => ()
    case msg => quarantinedWallet(state, reason)(msg)
  }

  protected[wallet] def awaitRollbackHeader(state: ErgoWalletState,
                                  version: VersionTag,
                                  retained: Boolean): Receive = {
    case RetryFullChainProbe =>
      probeRetryScheduled = false
      self ! ContinueFullChainProbe
    case ContinueFullChainProbe =>
      val branchPoint = org.ergoplatform.core.versionToId(version)
      if (historyReader.heightOf(branchPoint).nonEmpty) {
        if (retained) verifyRetainedRollback(state, version)
        else verifyMissingRollback(state, version)
      } else {
        scheduleFullChainProbeRetry()
      }
    case _: ChangedHistory => self ! ContinueFullChainProbe
    case Rollback(version) =>
      supersedingRollback = Some(version)
      startSupersedingRollback(state)
    case _: ScanSelectedInThePast => ()
    case _: ChangedState | _: ChangedMempool | _: ScanOffChain | _: ScanOnChain |
         _: ScanInThePast => deferChainUpdate(state)
    case msg =>
      quarantinedWallet(state,
        new IllegalStateException("Wallet is waiting for rollback-header authority"))(msg)
  }

  protected[wallet] def activeWallet(state: ErgoWalletState): Receive = {
    case _: ChangedHistory | ContinueFullChainProbe | RetryFullChainProbe => ()
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
        case Failure(t) =>
          val errorMsg = s"Updating wallet state context failed : ${t.getMessage}"
          log.error(errorMsg, t)
          context.become(loadedWallet(state.copy(error = Some(errorMsg))))
      }

    /* SCAN COMMANDS */
    //scan mempool transaction
    case ScanOffChain(tx) =>
      val dustLimit = settings.walletSettings.dustLimit
      val newWalletBoxes = WalletScanLogic.extractWalletOutputs(tx, None, state.walletVars, dustLimit)
      val inputs = WalletScanLogic.extractInputBoxes(tx)
      val newState = state.copy(offChainRegistry =
        state.offChainRegistry.updateOnTransaction(newWalletBoxes, inputs, state.walletVars.externalScans)
      )
      context.become(loadedWallet(newState))

    case ScanSelectedInThePast(blockHeight, epoch) =>
      selectedScanPlan match {
        case Some(plan) if plan.epoch == epoch && plan.nextHeight == blockHeight =>
          def failScan(detail: String): Unit = {
            if (rescanRecoveryActive) stopIncompleteRescanRecovery(state, detail)
            else {
              cancelSelectedCatchUpScan()
              // The block batch may have committed before scanBlockUpdate failed.
              // Persist the recovery intent first so a crash cannot clear this fence at startup.
              val persisted = state.storage.beginRescanRecovery()
                .flatMap(_ => persistDeepForkQuarantine(state))
              finishDeepForkQuarantine(state, detail, persisted)
            }
          }

          def reproveAfterTipChange(nextState: ErgoWalletState, startHeight: Int): Unit = {
            if (rescanRecoveryActive) continueRescanAfterTipChange(nextState)
            else {
              cancelSelectedCatchUpScan()
              beginCatchUpBodyPreflight(nextState, startHeight)
            }
          }

          // Hold the history lock only while resolving one selected block and
          // its bounded ancestry batch. Wallet scanning and LevelDB writes run outside it.
          val captured: Try[Option[Either[String, (ErgoFullBlock, SelectedScanPlan)]]] = Try {
            historyReader.ifHolderAppliedFullTip(plan.tip) {
              val batchOpt = if (plan.batch.nonEmpty) Some(plan.batch) else selectedScanBatch(plan)
              batchOpt match {
                case Some(batch) if batch.headOption.exists(_._1 == blockHeight) =>
                  val blockId = batch.head._2
                  historyReader.typedModifierById[Header](blockId)
                    .filter(_.height == blockHeight)
                    .flatMap(historyReader.getFullBlock) match {
                    case None => Left(s"selected full block body is missing at height $blockHeight")
                    case Some(block) =>
                      val remaining = batch.tail
                      val next = plan.copy(nextHeight = blockHeight + 1, batch = remaining,
                        anchorIndex = if (remaining.isEmpty) plan.anchorIndex + 1 else plan.anchorIndex)
                      Right((block, next))
                  }
                case _ => Left(s"selected full-chain ancestry is unavailable at height $blockHeight")
              }
            }
          }
          captured match {
            case Failure(t) => failScan(s"selected full-chain block lookup failed at height $blockHeight: ${t.getMessage}")
            case Success(None) => reproveAfterTipChange(state, blockHeight)
            case Success(Some(Left(detail))) => failScan(detail)
            case Success(Some(Right((block, next)))) =>
              Try(ergoWalletService.scanBlockUpdate(state, block,
                settings.walletSettings.dustLimit)).flatten match {
                case Failure(t) =>
                  failScan(s"selected full block ${block.id} scan failed at height $blockHeight: ${t.getMessage}")
                case Success(updatedState) =>
                  if (rescanRecoveryActive && blockHeight == plan.tipHeight) {
                    scheduleRescanCompletion(updatedState, plan.tip, plan.tipHeight)
                  } else {
                    val installed = Try(historyReader.ifHolderAppliedFullTip(plan.tip) {
                      if (blockHeight < plan.tipHeight) {
                        selectedScanPlan = Some(next)
                        context.become(loadedWallet(updatedState))
                        self ! ScanSelectedInThePast(next.nextHeight, epoch)
                      } else {
                        cancelSelectedCatchUpScan()
                        activateWallet(updatedState)
                      }
                    })
                    installed match {
                      case Failure(t) => failScan(s"selected full-chain postscan check failed: ${t.getMessage}")
                      case Success(None) => reproveAfterTipChange(updatedState, blockHeight + 1)
                      case Success(Some(_)) => ()
                    }
                  }
              }
          }
        case _ => () // superseded queued scan
      }

    // A queued plain catch-up request must establish a fresh selected plan.
    case ScanInThePast(_, false) if selectedScanPlan.nonEmpty =>
      () // a queued legacy catch-up message cannot bypass the selected scan plan
    case ScanInThePast(blockHeight, false) =>
      val nextBlockHeight = state.expectedNextBlockHeight(blockHeight, settings.nodeSettings.isFullBlocksPruned)
      if (nextBlockHeight == blockHeight) beginCatchUpBodyPreflight(state, blockHeight)

    // No producer remains for this legacy message; explicit rescans use the
    // durable selected-chain plan and must not enter the old best-header loop.
    case ScanInThePast(_, true) => ()

    //scan block transactions
    case _: ScanOnChain if selectedScanPlan.nonEmpty =>
      deferChainUpdate(state)
    case ScanOnChain(newBlock) =>
      if (state.secretIsSet(settings.walletSettings.testMnemonic)) { // scan blocks only if wallet is initialized
        val nextBlockHeight = state.expectedNextBlockHeight(newBlock.height, settings.nodeSettings.isFullBlocksPruned)
        if (nextBlockHeight == newBlock.height) {
          log.info(s"Wallet is going to scan a block ${newBlock.id} on chain at height ${newBlock.height}")
          def failDirectScan(ex: Throwable): Unit = {
            val detail = s"scanning new block ${newBlock.id} on chain at height ${newBlock.height} failed: ${ex.getMessage}"
            log.error(detail, ex)
            // A failed or inconsistent scan may have committed its registry batch.
            // Keep the intent ahead of the quarantine marker across a crash.
            val persisted = state.storage.beginRescanRecovery()
              .flatMap(_ => persistDeepForkQuarantine(state))
            finishDeepForkQuarantine(state, detail, persisted)
          }
          Try(ergoWalletService.scanBlockUpdate(state, newBlock, settings.walletSettings.dustLimit)).flatten match {
            case Failure(ex) => failDirectScan(ex)
            case Success(updatedState) =>
              registryCheckpoint(updatedState) match {
                case Success((tip, height)) if tip == newBlock.id && height == newBlock.height =>
                  // The holder may have selected another full tip while the wallet
                  // committed this block. Install only under a short history fence.
                  Try(historyReader.ifHolderAppliedFullTip(newBlock.id) {
                    activateWallet(updatedState)
                  }) match {
                    case Success(Some(_)) => ()
                    case Success(None) =>
                      // Prove whether the committed block remains an ancestor and
                      // scan the newly selected suffix, or await the holder rollback.
                      beginCatchUpBodyPreflight(updatedState, newBlock.height + 1)
                    case Failure(ex) => failDirectScan(ex)
                  }
                case Success((tip, height)) =>
                  failDirectScan(new IllegalStateException(
                    s"registry committed $tip at height $height instead of ${newBlock.id} at ${newBlock.height}"))
                case Failure(ex) => failDirectScan(ex)
              }
          }
        } else if (nextBlockHeight < newBlock.height) {
          log.warn(s"Wallet: skipped blocks found starting from $nextBlockHeight, going back to scan them")
          beginCatchUpBodyPreflight(state, nextBlockHeight)
        } else {
          log.warn(s"Wallet: block in the past reported at ${newBlock.height}, blockId: ${newBlock.id}")
        }
      }

    case Rollback(version: VersionTag) =>
      cancelSelectedCatchUpScan()
      if (!state.registry.hasVersion(version)) {
        verifyMissingRollback(state, version)
      } else {
        verifyRetainedRollback(state, version)
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

    // A rebuilt registry has no trusted prefix. Prove and replay the selected
    // suffix requested by the caller, with durable intent before deletion.
    case RescanWallet(fromHeight) =>
      if (state.rescanInProgress) {
        log.info(s"Skipping rescan request from height: $fromHeight as one is already in progress")
        sender() ! Failure(RescanStartConflict("Rescan already in progress"))
      } else {
        beginQuarantinedRescan(state, fromHeight, sender(), allowFreshSuffix = true)
      }

    case GetWalletStatus =>
      val isSecretSet = state.secretIsSet(settings.walletSettings.testMnemonic)
      val isUnlocked = state.walletVars.proverOpt.isDefined
      val changeAddress = state.getChangeAddress(ergoAddressEncoder)
      val height = state.getWalletHeight
      val lastError = state.error
      val status = WalletStatus(isSecretSet, isUnlocked, changeAddress, height, lastError,
        WalletRescanState.Inactive)
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

    case DeriveKey(encodedPath) =>
      ergoWalletService.deriveKeyFromPath(state, encodedPath, ergoAddressEncoder) match {
        case Success((p2pkAddress, newState)) =>
          context.become(loadedWallet(newState))
          sender() ! Success(p2pkAddress)
        case f@Failure(_) =>
          sender() ! f
      }

    case DeriveNextKey =>
      ergoWalletService.deriveNextKey(state, settings.walletSettings.usePreEip3Derivation) match {
        case Success((derivationResult, newState)) =>
          context.become(loadedWallet(newState))
          sender() ! derivationResult
        case Failure(t) =>
          sender() ! DeriveNextKeyResult(Failure(t))
      }

    case UpdateChangeAddress(address) =>
      ergoWalletService.updateChangeAddress(state, address) match {
        case Success(_) =>
          sender() ! StatusReply.success(())
        case Failure(t: ChangeAddressValidationException) =>
          log.warn(t.getMessage)
          sender() ! StatusReply.error(t)
        case Failure(t) =>
          log.error(s"Unable to update change address", t)
          sender() ! StatusReply.error(t)
      }

    case RemoveScan(scanId) =>
      ergoWalletService.removeScan(state, scanId) match {
        case Success(newState) =>
          context.become(loadedWallet(newState))
          sender() ! RemoveScanResponse(Success(()))
        case Failure(t) =>
          log.warn(s"Unable to remove scanId: $scanId", t)
          sender() ! RemoveScanResponse(Failure(t))
      }

    case AddScan(appRequest) =>
      ergoWalletService.addScan(state, appRequest) match {
        case Success((scan, newState)) =>
          context.become(loadedWallet(newState))
          sender() ! AddScanResponse(Success(scan))
        case Failure(t) =>
          log.warn(s"Unable to add scan: $appRequest", t)
          sender() ! AddScanResponse(Failure(t))
      }

    case AddBox(box: ErgoBox, scanIds: Set[ScanId]) =>
      state.registry.updateScans(scanIds, box)
      sender() ! AddBoxResponse(Success(())) // todo: what is the reasoning behind returning always success?

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

  private[wallet] final case class RescanCurrentView(
      epoch: Long, tip: ModifierId,
      readers: Try[(ErgoStateReader, ErgoStateContext, ErgoMemPoolReader, Boolean)])

  /** Start actor and register its proper closing into coordinated shutdown */
  def apply(settings: ErgoSettings,
            parameters: Parameters,
            service: ErgoWalletService,
            boxSelector: BoxSelector,
            historyReader: ErgoHistoryReader,
            nodeViewHolderRef: Option[ActorRef] = None)(implicit actorSystem: ActorSystem): ActorRef = {
    val props = Props(classOf[ErgoWalletActor], settings, parameters, service, boxSelector,
      historyReader, nodeViewHolderRef)
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
