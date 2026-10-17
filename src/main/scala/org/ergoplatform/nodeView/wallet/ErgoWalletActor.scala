package org.ergoplatform.nodeView.wallet

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor._
import akka.pattern.StatusReply
import org.ergoplatform.ErgoBox._
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedMempool, ChangedState}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.{CurrentView, ReceivableMessages}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.GenesisHeight
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContextSerializer, ErgoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletService.ChangeAddressValidationException
import org.ergoplatform.nodeView.wallet.ErgoWalletServiceUtils.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.persistence.{Balance, OffChainRegistry}
import org.ergoplatform.nodeView.wallet.IdUtils.EncodedBoxId
import org.ergoplatform.sdk.wallet.secrets.DerivationPath
import org.ergoplatform.settings._
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.BoxSelector
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform._
import org.ergoplatform.core.{VersionTag, versionToId}
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.utils.ScorexEncoding
import scorex.util.ScorexLogging
import scorex.util.ModifierId

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class ErgoWalletActor(settings: ErgoSettings,
                      parameters: Parameters,
                      ergoWalletService: ErgoWalletService,
                      boxSelector: BoxSelector,
                      historyReader: ErgoHistoryReader,
                      nodeViewHolderRef: Option[ActorRef])
  extends Actor with Stash with ScorexLogging with ScorexEncoding {

  def this(settings: ErgoSettings, parameters: Parameters, ergoWalletService: ErgoWalletService,
           boxSelector: BoxSelector, historyReader: ErgoHistoryReader) =
    this(settings, parameters, ergoWalletService, boxSelector, historyReader, None)

  import ErgoWalletActor._

  private val ergoAddressEncoder: ErgoAddressEncoder = settings.addressEncoder

  private var initializationOutcomeUnknown: Option[Throwable] = None

  private case class ReplayRescan(height: Int)

  private sealed trait RescanPhase
  private case object AwaitingInitialTip extends RescanPhase
  private case object Replaying extends RescanPhase
  private case object AwaitingFinalTip extends RescanPhase
  private var nextSnapshotId = 0L

  private def requestAppliedTip(): Long = {
    nextSnapshotId += 1
    val requestId = nextSnapshotId
    nodeViewHolderRef match {
      case Some(holder) =>
        holder.tell(ReceivableMessages.GetDataFromCurrentView[ErgoState[_], AppliedTipReply] { view =>
          AppliedTipReply(requestId, readAppliedTip(view))
        }, self)
      case None =>
        self ! AppliedTipReply(requestId,
          Failure(new IllegalStateException("Node view holder is unavailable for rescan")))
    }
    context.system.scheduler.scheduleOnce(5.seconds, self, AppliedTipTimeout(requestId))(context.dispatcher)
    requestId
  }

  protected[wallet] def selectedBlockAt(height: Int): Option[ErgoFullBlock] =
    historyReader.bestFullBlockAt(height)

  private def rescanRead(message: Any): Boolean = message match {
    case _: ReadBalances | _: ReadPublicKeys | _: ReadExtendedPublicKeys |
         _: GetPrivateKeyFromPath | GetMiningPubKey | GetFirstSecret |
         _: GetWalletBoxes | _: GetScanUnspentBoxes | _: GetScanSpentBoxes |
         GetTransactions | _: GetTransaction | ReadScans | _: CheckSeed |
         GetWalletStatus | _: CollectWalletBoxes | _: GetScanTransactions |
         _: GetFilteredScanTxs | _: ExtractHints => true
    case _ => false
  }

  private def stagedRescan(active: ErgoWalletState, candidate: ErgoWalletState,
                           fromHeight: Int, targetHeight: Int, lastBlockId: Option[ModifierId],
                           stagedSpentIds: Set[EncodedBoxId] = Set.empty,
                           phase: RescanPhase = Replaying, snapshotId: Long = 0L): Receive = {
    case AppliedTipReply(id, result) if id == snapshotId && phase == AwaitingInitialTip =>
      result.flatMap { tip =>
        Try {
          require(tip.height < GenesisHeight ||
            tip.blockId.exists(id => selectedBlockAt(tip.height).exists(_.id == id)),
            "Rescan target does not match the applied state tip")
          val stateContext = ErgoStateContextSerializer(settings.chainSettings)
            .parseBytesTry(tip.contextBytes.toArray).get
          require(stateContext.currentHeight == tip.height &&
            stateContext.lastHeaderOpt.map(_.id) == tip.blockId,
            "Rescan snapshot context does not match the applied state tip")
          active.storage.updateStateContext(stateContext).get
          if (candidate.storage ne active.storage) candidate.storage.updateStateContext(stateContext).get
          val currentParameters = stateContext.currentParameters
          val nextActive = active.copy(parameters = currentParameters,
            walletVars = active.walletVars.withParameters(currentParameters).getOrElse(active.walletVars))
          val nextCandidate = candidate.copy(parameters = currentParameters,
            walletVars = candidate.walletVars.withParameters(currentParameters).getOrElse(candidate.walletVars))
          (tip.height, nextActive, nextCandidate)
        }
      } match {
        case Failure(error) => failStagedRescan(active, candidate, error)
        case Success((appliedHeight, nextActive, nextCandidate)) =>
          context.become(stagedRescan(nextActive, nextCandidate, fromHeight, appliedHeight,
            lastBlockId, stagedSpentIds))
          self ! ReplayRescan(Math.max(GenesisHeight, Math.min(fromHeight, appliedHeight)))
      }

    case AppliedTipReply(id, result) if id == snapshotId && phase == AwaitingFinalTip =>
      result.flatMap { tip =>
        Try {
          require(tip.height == targetHeight && tip.blockId == lastBlockId &&
            (targetHeight < GenesisHeight || lastBlockId.exists(id => selectedBlockAt(targetHeight).exists(_.id == id))) &&
            candidate.getWalletHeight == targetHeight,
            "Rescan replay does not match the applied state tip")
        }
      } match {
        case Failure(error) => failStagedRescan(active, candidate, error)
        case Success(_) =>
          Try(rebaseOffChain(active, candidate, stagedSpentIds)) match {
            case Failure(error) => failStagedRescan(active, candidate, error)
            case Success(rebased) =>
              Try(ergoWalletService.publishRegistryRescan(active, rebased, settings)).flatten match {
                case Success(selected) => context.become(loadedWallet(selected))
                case Failure(error: WalletInitialization.OutcomeUnknown) =>
                  failStopRescan(active, Some(candidate), error)
                case Failure(error) => failStagedRescan(active, candidate, error, cleanupCandidate = false)
              }
          }
      }

    case AppliedTipTimeout(id) if id == snapshotId && phase != Replaying =>
      failStagedRescan(active, candidate,
        new IllegalStateException("Rescan applied state snapshot timed out"))

    case _: AppliedTipReply | _: AppliedTipTimeout => // A previous snapshot request completed or expired.

    case ReplayRescan(height) if phase == Replaying =>
      if (height > targetHeight) {
        Try {
          val selectedAncestor = targetHeight < GenesisHeight ||
            lastBlockId.exists(id => selectedBlockAt(targetHeight).exists(_.id == id))
          require(selectedAncestor && candidate.getWalletHeight == targetHeight,
            "Rescan replay no longer matches the selected full-block ancestor")
        } match {
          case Failure(error) => failStagedRescan(active, candidate, error)
          case Success(_) =>
            val id = requestAppliedTip()
            context.become(stagedRescan(active, candidate, fromHeight, targetHeight, lastBlockId,
              stagedSpentIds, AwaitingFinalTip, id))
        }
      } else {
        Try(selectedBlockAt(height)) match {
          case Failure(error) => failStagedRescan(active, candidate, error)
          case Success(None) =>
            failStagedRescan(active, candidate,
              new IllegalStateException(s"Required rescan block at height $height is unavailable"))
          case Success(Some(block)) if lastBlockId.exists(_ != block.header.parentId) =>
            failStagedRescan(active, candidate,
              new IllegalStateException(s"Rescan block ancestry changed at height $height"))
          case Success(Some(block)) =>
            Try(ergoWalletService.scanBlockUpdate(candidate, block, settings.walletSettings.dustLimit)).flatten match {
              case Success(updated) =>
                context.become(stagedRescan(active, updated, fromHeight, targetHeight,
                  Some(block.id), stagedSpentIds))
                self ! ReplayRescan(height + 1)
              case Failure(error) => failStagedRescan(active, candidate, error)
            }
        }
      }

    case _: ReplayRescan => // Replay starts only after the initial holder snapshot.

    case RescanWallet(_) =>
      sender() ! Failure(new IllegalStateException("Rescan already in progress"))

    case LockWallet =>
      val lockedActive = ergoWalletService.lockWallet(active)
      val lockedCandidate = ergoWalletService.lockWallet(candidate)
      context.become(stagedRescan(lockedActive, lockedCandidate, fromHeight, targetHeight, lastBlockId,
        stagedSpentIds, phase, snapshotId))

    case UnlockWallet(walletPass) =>
      walletPass.erase()
      sender() ! Failure(new IllegalStateException("Wallet rescan in progress"))

    case InitWallet(walletPass, mnemonicPassOpt) =>
      walletPass.erase()
      mnemonicPassOpt.foreach(_.erase())
      sender() ! Failure(new IllegalStateException("Wallet rescan in progress"))
    case RestoreWallet(mnemonic, mnemonicPassOpt, walletPass, _) =>
      mnemonic.erase()
      mnemonicPassOpt.foreach(_.erase())
      walletPass.erase()
      sender() ! Failure(new IllegalStateException("Wallet rescan in progress"))

    case _: DeriveKey | _: GenerateTransaction | _: SignTransaction =>
      sender() ! Failure(new IllegalStateException("Wallet rescan in progress"))
    case DeriveNextKey =>
      sender() ! DeriveNextKeyResult(Failure(new IllegalStateException("Wallet rescan in progress")))
    case _: UpdateChangeAddress =>
      sender() ! StatusReply.error("Wallet rescan in progress")
    case _: RemoveScan =>
      sender() ! RemoveScanResponse(Failure(new IllegalStateException("Wallet rescan in progress")))
    case _: AddScan =>
      sender() ! AddScanResponse(Failure(new IllegalStateException("Wallet rescan in progress")))
    case _: AddBox =>
      sender() ! AddBoxResponse(Failure(new IllegalStateException("Wallet rescan in progress")))
    case _: StopTracking =>
      sender() ! StopTrackingResponse(Failure(new IllegalStateException("Wallet rescan in progress")))
    case _: GenerateCommitmentsFor =>
      sender() ! GenerateCommitmentsResponse(Failure(new IllegalStateException("Wallet rescan in progress")))

    case ChangedState(s: ErgoStateReader@unchecked) =>
      Try {
        active.storage.updateStateContext(s.stateContext).get
          val parameters = s.stateContext.currentParameters
          val nextActive = ergoWalletService.updateUtxoState(active.copy(stateReaderOpt = Some(s),
            parameters = parameters, walletVars = active.walletVars.withParameters(parameters).getOrElse(active.walletVars)))
          val nextCandidate = ergoWalletService.updateUtxoState(candidate.copy(stateReaderOpt = Some(s),
            parameters = parameters, walletVars = candidate.walletVars.withParameters(parameters).getOrElse(candidate.walletVars)))
          (nextActive, nextCandidate)
      } match {
        case Failure(error) => failStagedRescan(active, candidate, error)
        case Success((nextActive, nextCandidate)) =>
          context.become(stagedRescan(nextActive, nextCandidate, fromHeight, targetHeight, lastBlockId,
            stagedSpentIds, phase, snapshotId))
      }
    case ChangedMempool(mr: ErgoMemPoolReader@unchecked) =>
      Try {
        val nextActive = ergoWalletService.updateUtxoState(active.copy(mempoolReaderOpt = Some(mr)))
        val nextCandidate = ergoWalletService.updateUtxoState(candidate.copy(mempoolReaderOpt = Some(mr)))
        (nextActive, nextCandidate)
      } match {
        case Failure(error) => failStagedRescan(active, candidate, error)
        case Success((nextActive, nextCandidate)) =>
          context.become(stagedRescan(nextActive, nextCandidate, fromHeight, targetHeight, lastBlockId,
            stagedSpentIds, phase, snapshotId))
      }
    case ScanOffChain(tx) =>
      Try {
        val boxes = WalletScanLogic.extractWalletOutputs(tx, None, active.walletVars, settings.walletSettings.dustLimit)
        val inputs = WalletScanLogic.extractInputBoxes(tx)
        val updated = active.copy(offChainRegistry =
          active.offChainRegistry.updateOnTransaction(boxes, inputs, active.walletVars.externalScans))
        (updated, inputs)
      } match {
        case Failure(error) => failStagedRescan(active, candidate, error)
        case Success((updated, inputs)) =>
          context.become(stagedRescan(updated, candidate, fromHeight, targetHeight, lastBlockId,
            stagedSpentIds ++ inputs, phase, snapshotId))
      }
    case change @ ScanOnChain(block) if block.height <= targetHeight =>
      if (!Try(selectedBlockAt(block.height).exists(_.id == block.id)).getOrElse(false)) {
        abortStagedRescanForChainChange(active, candidate, change)
      }
    case change: ScanOnChain => abortStagedRescanForChainChange(active, candidate, change)
    case change: Rollback => abortStagedRescanForChainChange(active, candidate, change)
    case ScanInThePast(_, false) => // The candidate is already replaying the selected history.
    case ScanInThePast(_, true) => // A previous rescan signal cannot advance this candidate.

    case CloseWallet =>
      closeAndDiscardCandidate(candidate).failed.foreach(error =>
        log.warn("Could not close or discard abandoned rescan registry", error))
      active.registry.close()
      active.storage.close()
      context.stop(self)

    case message if rescanRead(message) => loadedWallet(active)(message)

    case message => unhandled(message)
  }

  private def failStagedRescan(active: ErgoWalletState, candidate: ErgoWalletState,
                               error: Throwable, cleanupCandidate: Boolean = true): Unit = {
    log.error("Wallet rescan failed", error)
    val closed = if (cleanupCandidate) closeAndDiscardCandidate(candidate)
    else Try(candidate.registry.close())
    val cleanupError = closed.failed.toOption
    cleanupError.foreach { closeError =>
      if (closeError ne error) error.addSuppressed(closeError)
      log.warn("Could not close or discard abandoned rescan registry", closeError)
    }
    val statusError = Option(error.getMessage).getOrElse(error.toString) +
      (if (cleanupError.nonEmpty || error.getSuppressed.nonEmpty)
        "; Candidate cleanup or handle closure was incomplete; see node logs" else "")
    resumeIfSelected(active, error) {
      context.become(loadedWallet(active.copy(error = Some(statusError), rescanInProgress = false)))
    }
  }

  private def resumeIfSelected(active: ErgoWalletState, priorError: Throwable)(resume: => Unit): Unit = {
    val matches = active.generation match {
      case Some(generation) => Try(WalletInitialization.matchesSelectedDescriptor(generation, settings)).flatten
      case None => Failure(new IllegalStateException("Rescan has no selected wallet generation"))
    }
    matches match {
      case Success(true) => resume
      case result =>
        val selectionError = result.failed.getOrElse(
          new IllegalStateException("Selected wallet generation changed during rescan"))
        val unknown = new WalletInitialization.OutcomeUnknown(selectionError)
        if (priorError ne selectionError) unknown.addSuppressed(priorError)
        failStopRescan(active, None, unknown)
    }
  }

  private def failStopRescan(active: ErgoWalletState, candidate: Option[ErgoWalletState],
                             error: WalletInitialization.OutcomeUnknown): Unit = {
    log.error("Wallet rescan selection is uncertain; stopping the wallet", error)
    candidate.foreach(state => Try(state.registry.close()).failed.foreach(closeError =>
      log.warn("Could not close rescan candidate registry while stopping", closeError)))
    Try(active.registry.close()).failed.foreach(closeError =>
      log.warn("Could not close active wallet registry while stopping", closeError))
    Try(active.storage.close()).failed.foreach(closeError =>
      log.warn("Could not close wallet storage while stopping", closeError))
    context.stop(self)
    ErgoApp.shutdownSystem()(context.system)
  }

  private def closeAndDiscardCandidate(candidate: ErgoWalletState): Try[Unit] =
    Try(candidate.registry.close()).flatMap { _ =>
      candidate.generation match {
        case Some(generation) =>
          Try(WalletInitialization.discardUnselectedRescanCandidate(generation, settings)).flatten
        case None => Failure(new IllegalStateException("Rescan candidate generation is unavailable"))
      }
    }

  private def rebaseOffChain(active: ErgoWalletState, candidate: ErgoWalletState,
                             stagedSpentIds: Set[EncodedBoxId]): ErgoWalletState = {
    val priorOnChain = active.registry.walletUnspentBoxes().map(Balance.apply)
    val visiblePriorIds = active.offChainRegistry.onChainBalances.map(_.id).toSet
    val pendingSpentIds = (priorOnChain.map(_.id).toSet -- visiblePriorIds) ++
      stagedSpentIds
    val certain = candidate.registry.walletUnspentBoxes()
    val certainIds = certain.map(_.boxId).toSet
    val offChain = active.offChainRegistry.offChainBoxes.filterNot(b => certainIds.contains(b.boxId))
    val onChain = certain.map(Balance.apply).filterNot(b => pendingSpentIds.contains(b.id))
    candidate.copy(offChainRegistry = OffChainRegistry(candidate.getWalletHeight, offChain, onChain))
  }

  private def abortStagedRescanForChainChange(active: ErgoWalletState,
                                              candidate: ErgoWalletState, change: Any): Unit = {
    val cleanupError = closeAndDiscardCandidate(candidate).failed.toOption
    cleanupError.foreach(error =>
      log.warn("Could not close or discard abandoned rescan registry", error))
    val errorMessage = "Rescan aborted because the selected chain changed" +
      (if (cleanupError.nonEmpty)
        "; Candidate cleanup or handle closure was incomplete; see node logs" else "")
    val restored = active.copy(error = Some(errorMessage),
      rescanInProgress = false)
    resumeIfSelected(active, new IllegalStateException(errorMessage)) {
      context.become(loadedWallet(restored))
      loadedWallet(restored)(change)
    }
  }

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
      unstashAll()
    case _ => // stashing all messages until wallet is setup
      stash()
  }

  private def loadedWallet(state: ErgoWalletState): Receive = {
    case _: AppliedTipReply | _: AppliedTipTimeout => // Ignore replies from an abandoned rescan.

    case InitWallet(walletPass, mnemonicPassOpt) if initializationOutcomeUnknown.isDefined =>
      walletPass.erase()
      mnemonicPassOpt.foreach(_.erase())
      sender() ! Failure(initializationOutcomeUnknown.get)

    case RestoreWallet(mnemonic, mnemonicPassOpt, walletPass, _) if initializationOutcomeUnknown.isDefined =>
      mnemonic.erase()
      mnemonicPassOpt.foreach(_.erase())
      walletPass.erase()
      sender() ! Failure(initializationOutcomeUnknown.get)

    case UnlockWallet(walletPass) if initializationOutcomeUnknown.isDefined =>
      walletPass.erase()
      sender() ! Failure(initializationOutcomeUnknown.get)

    case _: RescanWallet if initializationOutcomeUnknown.isDefined =>
      sender() ! Failure(initializationOutcomeUnknown.get)

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
          rememberInitializationFailure(state, t)
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
          rememberInitializationFailure(state, t)
          val f = wrapLegalExc(t) //getting nicer message for illegal key size exception
          log.error(s"Wallet restoration is failed, details: ${f.exception.getMessage}")
          sender() ! f
      }

    // branch for key already being set
    case _: RestoreWallet | _: InitWallet =>
      val reason = if (state.generation.isDefined) {
        "Wallet is already initialized; use a separate wallet data directory to initialize another wallet."
      } else {
        "Wallet is already initialized or testMnemonic is set. Clear current secret to re-init it."
      }
      sender() ! Failure(new Exception(reason))

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

    // rescan=true means we serve a user request for rescan from arbitrary height
    case ScanInThePast(blockHeight, rescan) =>
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
        if (blockHeight < newState.fullHeight) {
          self ! ScanInThePast(blockHeight + 1, rescan)
        } else if (rescan) {
          log.info(s"Rescanning finished at height $blockHeight")
          context.become(loadedWallet(newState.copy(rescanInProgress = false)))
        }
      }

    //scan block transactions
    case ScanOnChain(newBlock) =>
      if (state.secretIsSet(settings.walletSettings.testMnemonic)) { // scan blocks only if wallet is initialized
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

    // Selected generations replay in a sibling registry and switch only after verification.
    case RescanWallet(fromHeight) =>
      if (state.generation.isDefined && fromHeight < 0) {
        sender() ! Failure(new IllegalArgumentException("Rescan height must be nonnegative"))
      } else if (!state.rescanInProgress) {
        log.info(s"Rescanning the wallet from height: $fromHeight")
        ergoWalletService.recreateRegistry(state, settings) match {
          case Success(candidate) if state.generation.isDefined =>
            val targetHeight = state.fullHeight
            val snapshotId = requestAppliedTip()
            context.become(stagedRescan(state.copy(rescanInProgress = true), candidate,
              fromHeight, targetHeight, None, phase = AwaitingInitialTip, snapshotId = snapshotId))
            sender() ! Success(())
          case Success(newState) =>
            context.become(loadedWallet(newState.copy(rescanInProgress = true)))
            val heightToScanFrom = Math.min(newState.fullHeight, fromHeight)
            self ! ScanInThePast(heightToScanFrom, rescan = true)
            sender() ! Success(())
          case f@Failure(error: WalletInitialization.OutcomeUnknown) =>
            sender() ! f
            failStopRescan(state, None, error)
          case f@Failure(t) =>
            log.error("Error during rescan attempt: ", t)
            sender() ! f
        }
      } else {
        log.info(s"Skipping rescan request from height: $fromHeight as one is already in progress")
        sender() ! Failure(new IllegalStateException("Rescan already in progress"))
      }

    case GetWalletStatus =>
      val isSecretSet = state.secretIsSet(settings.walletSettings.testMnemonic)
      val isUnlocked = state.walletVars.proverOpt.isDefined
      val changeAddress = state.getChangeAddress(ergoAddressEncoder)
      val height = state.getWalletHeight
      val lastError = initializationOutcomeUnknown.map(_.getMessage).orElse(state.error)
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

  private def rememberInitializationFailure(state: ErgoWalletState, error: Throwable): Unit = error match {
    case _: WalletInitialization.OutcomeUnknown =>
      initializationOutcomeUnknown = Some(error)
      context.become(loadedWallet(state.copy(error = Some(error.getMessage))))
    case _ =>
  }

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

  private case class AppliedTip(height: Int, blockId: Option[ModifierId], contextBytes: Vector[Byte])
  private case class AppliedTipReply(requestId: Long, result: Try[AppliedTip])
  private case class AppliedTipTimeout(requestId: Long)

  // Evaluate only immutable tip facts in the holder's mailbox. A digest state can
  // advance its version on a header alone, so version equality is insufficient.
  private def readAppliedTip(view: CurrentView[ErgoState[_]]): Try[AppliedTip] = Try {
    val context = view.state.stateContext
    val height = context.currentHeight
    if (height < GenesisHeight) {
      require(height == GenesisHeight - 1 && context.lastHeaderOpt.isEmpty &&
        view.state.version == ErgoState.genesisStateVersion && view.history.bestFullBlockIdOpt.isEmpty,
        "Holder does not expose an applied genesis state")
      AppliedTip(height, None, context.bytes.toVector)
    } else {
      val header = context.lastHeaderOpt.getOrElse(
        throw new IllegalStateException("Holder state has no applied header"))
      require(header.height == height && context.lastExtensionOpt.isDefined &&
        versionToId(view.state.version) == header.id &&
        view.history.bestFullBlockIdOpt.contains(header.id) &&
        view.history.bestHeaderIdAtHeight(height).contains(header.id),
        "Holder does not expose an applied full-block tip")
      AppliedTip(height, Some(header.id), context.bytes.toVector)
    }
  }

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
