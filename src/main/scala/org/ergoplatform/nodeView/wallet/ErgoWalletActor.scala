package org.ergoplatform.nodeView.wallet

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor._
import akka.pattern.StatusReply
import org.ergoplatform.ErgoBox._
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, ChangedMempool, ChangedState}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.ErgoHistoryReader._
import org.ergoplatform.modifiers.history.header.PreGenesisHeader
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.ErgoStateReader
import org.ergoplatform.nodeView.wallet.ErgoWalletService.ChangeAddressValidationException
import org.ergoplatform.nodeView.wallet.ErgoWalletServiceUtils.DeriveNextKeyResult
import org.ergoplatform.sdk.wallet.secrets.DerivationPath
import org.ergoplatform.settings._
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.BoxSelector
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform._
import org.ergoplatform.core.VersionTag
import org.ergoplatform.nodeView.wallet.persistence.WalletDigest
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.utils.ScorexEncoding
import scorex.util.ScorexLogging
import scorex.util.ModifierId

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class ErgoWalletActor(protected val settings: ErgoSettings,
                      parameters: Parameters,
                      ergoWalletService: ErgoWalletService,
                      boxSelector: BoxSelector,
                      protected val historyReader: ErgoHistoryReader)
  extends Actor with Stash with ScorexLogging with ScorexEncoding with WalletForkRecovery {

  private val ergoAddressEncoder: ErgoAddressEncoder = settings.addressEncoder
  protected[wallet] case object ContinueFullChainProbe
  protected[wallet] case object RetryFullChainProbe
  protected[wallet] var pendingChainMessages = 0
  private var probeRetryScheduled = false
  protected[wallet] var supersedingRollback: Option[VersionTag] = None
  // A rollback can leave the two registry databases temporarily inconsistent.
  // Status must use the last checked height until the durable intent is cleared.
  protected[wallet] var retainedRollbackSourceHeight: Option[Int] = None

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
      state.storage.deepForkQuarantine match {
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
              beginFullChainProbe(state, tip, height)(
                selectedTip => {
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
                },
                _ => {
                  context.become(quarantinedWallet(state,
                    new IllegalStateException("Wallet deep-fork quarantine is active")))
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
      unstashAll()
    case _ => // stashing all messages until wallet is setup
      stash()
  }

  protected[wallet] def loadedWallet(state: ErgoWalletState): Receive =
    state.storage.deepForkQuarantine match {
      case Success(false) => activeWallet(state)
      case Success(true) =>
        quarantinedWallet(state, new IllegalStateException("Wallet deep-fork quarantine is active"))
      case Failure(t) =>
        quarantinedWallet(state, new IllegalStateException("Wallet deep-fork quarantine marker is unreadable", t))
    }

  protected def persistDeepForkQuarantine(state: ErgoWalletState): scala.util.Try[Unit] =
    state.storage.quarantineDeepFork()

  protected def rollbackRetainedRegistry(state: ErgoWalletState, version: VersionTag): Try[Unit] =
    state.registry.rollbackDurably(version)

  protected def clearRetainedRollbackIntent(state: ErgoWalletState,
                                            source: ModifierId,
                                            target: ModifierId): Try[Unit] =
    state.storage.clearRetainedRollback(source, target)

  private def readWalletState(state: ErgoWalletState): ErgoWalletState = {
    val ws = settings.walletSettings
    ergoWalletService.readWallet(
      state, ws.testMnemonic.map(SecretString.create(_)), ws.testKeysQty, ws.secretStorage
    )
  }

  private def activateWallet(newState: ErgoWalletState): Unit = {
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

  /** Keep one history lock for at most one fixed-size ancestor batch. */
  protected[wallet] def beginFullChainProbe(state: ErgoWalletState,
                                  targetId: ModifierId,
                                  targetHeight: Int)
                                 (onSelected: ModifierId => Unit,
                                  onOther: ModifierId => Unit): Unit = {
    context.become(provingFullChain(state, targetId, targetHeight,
      None, onSelected, onOther))
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
                               onOther: ModifierId => Unit): Receive = {
    case RetryFullChainProbe =>
      probeRetryScheduled = false
      self ! ContinueFullChainProbe
    case ContinueFullChainProbe =>
      Try(probeSelectedFullChain(targetId, targetHeight, cursor)) match {
        case Success(FullChainSelected(tip)) if historyReader.bestFullBlockIdOpt.contains(tip) =>
          if (!startSupersedingRollback(state)) onSelected(tip)
        case Success(FullChainOther(tip)) if historyReader.bestFullBlockIdOpt.contains(tip) =>
          if (!startSupersedingRollback(state)) onOther(tip)
        case Success(FullChainPending(next)) =>
          context.become(provingFullChain(state, targetId, targetHeight,
            Some(next), onSelected, onOther))
          self ! ContinueFullChainProbe
        case Success(FullChainUnknown) =>
          context.become(provingFullChain(state, targetId, targetHeight,
            None, onSelected, onOther))
          scheduleFullChainProbeRetry()
        case Failure(t) =>
          log.warn("Selected full-chain proof is temporarily unavailable", t)
          scheduleFullChainProbeRetry()
        case _ =>
          context.become(provingFullChain(state, targetId, targetHeight,
            None, onSelected, onOther))
          self ! ContinueFullChainProbe
      }
    case _: ChangedHistory =>
      // A header-only update does not invalidate ancestry below the same full tip.
      val retainedCursor = cursor.filter { current =>
        Try(historyReader.bestFullBlockIdOpt.contains(current.fullTipId)).getOrElse(false)
      }
      context.become(provingFullChain(state, targetId, targetHeight,
        retainedCursor, onSelected, onOther))
      self ! ContinueFullChainProbe
    case Rollback(version) =>
      supersedingRollback = Some(version)
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
    log.warn(detail)
    val reason = new IllegalStateException(detail)
    context.become(waitingForSelectedRollback(state, reason))
  }

  private def waitingForSelectedRollback(state: ErgoWalletState, reason: Throwable): Receive = {
    case Rollback(version) =>
      if (state.registry.hasVersion(version)) verifyRetainedRollback(state, version)
      else verifyMissingRollback(state, version)
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
    case _: ChangedState | _: ChangedMempool | _: ScanOffChain | _: ScanOnChain |
         _: ScanInThePast => deferChainUpdate(state)
    case msg =>
      quarantinedWallet(state,
        new IllegalStateException("Wallet is waiting for rollback-header authority"))(msg)
  }

  private def activeWallet(state: ErgoWalletState): Receive = {
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

    // We do wallet rescan by closing the wallet's database, deleting it from the disk, then reopening it and sending a rescan signal.
    case RescanWallet(fromHeight) =>
      if (!state.rescanInProgress) {
        log.info(s"Rescanning the wallet from height: $fromHeight")
        ergoWalletService.recreateRegistry(state, settings) match {
          case Success(newState) =>
            context.become(loadedWallet(newState.copy(rescanInProgress = true)))
            val heightToScanFrom = Math.min(newState.fullHeight, fromHeight)
            self ! ScanInThePast(heightToScanFrom, rescan = true)
            sender() ! Success(())
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
