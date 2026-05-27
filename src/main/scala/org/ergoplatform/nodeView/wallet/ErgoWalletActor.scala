package org.ergoplatform.nodeView.wallet

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor._
import akka.pattern.StatusReply
import org.ergoplatform.ErgoBox._
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedMempool, ChangedState, UtxoSnapshotAppliedToState}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.history.ErgoHistoryUtils.Height
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.ErgoWalletServiceUtils.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, UtxoSnapshotScanStatus}
import org.ergoplatform.sdk.wallet.secrets.DerivationPath
import org.ergoplatform.settings._
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.BoxSelector
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform._
import org.ergoplatform.core.{VersionTag, versionToId}
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.utils.ScorexEncoding
import scorex.util.{ModifierId, ScorexLogging}

import scala.concurrent.duration._
import scala.util.{Failure, Success}

class ErgoWalletActor(settings: ErgoSettings,
                      parameters: Parameters,
                      ergoWalletService: ErgoWalletService,
                      boxSelector: BoxSelector,
                      historyReader: ErgoHistoryReader)
  extends Actor with Stash with ScorexLogging with ScorexEncoding {

  private val ergoAddressEncoder: ErgoAddressEncoder = settings.addressEncoder
  private var utxoSnapshotScanner: ActorRef = _

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
        utxoSnapshotScanner = context.actorOf(
          UtxoSnapshotWalletScanner.props(self, settings).withDispatcher(GlobalConstants.ApiDispatcher),
          "utxo-snapshot-wallet-scanner"
        )
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
      maybeStartAvailableUtxoSnapshotScan(newState)
      unstashAll()
    case _ => // stashing all messages until wallet is setup
      stash()
  }

  private def pendingUtxoSnapshotScan(state: ErgoWalletState): Option[UtxoSnapshotScanStatus] =
    if (settings.nodeSettings.utxoSettings.utxoBootstrap) {
      state.storage.readUtxoSnapshotScanStatus().filterNot(_.completed)
    } else {
      None
    }

  private def matchingUtxoSnapshotReader(status: UtxoSnapshotScanStatus,
                                         reader: UtxoStateReader): Boolean =
    status.snapshotHeight == reader.stateContext.currentHeight &&
      status.snapshotBlockId == versionToId(reader.version)

  private def startUtxoSnapshotScan(snapshotHeight: Height,
                                    snapshotBlockId: ModifierId,
                                    stateReader: UtxoStateReader,
                                    forceRestart: Boolean): Unit = {
    if (settings.nodeSettings.utxoSettings.utxoBootstrap) {
      utxoSnapshotScanner ! StartUtxoSnapshotScan(snapshotHeight, snapshotBlockId, stateReader, forceRestart)
    }
  }

  private def canScanUtxoSnapshot(state: ErgoWalletState): Boolean =
    state.walletVars.trackedBytes.nonEmpty || state.walletVars.externalScans.nonEmpty

  private def maybeStartAvailableUtxoSnapshotScan(state: ErgoWalletState): Unit = {
    val shouldStart =
      settings.nodeSettings.utxoSettings.utxoBootstrap &&
        settings.nodeSettings.isFullBlocksPruned &&
        canScanUtxoSnapshot(state) &&
        state.getWalletHeight == 0 &&
        state.storage.readUtxoSnapshotScanStatus().isEmpty

    if (shouldStart) {
      state.utxoStateReaderOpt
        .orElse(state.stateReaderOpt.collect { case reader: UtxoStateReader => reader })
        .foreach { reader =>
          startUtxoSnapshotScan(reader.stateContext.currentHeight, versionToId(reader.version), reader, forceRestart = false)
        }
    }
  }

  private def fail[T](message: String): Failure[T] =
    Failure(new IllegalStateException(message))

  private def loadedWallet(state: ErgoWalletState): Receive = {
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
          (pendingUtxoSnapshotScan(newState), s) match {
            case (Some(status), reader: UtxoStateReader) if matchingUtxoSnapshotReader(status, reader) =>
              startUtxoSnapshotScan(status.snapshotHeight, status.snapshotBlockId, reader, forceRestart = false)
            case (None, _: UtxoStateReader) =>
              maybeStartAvailableUtxoSnapshotScan(newState)
            case _ =>
          }
        case Failure(t) =>
          val errorMsg = s"Updating wallet state context failed : ${t.getMessage}"
          log.error(errorMsg, t)
          context.become(loadedWallet(state.copy(error = Some(errorMsg))))
      }

    case UtxoSnapshotAppliedToState(height, blockId, reader) =>
      if (canScanUtxoSnapshot(state)) {
        startUtxoSnapshotScan(height, blockId, reader, forceRestart = false)
      } else {
        log.debug("Skipping UTXO snapshot wallet scan because wallet scan variables are not initialized")
      }

    /* SCAN COMMANDS */
    case GetOrInitUtxoSnapshotScanStatus(snapshotHeight, snapshotBlockId, manifestDepth, totalSubtrees) =>
      val statusTry = state.storage.readUtxoSnapshotScanStatus() match {
        case Some(status)
          if status.snapshotHeight == snapshotHeight &&
            status.snapshotBlockId == snapshotBlockId &&
            status.manifestDepth == manifestDepth &&
            status.totalSubtrees == totalSubtrees =>
          Success(status)
        case _ =>
          val status = UtxoSnapshotScanStatus(
            snapshotHeight,
            snapshotBlockId,
            manifestDepth,
            nextSubtreeIndex = 0,
            totalSubtrees,
            completed = false
          )
          state.storage.writeUtxoSnapshotScanStatus(status).map(_ => status)
      }
      sender() ! statusTry

    case ApplyUtxoSnapshotScanBatch(snapshotHeight, snapshotBlockId, subtreeIndex, nextSubtreeIndex, completed, boxes) =>
      val replyTo = sender()
      val updateTry = state.storage.readUtxoSnapshotScanStatus() match {
        case Some(status)
          if status.snapshotHeight == snapshotHeight &&
            status.snapshotBlockId == snapshotBlockId &&
            !status.completed &&
            status.nextSubtreeIndex == subtreeIndex &&
            nextSubtreeIndex >= subtreeIndex =>
          val updatedStatus = status.copy(nextSubtreeIndex = nextSubtreeIndex, completed = completed)
          ergoWalletService
            .scanUtxoSnapshotChunk(
              state,
              boxes,
              snapshotBlockId,
              snapshotHeight,
              subtreeIndex,
              finalChunk = completed,
              settings.walletSettings.dustLimit
            )
            .flatMap { updatedState =>
              updatedState.storage.writeUtxoSnapshotScanStatus(updatedStatus).map { _ =>
                updatedState -> updatedStatus
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
            self ! ScanInThePast(updatedStatus.snapshotHeight + 1, rescan = false)
          }
          replyTo ! Success(updatedStatus)
        case Failure(t) =>
          val errorMsg = s"UTXO snapshot wallet scan batch failed : ${t.getMessage}"
          log.error(errorMsg, t)
          context.become(loadedWallet(state.copy(error = Some(errorMsg))))
          replyTo ! Failure(t)
      }

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
      if (pendingUtxoSnapshotScan(state).nonEmpty) {
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
          if (blockHeight < newState.fullHeight) {
            self ! ScanInThePast(blockHeight + 1, rescan)
          } else if (rescan) {
            log.info(s"Rescanning finished at height $blockHeight")
            context.become(loadedWallet(newState.copy(rescanInProgress = false)))
          }
        }
      }

    //scan block transactions
    case ScanOnChain(newBlock) =>
      if (state.secretIsSet(settings.walletSettings.testMnemonic)) { // scan blocks only if wallet is initialized
        if (pendingUtxoSnapshotScan(state).nonEmpty) {
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
          maybeStartAvailableUtxoSnapshotScan(newState)
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
            val resetState = newState.copy(
              offChainRegistry = OffChainRegistry.init(newState.registry),
              outputsFilter = None,
              rescanInProgress = true
            )
            if (settings.nodeSettings.utxoSettings.utxoBootstrap && settings.nodeSettings.isFullBlocksPruned) {
              resetState.storage.removeUtxoSnapshotScanStatus() match {
                case Success(_) =>
                  val utxoReaderOpt = resetState.utxoStateReaderOpt
                    .orElse(resetState.stateReaderOpt.collect { case reader: UtxoStateReader => reader })
                  utxoReaderOpt match {
                    case Some(reader) =>
                      val snapshotHeight = reader.stateContext.currentHeight
                      val snapshotBlockId = versionToId(reader.version)
                      context.become(loadedWallet(resetState))
                      startUtxoSnapshotScan(snapshotHeight, snapshotBlockId, reader, forceRestart = true)
                      sender() ! Success(())
                    case None =>
                      val err = new IllegalStateException("Unable to restart UTXO snapshot wallet scan: UTXO state reader is unavailable")
                      context.become(loadedWallet(resetState.copy(error = Some(err.getMessage), rescanInProgress = false)))
                      sender() ! Failure(err)
                  }
                case Failure(t) =>
                  log.error("Error during UTXO snapshot wallet scan reset: ", t)
                  context.become(loadedWallet(resetState.copy(error = Some(t.getMessage), rescanInProgress = false)))
                  sender() ! Failure(t)
              }
            } else {
              context.become(loadedWallet(resetState))
              val heightToScanFrom = Math.min(resetState.fullHeight, fromHeight)
              self ! ScanInThePast(heightToScanFrom, rescan = true)
              sender() ! Success(())
            }
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
      state.storage.updateChangeAddress(address) match {
        case Success(_) =>
          sender() ! StatusReply.success(())
        case Failure(t) =>
          log.error(s"Unable to update change address", t)
          sender() ! StatusReply.error(s"Unable to update change address : ${t.getMessage}")
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
