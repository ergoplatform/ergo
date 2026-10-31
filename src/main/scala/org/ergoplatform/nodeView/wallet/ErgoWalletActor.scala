package org.ergoplatform.nodeView.wallet

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor._
import akka.pattern.StatusReply
import org.ergoplatform.ErgoBox._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedMempool, ChangedState}
import org.ergoplatform.nodeView.history.ErgoHistoryReader
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.ErgoStateReader
import org.ergoplatform.nodeView.wallet.ErgoWalletService.ChangeAddressValidationException
import org.ergoplatform.nodeView.wallet.ErgoWalletServiceUtils.DeriveNextKeyResult
import org.ergoplatform.nodeView.wallet.persistence.{OffChainRegistry, WalletStorage}
import org.ergoplatform.sdk.wallet.secrets.DerivationPath
import org.ergoplatform.settings._
import org.ergoplatform.wallet.Constants.ScanId
import org.ergoplatform.wallet.boxes.BoxSelector
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform._
import org.ergoplatform.core.VersionTag
import org.ergoplatform.sdk.SecretString
import org.ergoplatform.utils.ScorexEncoding
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.annotation.tailrec

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class ErgoWalletActor(settings: ErgoSettings,
                      parameters: Parameters,
                      ergoWalletService: ErgoWalletService,
                      boxSelector: BoxSelector,
                      historyReader: ErgoHistoryReader)
  extends Actor with Stash with ScorexLogging with ScorexEncoding {

  private val ergoAddressEncoder: ErgoAddressEncoder = settings.addressEncoder
  private val alreadyInitializedMessage =
    "Wallet is already initialized or testMnemonic is set. Clear current secret to re-init it."

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
      pruneObsoleteUnconfirmedTransactions(newState)
      context.become(loadedWallet(ergoWalletService.restoreOffChainState(newState)))
      unstashAll()
    case _ => // stashing all messages until wallet is setup
      stash()
  }

  private def loadedWallet(state: ErgoWalletState): Receive = {
    // Init wallet (w. mnemonic generation) if secret is not set yet
    case InitWallet(walletPass, mnemonicPassOpt) if !state.secretIsSet(settings.walletSettings.testMnemonic) =>
      val initialized =
        try {
          Try(ergoWalletService.initWallet(state, settings, walletPass, mnemonicPassOpt)).flatten
        } finally mnemonicPassOpt.foreach(_.erase())
      initialized match {
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
      val restored =
        try {
          Try(
            ergoWalletService.restoreWallet(
              state,
              settings,
              mnemonic,
              mnemonicPassOpt,
              walletPass,
              usePre1627KeyDerivation
            )
          ).flatten
        } finally {
          mnemonic.erase()
          mnemonicPassOpt.foreach(_.erase())
        }
      restored match {
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
    case RestoreWallet(mnemonic, mnemonicPassOpt, walletPass, _) =>
      mnemonic.erase()
      mnemonicPassOpt.foreach(_.erase())
      walletPass.erase()
      sender() ! Failure(new Exception(alreadyInitializedMessage))

    case InitWallet(walletPass, mnemonicPassOpt) =>
      walletPass.erase()
      mnemonicPassOpt.foreach(_.erase())
      sender() ! Failure(new Exception(alreadyInitializedMessage))

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
      val (newState, walletAffected) = ergoWalletService.scanOffChainUpdate(state, tx)
      if (walletAffected) {
        // the transaction is kept until it gets on the blockchain, so that a restart in the meantime
        // does not make the wallet consider its inputs spendable again
        state.storage.addUnconfirmedTransaction(tx, state.fullHeight) match {
          case Success(_) =>
          case Failure(t) => log.error(s"Could not store unconfirmed transaction ${tx.id}: ", t)
        }
      }
      context.become(loadedWallet(newState))

    case ReadUnconfirmedTransactions =>
      sender() ! unconfirmedTransactionsToRestore(state)

    case ForgetUnconfirmedTransactions(ids) =>
      if (state.storage.unconfirmedTransactionHeights.nonEmpty && ids.nonEmpty) {
        val storedTxs = state.storage.readUnconfirmedTransactions().map(_._1)
        val storedIds = storedTxs.map(_.id).toSet
        val confirmedInRegistry = ids.filter(id => state.registry.getTx(id).isDefined).toSet
        val unconfirmedToForget = ids.toSet -- confirmedInRegistry
        val cascaded = confirmedInRegistry ++
          ErgoWalletActor.collectWithDescendants(storedTxs, unconfirmedToForget)
        val existingToForget = cascaded.intersect(storedIds).toSeq
        if (existingToForget.nonEmpty && forgetUnconfirmedTransactions(state, existingToForget)) {
          context.become(loadedWallet(refreshOffChainState(state)))
        }
      }

    // rescan=true means we serve a user request for rescan from arbitrary height
    case ScanInThePast(blockHeight, rescan) =>
      val nextBlockHeight = state.expectedNextBlockHeight(blockHeight, settings.nodeSettings.isFullBlocksPruned)
      if (nextBlockHeight == blockHeight || rescan) {
        val (newState, scanFailed) =
          historyReader.bestFullBlockAt(blockHeight) match {
            case Some(block) =>
              val operation = if (rescan) "rescanning" else "scanning"
              log.info(s"Wallet is $operation a block ${block.id} in the past at height ${block.height}")
              ergoWalletService.scanBlockUpdate(state, block, settings.walletSettings.dustLimit) match {
                case Failure(ex) =>
                  val errorMsg = s"Block ${block.id} $operation at height $blockHeight failed : ${ex.getMessage}"
                  log.error(errorMsg, ex)
                  (state.copy(error = Some(errorMsg)), true)
                case Success(updatedState) =>
                  (forgetConfirmedAndExpired(updatedState, block, refreshOffChain = !rescan), false)
              }
            case None =>
              (state, false) // We may do not have a block if, for example, the blockchain is pruned. This is okay, just skip it.
        }
        val nextState = if (scanFailed && rescan) newState.copy(rescanInProgress = false) else newState
        context.become(loadedWallet(nextState))
        if (!scanFailed) {
          if (blockHeight < nextState.fullHeight) {
            self ! ScanInThePast(blockHeight + 1, rescan)
          } else if (rescan) {
            log.info(s"Rescanning finished at height $blockHeight")
            val refreshedState = if (nextState.storage.unconfirmedTransactionHeights.nonEmpty) {
              refreshOffChainState(nextState)
            } else {
              nextState
            }
            context.become(loadedWallet(refreshedState.copy(rescanInProgress = false)))
          }
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
                forgetConfirmedAndExpired(updatedState, newBlock, refreshOffChain = true)
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
      val reply: Any = try {
        state.secretStorageOpt match {
          case Some(secretStorage) =>
            secretStorage.checkSeed(mnemonic, passOpt)
          case None =>
            Failure(new Exception("Wallet not initialized"))
        }
      } finally {
        mnemonic.erase()
        passOpt.foreach(_.erase())
      }
      sender() ! reply

    case UnlockWallet(walletPass) =>
      log.info("Unlocking wallet")
      val unlocked = try {
        ergoWalletService.unlockWallet(state, walletPass, settings.walletSettings.usePreEip3Derivation)
      } finally walletPass.erase()
      unlocked match {
        case Success(newState) =>
          log.info("Wallet successfully unlocked")
          context.become(loadedWallet(newState))
          sender() ! Success(())
        case f@Failure(t) =>
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

  private def refreshOffChainState(state: ErgoWalletState): ErgoWalletState =
    ergoWalletService.restoreOffChainState(state.copy(offChainRegistry = OffChainRegistry.init(state.registry)))

  /**
    * Stored unconfirmed transactions worth putting back into the memory pool. Transactions already
    * confirmed or conflicted on-chain according to [[WalletRegistry]], or left unconfirmed past
    * [[WalletStorage.UnconfirmedTxLifetimeInBlocks]], are pruned instead of re-submitted.
    */
  private def unconfirmedTransactionsToRestore(state: ErgoWalletState): Seq[ErgoTransaction] =
    ErgoWalletActor.orderByDependency(pruneObsoleteUnconfirmedTransactions(state))

  private def pruneObsoleteUnconfirmedTransactions(state: ErgoWalletState): Seq[ErgoTransaction] = {
    if (state.storage.unconfirmedTransactionHeights.isEmpty) {
      Seq.empty
    } else {
      val storedWithHeights = state.storage.readUnconfirmedTransactions()
      val storedTxs = storedWithHeights.map(_._1)

      val confirmed = storedTxs.iterator
        .filter { tx =>
          state.registry.getTx(tx.id).isDefined ||
            tx.inputs.exists(in => state.registry.getBox(in.boxId).exists(_.spendingTxIdOpt.contains(tx.id)))
        }
        .map(_.id)
        .toSet

      val conflictedInRegistry = storedTxs.iterator
        .filterNot(tx => confirmed.contains(tx.id))
        .filter { tx =>
          val inputSpentByOther = tx.inputs.exists { in =>
            state.registry.getBox(in.boxId).exists { tb =>
              tb.spendingHeightOpt.isDefined && !tb.spendingTxIdOpt.contains(tx.id)
            }
          }
          val dataInputSpent = tx.dataInputs.exists { di =>
            state.registry.getBox(di.boxId).exists(_.spendingHeightOpt.isDefined)
          }
          inputSpentByOther || dataInputSpent
        }
        .map(_.id)
        .toSet

      val expired = storedWithHeights.iterator
        .collect {
          case (tx, seenAt)
            if !confirmed.contains(tx.id) &&
              state.fullHeight - seenAt > WalletStorage.UnconfirmedTxLifetimeInBlocks => tx.id
        }
        .toSet

      val deadUnconfirmed = ErgoWalletActor.collectWithDescendants(storedTxs, conflictedInRegistry ++ expired)
      val toForgetSet = confirmed ++ deadUnconfirmed
      if (toForgetSet.nonEmpty) {
        forgetUnconfirmedTransactions(state, toForgetSet.toSeq)
      }
      storedTxs.filterNot(tx => toForgetSet.contains(tx.id))
    }
  }

  /**
    * Drop stored unconfirmed transactions confirmed in `block`, conflicted when `block` spent one
    * of their inputs or data inputs via a different transaction, or left unconfirmed for longer
    * than [[WalletStorage.UnconfirmedTxLifetimeInBlocks]] blocks (along with stored unconfirmed
    * descendants of conflicted or expired transactions).
    */
  private def forgetConfirmedAndExpired(state: ErgoWalletState,
                                        block: ErgoFullBlock,
                                        refreshOffChain: Boolean): ErgoWalletState = {
    val seenAtHeights = state.storage.unconfirmedTransactionHeights
    if (seenAtHeights.isEmpty) {
      state
    } else {
      val storedWithHeights = state.storage.readUnconfirmedTransactions()
      val storedTxs = storedWithHeights.map(_._1)
      val blockTxIds = block.transactions.iterator.map(_.id).toSet
      val confirmed = blockTxIds.intersect(seenAtHeights.keySet)
      val blockSpentBoxIds = block.transactions.iterator
        .flatMap(_.inputs.iterator.map(in => bytesToId(in.boxId)))
        .toSet

      val directlyConflicted = storedTxs.iterator
        .filterNot(tx => confirmed.contains(tx.id))
        .filter { tx =>
          (tx.inputs.iterator.map(_.boxId) ++ tx.dataInputs.iterator.map(_.boxId))
            .exists(boxId => blockSpentBoxIds.contains(bytesToId(boxId)))
        }
        .map(_.id)
        .toSet

      val expired = seenAtHeights.collect {
        case (id, seenAt)
          if !confirmed.contains(id) &&
            block.height - seenAt > WalletStorage.UnconfirmedTxLifetimeInBlocks => id
      }.toSet

      if (expired.nonEmpty) {
        log.warn(s"Wallet gave up on ${expired.size} transaction(s) still unconfirmed at height ${block.height}")
      }

      val conflictedWithDescendants = ErgoWalletActor.collectWithDescendants(storedTxs, directlyConflicted)
      if (conflictedWithDescendants.nonEmpty) {
        log.info(s"Wallet dropping ${conflictedWithDescendants.size} unconfirmed transaction(s) conflicted on-chain at height ${block.height}")
      }
      val deadUnconfirmed = ErgoWalletActor.collectWithDescendants(storedTxs, conflictedWithDescendants ++ expired)

      val toForget = (confirmed ++ deadUnconfirmed).toSeq
      if (toForget.nonEmpty) {
        forgetUnconfirmedTransactions(state, toForget)
      }
      if (refreshOffChain) refreshOffChainState(state) else state
    }
  }

  private def forgetUnconfirmedTransactions(state: ErgoWalletState, ids: Seq[ModifierId]): Boolean = {
    state.storage.removeUnconfirmedTransactions(ids) match {
      case Success(_) => true
      case Failure(t) =>
        log.error("Could not forget unconfirmed transactions: ", t)
        false
    }
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

  /**
    * Order transactions so that a transaction spending or referencing (via `inputs` or
    * `dataInputs`) an output of another one comes after it.
    *
    * Unconfirmed transactions do form such chains, and both consumers of this ordering need it: the
    * off-chain registry only nets a spending out if it has seen the box being spent already, and the
    * memory pool refuses or invalidates a transaction whose inputs or data inputs it does not know yet.
    *
    * Transactions with no producer among `txs` keep their relative order. A cycle is impossible
    * between valid transactions, but should one be given, its members are appended unordered rather
    * than dropped.
    */
  def orderByDependency(txs: Seq[ErgoTransaction]): Seq[ErgoTransaction] = {
    val producerOf: Map[ModifierId, ModifierId] =
      txs.flatMap(tx => tx.outputs.map(out => bytesToId(out.id) -> tx.id)).toMap

    @tailrec
    def loop(remaining: Seq[ErgoTransaction],
             ordered: Set[ModifierId],
             acc: Seq[ErgoTransaction]): Seq[ErgoTransaction] = {
      if (remaining.isEmpty) {
        acc
      } else {
        val (ready, blocked) = remaining.partition { tx =>
          (tx.inputs.iterator.map(_.boxId) ++ tx.dataInputs.iterator.map(_.boxId)).forall { boxId =>
            producerOf.get(bytesToId(boxId)).forall(ordered.contains)
          }
        }
        if (ready.isEmpty) {
          acc ++ blocked
        } else {
          loop(blocked, ordered ++ ready.map(_.id), acc ++ ready)
        }
      }
    }

    loop(txs, Set.empty, Seq.empty)
  }

  /**
    * Expand `rootIds` to include any transaction in `txs` that directly or transitively spends or
    * references (via `inputs` or `dataInputs`) an output produced by a transaction in the set.
    */
  def collectWithDescendants(txs: Seq[ErgoTransaction], rootIds: Set[ModifierId]): Set[ModifierId] = {
    if (rootIds.isEmpty || txs.isEmpty) {
      rootIds
    } else {
      val producerOf: Map[ModifierId, ModifierId] =
        txs.flatMap(tx => tx.outputs.map(out => bytesToId(out.id) -> tx.id)).toMap

      @tailrec
      def loop(acc: Set[ModifierId]): Set[ModifierId] = {
        val expanded = txs.foldLeft(acc) { case (set, tx) =>
          if (set.contains(tx.id)) {
            set
          } else {
            val dependsOnDead = (tx.inputs.iterator.map(_.boxId) ++ tx.dataInputs.iterator.map(_.boxId))
              .exists(boxId => producerOf.get(bytesToId(boxId)).exists(set.contains))
            if (dependsOnDead) set + tx.id else set
          }
        }
        if (expanded.size == acc.size) acc else loop(expanded)
      }

      loop(rootIds)
    }
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
