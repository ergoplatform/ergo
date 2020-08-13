package org.ergoplatform.nodeView.wallet

import java.io.File
import java.util

import akka.actor.{Actor, ActorRef}
import cats.Traverse
import org.ergoplatform.ErgoBox._
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.persistence._
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, ExternalSecret, PaymentRequest, TransactionGenerationRequest}
import org.ergoplatform.nodeView.wallet.scanning.{Scan, ScanRequest}
import org.ergoplatform.settings._
import org.ergoplatform.utils.{BoxUtils, FileUtils}
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.wallet.TokensMap
import org.ergoplatform.wallet.boxes.{BoxSelector, ChainStatus, TrackedBox}
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.secrets.{DerivationPath, ExtendedSecretKey, JsonSecretStorage}
import org.ergoplatform.wallet.transactions.TransactionBuilder
import scorex.core.VersionTag
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedMempool, ChangedState}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, idToBytes}
import sigmastate.Values.ByteArrayConstant
import sigmastate.eval.Extensions._
import sigmastate.eval._

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Try}


class ErgoWalletActor(settings: ErgoSettings,
                      boxSelector: BoxSelector,
                      historyReader: ErgoHistoryReader)
  extends Actor with ScorexLogging with ScorexEncoding {

  import ErgoWalletActor._
  import cats.implicits._

  private implicit val ec: ExecutionContextExecutor = scala.concurrent.ExecutionContext.global

  private val walletSettings: WalletSettings = settings.walletSettings
  private implicit val ergoAddressEncoder: ErgoAddressEncoder = settings.addressEncoder

  private var secretStorageOpt: Option[JsonSecretStorage] = None
  private val storage: WalletStorage = WalletStorage.readOrCreate(settings)
  private var registry: WalletRegistry = WalletRegistry.apply(settings)
  private var offChainRegistry: OffChainRegistry = OffChainRegistry.init(registry)

  private var walletVars = WalletVars.apply(storage, settings)
  //todo: temporary 3.2.x collection and readers
  private var stateReaderOpt: Option[ErgoStateReader] = None
  private var mempoolReaderOpt: Option[ErgoMemPoolReader] = None
  private var utxoReaderOpt: Option[UtxoStateReader] = None

  // State context used to sign transactions and check that coins found in the blockchain are indeed belonging
  // to the wallet (by executing testing transactions against them).
  // The state context is being updated by listening to state updates.
  private def stateContext: ErgoStateContext = storage.readStateContext

  /**
    * Height of the chain as reported by the state
    * (i.e. height of a last block applied to the state, not the wallet)
    * Wallet's height may be behind it.
    */
  private var fullHeight: Int = ErgoHistory.EmptyHistoryHeight
  private var parameters: Parameters = LaunchParameters

  /**
    * @return height of the last block scanned by the wallet
    */
  private def walletHeight(): Int = {
    registry.fetchDigest().height
  }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ChangedState[_]])
    context.system.eventStream.subscribe(self, classOf[ChangedMempool[_]])

    walletSettings.testMnemonic match {
      case Some(testMnemonic) =>
        log.warn("Initializing wallet in test mode. Switch to secure mode for production usage.")
        val seed = Mnemonic.toSeed(testMnemonic)
        val rootSk = ExtendedSecretKey.deriveMasterKey(seed)
        val childSks = walletSettings.testKeysQty.toIndexedSeq.flatMap(x => (0 until x).map(rootSk.child))
        val prover = ErgoProvingInterpreter(rootSk +: childSks, parameters)
        walletVars = walletVars.withProver(prover)
      case None =>
        log.info("Trying to read wallet in secure mode ..")
        JsonSecretStorage.readFile(settings.walletSettings.secretStorage).fold(
          e => log.info(
            s"Failed to read wallet. Manual initialization is required to sign transactions. Cause: ${e.getCause}"),
          secretStorage => {
            log.info("Wallet loaded successfully and locked")
            secretStorageOpt = Some(secretStorage)
          }
        )
    }
  }

  /**
    * Process the block transactions and update database and in-memory structures for offchain data accordingly
    *
    * @param block - block to scan
    */
  private def scanBlock(block: ErgoFullBlock): Unit = {
    log.info(s"Wallet is going to scan a block ${block.id} at height ${block.height}")
    val (reg, offReg) =
      WalletScanLogic.scanBlockTransactions(registry, offChainRegistry, stateContext, walletVars, block)
    registry = reg
    offChainRegistry = offReg
  }

  private def scanLogic: Receive = {
    //scan mempool transaction
    case ScanOffChain(tx) =>
      val newWalletBoxes = WalletScanLogic.extractWalletOutputs(tx, None, walletVars)
      val inputs = WalletScanLogic.extractInputBoxes(tx)
      offChainRegistry = offChainRegistry.updateOnTransaction(newWalletBoxes, inputs)

    case ScanInThePast(blockHeight) =>
      val expectedHeight = walletHeight() + 1
      if (expectedHeight == blockHeight) {
        historyReader.bestFullBlockAt(blockHeight) match {
          case Some(block) =>
            scanBlock(block)
          case None =>
          // We may do not have a block if, for example, the blockchain is pruned. This is okay, just skip it.
        }
        if (blockHeight < fullHeight) {
          self ! ScanInThePast(blockHeight + 1)
        }
      }

    //scan block transactions
    case ScanOnChain(block) =>
      val expectedHeight = walletHeight() + 1
      if (expectedHeight == block.height) {
        scanBlock(block)
      } else if (expectedHeight < block.height) {
        log.warn(s"Wallet: skipped blocks found starting from $expectedHeight, going back to scan them")
        self ! ScanInThePast(expectedHeight)
      } else {
        log.warn(s"Wallet: block in the past reported at ${block.height}, blockId: ${block.id}")
      }

    case Rollback(version: VersionTag) =>
      registry.rollback(version).fold(
        e => log.error(s"Failed to rollback wallet registry to version $version due to: $e"), _ => ())
  }

  private def readers: Receive = {
    case ReadBalances(chainStatus) =>
      sender() ! (if (chainStatus.onChain) registry.fetchDigest() else offChainRegistry.digest)

    case ReadPublicKeys(from, until) =>
      sender() ! walletVars.publicKeyAddresses.slice(from, until)

    case GetFirstSecret =>
      if (walletVars.proverOpt.nonEmpty) {
        walletVars.proverOpt.foreach(_.hdKeys.headOption.foreach(s => sender() ! Success(s.privateInput)))
      } else {
        sender() ! Failure(new Exception("Wallet is locked"))
      }

    case GetWalletBoxes(unspent) =>
      val currentHeight = fullHeight
      sender() ! (if (unspent) registry.walletUnspentBoxes() else registry.walletConfirmedBoxes(0))
        .map(tb => WalletBox(tb, currentHeight))
        .sortBy(_.trackedBox.inclusionHeightOpt)

    case GetScanBoxes(scanId, unspent) =>
      val currentHeight = fullHeight
      sender() ! (if (unspent) registry.unspentBoxes(scanId) else registry.confirmedBoxes(scanId, 0))
        .map(tb => WalletBox(tb, currentHeight))
        .sortBy(_.trackedBox.inclusionHeightOpt)

    case GetTransactions =>
      sender() ! registry.allWalletTxs()
        .sortBy(-_.inclusionHeight)
        .map(tx => AugWalletTransaction(tx, fullHeight - tx.inclusionHeight))

    case GetTransaction(txId) =>
      sender() ! registry.getTx(txId)
        .map(tx => AugWalletTransaction(tx, fullHeight - tx.inclusionHeight))

    case ReadScans =>
      sender() ! ReadScansResponse(walletVars.externalScans)
  }

  private def updateUtxoSet(): Unit = {
    (mempoolReaderOpt, stateReaderOpt) match {
      case (Some(mr), Some(sr)) =>
        sr match {
          case u: UtxoStateReader =>
            utxoReaderOpt = Some(u.withTransactions(mr.getAll))
          case _ =>
        }
      case (_, _) =>
    }
  }

  private def onMempoolChanged: Receive = {
    case ChangedMempool(mr: ErgoMemPoolReader@unchecked) =>
      mempoolReaderOpt = Some(mr)
      updateUtxoSet()
  }

  private def onStateChanged: Receive = {
    case ChangedState(s: ErgoStateReader@unchecked) =>
      val stateContext = s.stateContext
      storage.updateStateContext(stateContext)
      fullHeight = stateContext.currentHeight
      parameters = stateContext.currentParameters

      stateReaderOpt = Some(s)
      updateUtxoSet()
  }

  // Secret is set in form of keystore file of testMnemonic in the config
  private def secretIsSet: Boolean = secretStorageOpt.nonEmpty || walletSettings.testMnemonic.nonEmpty

  /**
    * Address to send change to. Corresponds to first secret of the prover by default. Can be set by a user via API.
    */
  private def changeAddress(prover: ErgoProvingInterpreter): P2PKAddress =
    storage.readChangeAddress
      .getOrElse {
        log.info("Change address not specified. Using root address from wallet.")
        P2PKAddress(prover.hdPubKeys.head.key)
      }

  private def changeAddress: Option[P2PKAddress] = walletVars.proverOpt.map(changeAddress)

  private def walletInit: Receive = {
    //Init wallet (w. mnemonic generation) if secret is not set yet
    case InitWallet(pass, mnemonicPassOpt) if !secretIsSet =>
      //Read high-quality random bits from Java's SecureRandom
      val entropy = scorex.utils.Random.randomBytes(settings.walletSettings.seedStrengthBits / 8)
      val mnemonicTry = new Mnemonic(walletSettings.mnemonicPhraseLanguage, walletSettings.seedStrengthBits)
        .toMnemonic(entropy)
        .map { mnemonic =>
          val seed = Mnemonic.toSeed(mnemonic, mnemonicPassOpt)
          val secretStorage = JsonSecretStorage.init(seed, pass)(settings.walletSettings.secretStorage)
          secretStorageOpt = Some(secretStorage)
          mnemonic
        } match {
        case s: Success[String] =>
          self ! UnlockWallet(pass)
          util.Arrays.fill(entropy, 0: Byte)
          log.info("Wallet is initialized")
          s
        case Failure(t) =>
          val f = wrapLegalExc(t) //getting nicer message for illegal key size exception
          log.error(s"Wallet initialization is failed, details: ${f.exception.getMessage}")
          f
      }
      sender() ! mnemonicTry

    //Restore wallet with mnemonic if secret is not set yet
    case RestoreWallet(mnemonic, passOpt, encryptionPass) if !secretIsSet =>
      val res = Try {
        JsonSecretStorage.restore(mnemonic, passOpt, encryptionPass)(settings.walletSettings.secretStorage)
      }.map { secretStorage =>
        secretStorageOpt = Some(secretStorage)
      } match {
        case s: Success[Unit] =>
          self ! UnlockWallet(encryptionPass)
          log.info("Wallet is restored")
          s
        case Failure(t) =>
          val f = wrapLegalExc(t) //getting nicer message for illegal key size exception
          log.error(s"Wallet restoration is failed, details: ${f.exception.getMessage}")
          f
      }
      sender() ! res

    // branch for key already being set
    case _: RestoreWallet | _: InitWallet =>
      sender() ! Failure(new Exception("Wallet is already initialized or testMnemonic is set. Clear current secret to re-init it."))
  }

  private def walletCommands: Receive = {
    case CheckSeed(mnemonic, passOpt) =>
      secretStorageOpt match {
        case Some(secretStorage) =>
          val checkResult = secretStorage.checkSeed(mnemonic, passOpt)
          sender() ! checkResult
        case None =>
          sender() ! Failure(new Exception("Wallet not initialized"))
      }

    case UnlockWallet(encPass) =>
      secretStorageOpt match {
        case Some(secretStorage) =>
          val unlockResult = secretStorage.unlock(encPass)
          unlockResult match {
            case Success(_) =>
              Future {
                log.info("Starting wallet unlock")
                processUnlock(secretStorage)
                log.info("Wallet unlock finished")
              }
            case Failure(t) =>
              log.warn("Wallet unlock failed with: ", t)
          }
          sender() ! unlockResult
        case None =>
          sender() ! Failure(new Exception("Wallet not initialized"))
      }

    case LockWallet =>
      walletVars = walletVars.resetProver()
      secretStorageOpt.foreach(_.lock())

    case RescanWallet =>
      val registryFolder = WalletRegistry.registryFolder(settings)

      log.info(s"Rescanning the wallet, its registry is in $registryFolder")
      val rescanResult = Try {
        registry.close()
        FileUtils.deleteRecursive(registryFolder)
        registry = WalletRegistry.apply(settings)
      }
      rescanResult.recover { case t =>
        log.error("Error during rescan attempt: ", t)
      }
      sender() ! rescanResult

    case GetWalletStatus =>
      val status = WalletStatus(secretIsSet, walletVars.proverOpt.isDefined, changeAddress, walletHeight())
      sender() ! status

    case GenerateTransaction(requests, inputsRaw, dataInputsRaw) =>
      sender() ! generateTransactionWithOutputs(requests, inputsRaw, dataInputsRaw)

    case SignTransaction(secrets, tx, boxesToSpend, dataBoxes) =>
      sender() ! signTransaction(walletVars.proverOpt, secrets, tx, boxesToSpend, dataBoxes, parameters, stateContext)

    case DeriveKey(encodedPath) =>
      withWalletLockHandler(sender()) {
        _.secret.foreach { rootSecret =>
          DerivationPath.fromEncoded(encodedPath).foreach {
            case path if !path.publicBranch =>
              val secret = rootSecret.derive(path).asInstanceOf[ExtendedSecretKey]
              processSecretAddition(secret) match {
                case Success(_) => sender() ! Success(P2PKAddress(secret.publicKey.key))
                case f: Failure[Unit] => sender() ! f
              }
            case path =>
              sender() ! Failure(new Exception(
                s"A private path is expected, but the public one given: $path"))
          }
        }
      }

    case DeriveNextKey =>
      withWalletLockHandler(sender()) {
        _.secret.foreach { rootSecret =>
          sender() ! nextPath().flatMap { path =>
            val secret = rootSecret.derive(path).asInstanceOf[ExtendedSecretKey]
            processSecretAddition(secret).map { _ =>
              path -> P2PKAddress(secret.publicKey.key)
            }
          }
        }
      }

    case UpdateChangeAddress(address) =>
      storage.updateChangeAddress(address)

    case RemoveScan(scanId) =>
      val res: Try[Unit] = {
        storage.getScan(scanId) match {
          case None => Failure(new Exception(s"Scan #$scanId not found"))
          case Some(_) => Try(storage.removeScan(scanId))
        }
      }
      res.foreach(_ => walletVars = walletVars.removeScan(scanId))
      sender() ! RemoveScanResponse(res)

    case AddScan(appRequest) =>
      val res: Try[Scan] = storage.addScan(appRequest)
      res.foreach(app => walletVars = walletVars.addScan(app))
      sender() ! AddScanResponse(res)

    case StopTracking(scanId: ScanId, boxId: BoxId) =>
      sender() ! StopTrackingResponse(registry.removeScan(boxId, scanId))
  }

  override def receive: Receive =
    walletInit orElse
      walletCommands orElse
      onStateChanged orElse
      onMempoolChanged orElse
      scanLogic orElse
      readers

  private def withWalletLockHandler(callbackActor: ActorRef)
                                   (body: JsonSecretStorage => Unit): Unit =
    secretStorageOpt match {
      case Some(secretStorage) if !secretStorage.isLocked =>
        body(secretStorage)
      case Some(_) =>
        callbackActor ! Failure(new Exception("Wallet is locked"))
      case None =>
        callbackActor ! Failure(new Exception("Wallet is not initialized"))
    }

  private type FilterFn = TrackedBox => Boolean

  /**
    * This filter is selecting boxes which are onchain and not spent offchain yet or created offchain
    * (and not spent offchain, but that is ensured by offChainRegistry).
    * This filter is used when the wallet is going through its boxes to assemble a transaction.
    */
  private val walletFilter: FilterFn = (trackedBox: TrackedBox) => {
    val preStatus = if (trackedBox.chainStatus.onChain) {
      offChainRegistry.onChainBalances.exists(_.id == trackedBox.boxId)
    } else {
      true
    }

    val bid = trackedBox.box.id

    // double-check that box is not spent yet by inputs of mempool transactions
    def notInInputs: Boolean = {
      mempoolReaderOpt match {
        case Some(mr) => !mr.getAll.flatMap(_.inputs.map(_.boxId)).exists(_.sameElements(bid))
        case None => true
      }
    }

    // double-check that box is exists in UTXO set or outputs of offchain transaction
    def inOutputs: Boolean = {
      utxoReaderOpt.forall { utxo =>
        utxo.boxById(bid).isDefined
      }
    }

    preStatus && notInInputs && inOutputs
  }

  /**
    * This filter is not filtering out anything, used when the wallet works with externally provided boxes.
    */
  private val noFilter: FilterFn = (_: TrackedBox) => true

  /**
    * Convert requests (to make payments or to issue an asset) to transaction outputs
    * There can be only one asset issuance request in the input sequence.
    * @param requests - an input sequence of requests
    * @return sequence of transaction outputs or failure if inputs are incorrect
    */
  private def requestsToBoxCandidates(requests: Seq[TransactionGenerationRequest]): Try[Seq[ErgoBoxCandidate]] =
    Traverse[List].sequence {
      requests.toList
        .map {
          case PaymentRequest(address, value, assets, registers) =>
            Success(new ErgoBoxCandidate(value, address.script, fullHeight, assets.toColl, registers))
          case AssetIssueRequest(addressOpt, amount, name, description, decimals, registers) =>
            // Check that auxiliary registers do not try to rewrite registers R0...R6
            val registersCheck = if (registers.exists(_.forall(_._1.number < 7))) {
              Failure(new Exception("Additional registers contain R0...R6"))
            } else {
              Success(())
            }
            registersCheck.flatMap { _ =>
              val firstInputOpt = inputsFor(
                requests
                  .collect { case pr: PaymentRequest => pr.value }
                  .sum
              ).headOption
              firstInputOpt
                .fold[Try[ErgoBox]](Failure(new Exception("Can't issue asset with no inputs")))(Success(_))
                .flatMap { firstInput =>
                  val assetId = Digest32 !@@ firstInput.id
                  val nonMandatoryRegisters = scala.Predef.Map(
                    R4 -> ByteArrayConstant(name.getBytes("UTF-8")),
                    R5 -> ByteArrayConstant(description.getBytes("UTF-8")),
                    R6 -> ByteArrayConstant(String.valueOf(decimals).getBytes("UTF-8"))
                  ) ++ registers.getOrElse(Map())
                  (addressOpt orElse walletVars.publicKeyAddresses.headOption)
                    .fold[Try[ErgoAddress]](Failure(new Exception("No address available for box locking")))(Success(_))
                    .map { lockWithAddress =>
                      val minimalErgoAmount =
                        BoxUtils.minimalErgoAmountSimulated(
                          lockWithAddress.script,
                          Colls.fromItems(assetId -> amount),
                          nonMandatoryRegisters,
                          parameters
                        )
                      new ErgoBoxCandidate(
                        minimalErgoAmount,
                        lockWithAddress.script,
                        fullHeight,
                        Colls.fromItems(assetId -> amount),
                        nonMandatoryRegisters
                      )
                    }
                }
            }
          case other =>
            Failure(new Exception(s"Unknown TransactionRequest type: $other"))
        }
    }

  /**
    * A helper method which makes TrackedBox sequence out of boxes provided
    */
  private def boxesToFakeTracked(inputs: Seq[ErgoBox]): Iterator[TrackedBox] = {
    inputs
      .map { box => // declare fake inclusion height in order to confirm the box is onchain
        TrackedBox(box.transactionId, box.index, Some(1), None, None, box, Set(PaymentsScanId))
      }
      .toIterator
  }

  /**
    * Generates new transaction according to given requests using available or provided boxes.
    *
    * @param requests      - requests to transfer funds or to issue an asset
    * @param inputsRaw     - user-provided inputs. If empty then wallet is looking for inputs itself. If non-empty, then
    *                      the wallet is not adding anything, thus the user in this case should take care about satisfying
    *                      the (sum(inputs) == sum(outputs)) preservation rule for ergs.
    * @param dataInputsRaw - user-provided data (read-only) inputs. Wallet is not able to provide data inputs
    *                      (if they are needed in order to spend the spendable inputs).
    * @return generated transaction or an error
    */
  private def generateTransactionWithOutputs(requests: Seq[TransactionGenerationRequest],
                                             inputsRaw: Seq[String],
                                             dataInputsRaw: Seq[String]): Try[ErgoTransaction] = Try {

    // A helper which converts Base16-encoded boxes to ErgoBox instances
    def stringsToBoxes(strings: Seq[String]): Seq[ErgoBox] =
      strings.map(in => Base16.decode(in).flatMap(ErgoBoxSerializer.parseBytesTry)).map(_.get)

    walletVars.proverOpt match {
      case Some(prover) =>
        val userInputs = stringsToBoxes(inputsRaw)
        val dataInputs = stringsToBoxes(dataInputsRaw).toIndexedSeq

        requestsToBoxCandidates(requests).flatMap { outputs =>
          require(requests.count(_.isInstanceOf[AssetIssueRequest]) <= 1, "Too many asset issue requests")
          require(outputs.forall(c => c.value >= BoxUtils.minimalErgoAmountSimulated(c, parameters)), "Minimal ERG value not met")
          require(outputs.forall(_.additionalTokens.forall(_._2 > 0)), "Non-positive asset value")

          val assetIssueBox = outputs
            .zip(requests)
            .filter(_._2.isInstanceOf[AssetIssueRequest])
            .map(_._1)
            .headOption

          val targetBalance = outputs.map(_.value).sum
          val targetAssets = TransactionBuilder.collectOutputTokens(outputs.filterNot(bx => assetIssueBox.contains(bx)))

          val (inputBoxes, filter) = if (userInputs.nonEmpty) {
            //inputs are provided externally, no need for filtering
            (boxesToFakeTracked(userInputs), noFilter)
          } else {
            //inputs are to be selected by the wallet
            require(walletVars.publicKeyAddresses.nonEmpty, "No public keys in the prover to extract change address from")
            val boxesToSpend = (registry.walletUnspentBoxes() ++ offChainRegistry.offChainBoxes).distinct
            (boxesToSpend.toIterator, walletFilter)
          }

          val selectionOpt = boxSelector.select(inputBoxes, filter, targetBalance, targetAssets)

          selectionOpt.map { selectionResult =>
            prepareTransaction(prover, outputs, selectionResult, dataInputs)
          } match {
            case Right(txTry) => txTry.map(ErgoTransaction.apply)
            case Left(e) => Failure(
              new Exception(s"Failed to find boxes to assemble a transaction for $outputs, \nreason: $e")
            )
          }
        }

      case None =>
        Failure(new Exception(s"Cannot generateTransactionWithOutputs($requests, $inputsRaw): wallet is locked"))
    }
  }.flatten

  private def prepareTransaction(prover: ErgoProvingInterpreter,
                                 payTo: Seq[ErgoBoxCandidate],
                                 r: BoxSelector.BoxSelectionResult[TrackedBox],
                                 dataInputBoxes: IndexedSeq[ErgoBox]
                                ): Try[ErgoLikeTransaction] = {
    val inputs = r.boxes.map(_.box).toIndexedSeq
    val dataInputs = dataInputBoxes.map(dataInputBox => DataInput(dataInputBox.id))

    val changeAddr = changeAddress(prover).pubkey

    val changeBoxCandidates = r.changeBoxes.map { changeBox =>
      val assets = changeBox.tokens.map(t => Digest32 @@ idToBytes(t._1) -> t._2).toIndexedSeq
      new ErgoBoxCandidate(changeBox.value, changeAddr, fullHeight, assets.toColl)
    }

    val unsignedTx = new UnsignedErgoTransaction(
      inputs.map(_.id).map(id => new UnsignedInput(id)),
      dataInputs,
      (payTo ++ changeBoxCandidates).toIndexedSeq
    )

    prover.sign(unsignedTx, inputs, dataInputBoxes, stateContext)
      .fold(e => Failure(new Exception(s"Failed to sign boxes due to ${e.getMessage}: $inputs", e)), tx => Success(tx))
  }

  private def processSecretAddition(secret: ExtendedSecretKey): Try[Unit] = {
    walletVars.withExtendedKey(secret) match {
      case Success(newWalletVars) =>
        walletVars = newWalletVars
        val pubKey = secret.publicKey
        Success(storage.addKey(pubKey))
      case Failure(t) => Failure(t)
    }
  }

  private def processUnlock(secretStorage: JsonSecretStorage): Unit = Try {
    val rootSecretSeq = secretStorage.secret.toSeq

    if (rootSecretSeq.isEmpty) {
      log.warn("Master key is not available after unlock")
    }

    // first, we're trying to find in the database paths written by clients prior 3.3.0 and convert them
    // into a new format (pubkeys with paths stored instead of paths)
    val oldPaths = storage.readPaths()
    if (oldPaths.nonEmpty) {
      val oldDerivedSecrets = rootSecretSeq ++ oldPaths.flatMap { path =>
        rootSecretSeq.map(sk => sk.derive(path).asInstanceOf[ExtendedSecretKey])
      }
      val oldPubKeys = oldDerivedSecrets.map(_.publicKey)
      oldPubKeys.foreach(storage.addKey)
      storage.removePaths()
    }
    var pubKeys = storage.readAllKeys().toIndexedSeq

    //If no public keys in the database yet, add master's public key into it
    if (pubKeys.isEmpty) {
      val masterPubKey = rootSecretSeq.map(s => s.publicKey)
      masterPubKey.foreach(pk => storage.addKey(pk))
      pubKeys = masterPubKey.toIndexedSeq
    }

    val secrets = pubKeys.flatMap { pk =>
      val path = pk.path.toPrivateBranch
      rootSecretSeq.map(sk => sk.derive(path).asInstanceOf[ExtendedSecretKey])
    }
    walletVars = walletVars.withProver(ErgoProvingInterpreter(secrets, parameters))
  } match {
    case Success(_) =>
    case Failure(t) =>
      log.error("Unlock failed: ", t)
  }


  private def inputsFor(targetAmount: Long,
                        targetAssets: TokensMap = Map.empty): Seq[ErgoBox] = {
    val unspentBoxes = registry.walletUnspentBoxes()
    boxSelector
      .select(unspentBoxes.toIterator, walletFilter, targetAmount, targetAssets)
      .toSeq
      .flatMap(_.boxes)
      .map(_.box)
  }

  private def wrapLegalExc[T](e: Throwable): Failure[T] =
    if (e.getMessage.startsWith("Illegal key size")) {
      val dkLen = settings.walletSettings.secretStorage.encryption.dkLen
      Failure[T](new Exception(s"Key of length $dkLen is not allowed on your JVM version." +
        s"Set `ergo.wallet.secretStorage.encryption.dkLen = 128` or update JVM"))
    } else {
      Failure[T](e)
    }

  private def nextPath(): Try[DerivationPath] = {
    val secrets: IndexedSeq[ExtendedSecretKey] = walletVars.proverOpt.toIndexedSeq.flatMap(_.hdKeys)
    DerivationPath.nextPath(secrets)
  }

}

object ErgoWalletActor {

  // Private signals the wallet actor sends to itself

  /**
    * A signal the wallet actor sends to itself to scan a block in the past
    *
    * @param blockHeight - height of a block to scan
    */
  private final case class ScanInThePast(blockHeight: ErgoHistory.Height)


  // Publicly available signals for the wallet actor

  /**
    * Command to scan offchain transaction
    *
    * @param tx - offchain transaction
    */
  final case class ScanOffChain(tx: ErgoTransaction)

  /**
    * Command to scan a block
    *
    * @param block - block to scan
    */
  final case class ScanOnChain(block: ErgoFullBlock)

  /**
    * Rollback to previous version of the wallet, by throwing away effects of blocks after the version
    *
    * @param version
    */
  final case class Rollback(version: VersionTag)

  final case class SignTransaction(secrets: Seq[ExternalSecret],
                                   utx: UnsignedErgoTransaction,
                                   boxesToSpend: Seq[ErgoBox],
                                   dataBoxes: Seq[ErgoBox])

  /**
    * Generate new transaction fulfilling given requests
    *
    * @param requests
    * @param inputsRaw
    */
  final case class GenerateTransaction(requests: Seq[TransactionGenerationRequest],
                                       inputsRaw: Seq[String],
                                       dataInputsRaw: Seq[String])

  /**
    *
    * @param chainStatus
    */
  final case class ReadBalances(chainStatus: ChainStatus)

  /**
    * Read a slice of wallet public keys
    *
    * @param from
    * @param until
    */
  final case class ReadPublicKeys(from: Int, until: Int)

  /**
    * Initialize wallet with given wallet pass and optional mnemonic pass (according to BIP-32)
    *
    * @param walletPass
    * @param mnemonicPassOpt
    */
  final case class InitWallet(walletPass: String, mnemonicPassOpt: Option[String])

  /**
    * Restore wallet with mnemonic, optional mnemonic password and (mandatory) wallet encryption password
    *
    * @param mnemonic
    * @param mnemonicPassOpt
    * @param walletPass
    */
  final case class RestoreWallet(mnemonic: String, mnemonicPassOpt: Option[String], walletPass: String)

  /**
    * Unlock wallet with wallet password
    *
    * @param walletPass
    */
  final case class UnlockWallet(walletPass: String)

  /**
    * Derive key with given path according to BIP-32
    *
    * @param path
    */
  final case class DeriveKey(path: String)

  /**
    * Get boxes related to P2PK payments
    *
    * @param unspentOnly
    */
  final case class GetWalletBoxes(unspentOnly: Boolean)

  /**
    * Get boxes related to a scan
    *
    * @param unspentOnly
    */
  final case class GetScanBoxes(scanId: ScanId, unspentOnly: Boolean)

  /**
    * Set or update address for change outputs. Initially the address is set to root key address
    *
    * @param address
    */
  final case class UpdateChangeAddress(address: P2PKAddress)

  /**
    * Command to register new scan
    *
    * @param appRequest
    */
  final case class AddScan(appRequest: ScanRequest)

  /**
    * Wallet's response for scan registration request
    *
    * @param response
    */
  final case class AddScanResponse(response: Try[Scan])

  /**
    * Command to deregister a scan
    *
    * @param scanId
    */
  final case class RemoveScan(scanId: ScanId)

  /**
    * Wallet's response for scan removal request
    *
    * @param response
    */
  final case class RemoveScanResponse(response: Try[Unit])

  /**
    * Get wallet-related transaction
    *
    * @param id
    */
  final case class GetTransaction(id: ModifierId)

  final case class CheckSeed(mnemonic: String, passOpt: Option[String])

  /**
    * Get all wallet-related transaction
    */
  case object GetTransactions

  /**
    * Derive next key-pair according to BIP-32
    * //todo: describe procedure or provide a link
    */
  case object DeriveNextKey

  /**
    * Lock wallet
    */
  case object LockWallet

  /**
    * Rescan wallet
    */
  case object RescanWallet

  /**
    * Get wallet status
    */
  case object GetWalletStatus

  /**
    * Wallet status. To be sent in response to GetWalletStatus
    *
    * @param initialized   - whether wallet is initialized or not
    * @param unlocked      - whether wallet is unlocked or not
    * @param changeAddress - address used for change (optional)
    * @param height        - last height scanned
    */
  case class WalletStatus(initialized: Boolean,
                          unlocked: Boolean,
                          changeAddress: Option[P2PKAddress],
                          height: ErgoHistory.Height)

  /**
    * Get root secret key (used in miner)
    */
  case object GetFirstSecret

  /**
    * Get registered scans list
    */
  case object ReadScans

  /**
    * Get registered scans list
    */
  case class ReadScansResponse(apps: Seq[Scan])

  /**
    * Remove association between a scan and a box (remove a box if its the only one which belongs to the
    * scan)
    *
    * @param scanId
    * @param boxId
    */
  case class StopTracking(scanId: ScanId, boxId: BoxId)

  /**
    * Wrapper for a result of StopTracking processing
    *
    * @param status
    */
  case class StopTrackingResponse(status: Try[Unit])

  def signTransaction(proverOpt: Option[ErgoProvingInterpreter],
                      secrets: Seq[ExternalSecret],
                      tx: UnsignedErgoTransaction,
                      boxesToSpend: Seq[ErgoBox],
                      dataBoxes: Seq[ErgoBox],
                      parameters: Parameters,
                      stateContext: ErgoStateContext): Try[ErgoTransaction] = {
    val proverSecrets = proverOpt.map(_.secretKeys).getOrElse(Seq.empty)
    val secretsWrapped = secrets.map(_.key).toIndexedSeq
    val secretsProver = ErgoProvingInterpreter(secretsWrapped ++ proverSecrets, parameters)
    secretsProver
      .sign(tx, boxesToSpend.toIndexedSeq, dataBoxes.toIndexedSeq, stateContext)
      .map(ErgoTransaction.apply)
  }

}
