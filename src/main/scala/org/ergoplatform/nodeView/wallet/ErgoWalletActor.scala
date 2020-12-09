package org.ergoplatform.nodeView.wallet

import akka.actor.{Actor, ActorRef}
import cats.Traverse
import com.google.common.hash.BloomFilter
import org.ergoplatform.ErgoBox._
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader, UtxoStateReader}
import org.ergoplatform.nodeView.wallet.persistence._
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, ExternalSecret, PaymentRequest, TransactionGenerationRequest}
import org.ergoplatform.nodeView.wallet.scanning.{Scan, ScanRequest}
import org.ergoplatform.settings._
import org.ergoplatform.utils.{BoxUtils, FileUtils}
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}
import org.ergoplatform.wallet.boxes.BoxSelector.BoxSelectionResult
import org.ergoplatform.wallet.boxes.{BoxSelector, ChainStatus, TrackedBox}
import org.ergoplatform.wallet.interpreter.{ErgoProvingInterpreter, TransactionHintsBag}
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.protocol.context.ErgoLikeStateContext
import org.ergoplatform.wallet.secrets.{DerivationPath, ExtendedSecretKey, JsonSecretStorage}
import org.ergoplatform.wallet.transactions.TransactionBuilder
import scorex.core.VersionTag
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedMempool, ChangedState}
import scorex.core.utils.ScorexEncoding
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, idToBytes}
import sigmastate.Values.SigmaBoolean
import sigmastate.basics.DLogProtocol.ProveDlog
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

  // Bloom filter for boxes not being spent to the moment
  private var outputsFilter: Option[BloomFilter[Array[Byte]]] = None

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
    import WalletScanLogic.scanBlockTransactions

    log.info(s"Wallet is going to scan a block ${block.id} at height ${block.height}")
    val (reg, offReg, updatedOutputsFilter) =
      scanBlockTransactions(registry, offChainRegistry, stateContext, walletVars, block, outputsFilter)
    registry = reg
    offChainRegistry = offReg
    outputsFilter = Some(updatedOutputsFilter)
  }

  // expected height of a next block when the wallet is receiving a new block with the height blockHeight
  private def expectedHeight(blockHeight: Height): Height = {
    if (!settings.nodeSettings.isFullBlocksPruned) {
      // Node has all the full blocks and applies them sequentially
      walletHeight() + 1
    } else {
      // Node has pruned blockchain
      if (walletHeight() == 0) blockHeight else walletHeight() + 1
    }
  }

  private def scanLogic: Receive = {
    //scan mempool transaction
    case ScanOffChain(tx) =>
      val newWalletBoxes = WalletScanLogic.extractWalletOutputs(tx, None, walletVars)
      val inputs = WalletScanLogic.extractInputBoxes(tx)
      offChainRegistry = offChainRegistry.updateOnTransaction(newWalletBoxes, inputs)

    case ScanInThePast(blockHeight) =>
      if (expectedHeight(blockHeight) == blockHeight) {
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
      val expHeight = expectedHeight(block.height)
      if (expHeight == block.height) {
        scanBlock(block)
      } else if (expHeight < block.height) {
        log.warn(s"Wallet: skipped blocks found starting from $expHeight, going back to scan them")
        self ! ScanInThePast(expHeight)
      } else {
        log.warn(s"Wallet: block in the past reported at ${block.height}, blockId: ${block.id}")
      }

    case Rollback(version: VersionTag) =>
      registry.rollback(version) match {
        case Failure(t) =>
          log.error(s"Failed to rollback wallet registry to version $version due to: $t")
        case _: Success[Unit] =>
          // Reset outputs Bloom filter to have it initialized again on next block scanned
          // todo: for offchain registry, refresh is also needed, https://github.com/ergoplatform/ergo/issues/1180
          outputsFilter = None
      }
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

    /*
     * Read wallet boxes, unspent only (if corresponding flag is set), or all (both spent and unspent).
     * If considerUnconfirmed flag is set, mempool contents is considered as well.
     */
    case GetWalletBoxes(unspent, considerUnconfirmed) =>
      val currentHeight = fullHeight
      val boxes = if (unspent) {
        val confirmed = registry.walletUnspentBoxes()
        if (considerUnconfirmed) {
          // We filter out spent boxes in the same way as wallet does when assembling a transaction
          (confirmed ++ offChainRegistry.offChainBoxes).filter(walletFilter)
        } else {
          confirmed
        }
      } else {
        val confirmed = registry.walletConfirmedBoxes()
        if (considerUnconfirmed) {
          // Just adding boxes created off-chain
          confirmed ++ offChainRegistry.offChainBoxes
        } else {
          confirmed
        }
      }
      sender() ! boxes.map(tb => WalletBox(tb, currentHeight)).sortBy(_.trackedBox.inclusionHeightOpt)

    case GetScanBoxes(scanId, unspent, considerUnconfirmed) =>

      val unconfirmed = if(considerUnconfirmed) {
        offChainRegistry.offChainBoxes.filter(_.scans.contains(scanId))
      } else Seq.empty

      val currentHeight = fullHeight
      val boxes = (if (unspent){
        registry.unspentBoxes(scanId)
      } else {
        registry.confirmedBoxes(scanId)
      }) ++ unconfirmed
      sender() ! boxes.map(tb => WalletBox(tb, currentHeight)).sortBy(_.trackedBox.inclusionHeightOpt)

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
            utxoReaderOpt = Some(u.withMempool(mr))
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
          java.util.Arrays.fill(entropy, 0: Byte)
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
        val secretStorageSettings = settings.walletSettings.secretStorage
        JsonSecretStorage.restore(mnemonic, passOpt, encryptionPass, secretStorageSettings)
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

  // Read a box from UTXO set if the node has it, otherwise, from the wallet
  private def readBox(boxId: BoxId): Option[ErgoBox] = {
    utxoReaderOpt match {
      case Some(utxoReader) =>
        utxoReader.boxById(boxId)
      case None =>
        registry.getBox(boxId).map(_.box)
    }
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
      // We do wallet rescan by closing the wallet's database, deleting it from the disk,
      // then reopening it and sending a rescan signal.
      val rescanResult = Try {
        val registryFolder = WalletRegistry.registryFolder(settings)
        log.info(s"Rescanning the wallet, the registry is in $registryFolder")
        registry.close()
        FileUtils.deleteRecursive(registryFolder)
        registry = WalletRegistry.apply(settings)
        self ! ScanInThePast(walletHeight()) // walletHeight() corresponds to empty wallet state now
      }
      rescanResult.recover { case t =>
        log.error("Error during rescan attempt: ", t)
      }
      sender() ! rescanResult

    case GetWalletStatus =>
      val status = WalletStatus(secretIsSet, walletVars.proverOpt.isDefined, changeAddress, walletHeight())
      sender() ! status

    case GenerateTransaction(requests, inputsRaw, dataInputsRaw, sign) =>
      // generate signed or unsigned transaction
      val tx = if (sign) {
        generateSignedTransaction(requests, inputsRaw, dataInputsRaw)
      } else {
        generateUnsignedTransaction(requests, inputsRaw, dataInputsRaw).map(_._1)
      }
      sender() ! tx

    case GenerateCommitmentsFor(unsignedTx, externalSecretsOpt, externalInputsOpt, externalDataInputsOpt) =>
      val walletSecrets = walletVars.proverOpt.map(_.secretKeys).getOrElse(Seq.empty)
      val secrets = walletSecrets ++ externalSecretsOpt.getOrElse(Seq.empty).map(_.key)
      val prover: ErgoProvingInterpreter = ErgoProvingInterpreter(secrets.toIndexedSeq, parameters)

      val inputBoxes = externalInputsOpt.map(_.toIndexedSeq).getOrElse {
        unsignedTx.inputs.flatMap { unsignedInput =>
          readBox(unsignedInput.boxId)
        }
      }

      val dataBoxes = externalDataInputsOpt.map(_.toIndexedSeq).getOrElse {
        unsignedTx.dataInputs.flatMap { dataInput =>
          readBox(dataInput.boxId)
        }
      }

      val thbTry = prover.generateCommitmentsFor(unsignedTx, inputBoxes, dataBoxes, stateContext)
      sender() ! GenerateCommitmentsResponse(thbTry)

    case SignTransaction(tx, secrets, hints, boxesToSpendOpt, dataBoxesOpt) =>
      val boxesToSpend = boxesToSpendOpt.getOrElse(tx.inputs.flatMap { input =>
        readBox(input.boxId)
      })
      val dataBoxes = dataBoxesOpt.getOrElse(tx.dataInputs.flatMap { dataInput =>
        readBox(dataInput.boxId)
      })
      sender() ! signTransaction(walletVars.proverOpt, tx, secrets, hints, boxesToSpend, dataBoxes, parameters, stateContext)

    case ExtractHints(tx, real, simulated, boxesToSpendOpt, dataBoxesOpt) =>
      val inputBoxes = boxesToSpendOpt.map(_.toIndexedSeq).getOrElse {
        tx.inputs.flatMap { input =>
          readBox(input.boxId)
        }
      }

      val dataBoxes = dataBoxesOpt.map(_.toIndexedSeq).getOrElse {
        tx.dataInputs.flatMap { dataInput =>
          readBox(dataInput.boxId)
        }
      }

      val prover = walletVars.proverOpt.getOrElse(ErgoProvingInterpreter(IndexedSeq.empty, parameters))
      val bag = prover.bagForTransaction(tx, inputBoxes, dataBoxes, stateContext, real, simulated)
      sender() ! ExtractHintsResult(bag)

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
        _.secret.foreach { masterKey =>
          sender() ! nextKey(masterKey)
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

    case AddBox(box: ErgoBox, scanIds: Set[ScanId]) =>
      registry.updateScans(scanIds, box)
      sender() ! AddBoxResponse(Success(()))

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
        case Some(mr) => !mr.spentInputs.exists(_.sameElements(bid))
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
    *
    * @param requests - an input sequence of requests
    * @return sequence of transaction outputs or failure if inputs are incorrect
    */
  private def requestsToBoxCandidates(requests: Seq[TransactionGenerationRequest],
                                      assetId: BoxId): Try[Seq[ErgoBoxCandidate]] =
    Traverse[List].sequence {
      requests.toList
        .map {
          case PaymentRequest(address, value, assets, registers) =>
            Success(new ErgoBoxCandidate(value, address.script, fullHeight, assets.toColl, registers))
          case AssetIssueRequest(addressOpt, valueOpt, amount, name, description, decimals, registers) =>
            // Check that auxiliary registers do not try to rewrite registers R0...R6
            val registersCheck = if (registers.exists(_.forall(_._1.number < 7))) {
              Failure(new Exception("Additional registers contain R0...R6"))
            } else {
              Success(())
            }
            registersCheck.flatMap { _ =>
              val nonMandatoryRegisters = scala.Predef.Map(
                R4 -> ByteArrayConstant(name.getBytes("UTF-8")),
                R5 -> ByteArrayConstant(description.getBytes("UTF-8")),
                R6 -> ByteArrayConstant(String.valueOf(decimals).getBytes("UTF-8"))
              ) ++ registers.getOrElse(Map())
              (addressOpt orElse walletVars.publicKeyAddresses.headOption)
                .fold[Try[ErgoAddress]](Failure(new Exception("No address available for box locking")))(Success(_))
                .map { lockWithAddress =>
                  def minimalErgoAmount: Long =
                    BoxUtils.minimalErgoAmountSimulated(
                      lockWithAddress.script,
                      Colls.fromItems((Digest32 @@ assetId) -> amount),
                      nonMandatoryRegisters,
                      parameters
                    )
                  new ErgoBoxCandidate(
                    valueOpt.getOrElse(minimalErgoAmount),
                    lockWithAddress.script,
                    fullHeight,
                    Colls.fromItems((Digest32 @@ assetId) -> amount),
                    nonMandatoryRegisters
                  )
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
    * Generates new unsigned transaction according to given requests using stored or provided boxes.
    *
    * @param requests      - requests to transfer funds or to issue an asset
    * @param inputsRaw     - user-provided inputs. If empty then wallet is looking for inputs itself. If non-empty, then
    *                      the wallet is not adding anything, thus the user in this case should take care about satisfying
    *                      the (sum(inputs) == sum(outputs)) preservation rule for ergs.
    * @param dataInputsRaw - user-provided data (read-only) inputs. Wallet is not able to figure out needed data inputs
    *                      (to spend the spendable inputs).
    * @return generated transaction along with its inputs and data-inputs, or an error
    */
  private def generateUnsignedTransaction(requests: Seq[TransactionGenerationRequest],
                                          inputsRaw: Seq[String],
                                          dataInputsRaw: Seq[String]): Try[(UnsignedErgoTransaction, IndexedSeq[ErgoBox], IndexedSeq[ErgoBox])] = Try {

    val userInputs = stringsToBoxes(inputsRaw)
    val dataInputs = stringsToBoxes(dataInputsRaw).toIndexedSeq

    val (preInputBoxes, filter, changeAddressOpt: Option[ProveDlog]) = if (userInputs.nonEmpty) {
      //inputs are provided externally, no need for filtering
      (boxesToFakeTracked(userInputs), noFilter, None)
    } else {
      walletVars.proverOpt match {
        case Some(_) =>
          //inputs are to be selected by the wallet
          require(walletVars.publicKeyAddresses.nonEmpty, "No public keys in the prover to extract change address from")
          val boxesToSpend = (registry.walletUnspentBoxes() ++ offChainRegistry.offChainBoxes).distinct
          (boxesToSpend.toIterator, walletFilter, changeAddress.map(_.pubkey))

        case None =>
          throw new Exception(s"Cannot generateTransactionWithOutputs($requests, $inputsRaw): wallet is locked")
      }
    }

    //We're getting id of the first input, it will be used in case of asset issuance (asset id == first input id)
    val firstInput = preInputBoxes.next()
    val assetId = firstInput.box.id
    val inputBoxes = Iterator(firstInput) ++ preInputBoxes

    requestsToBoxCandidates(requests, assetId).flatMap { outputs =>
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

      val selectionOpt = boxSelector.select(inputBoxes, filter, targetBalance, targetAssets)

      selectionOpt.map { selectionResult =>
        prepareTransaction(outputs, selectionResult, dataInputs, changeAddressOpt) -> selectionResult.boxes
      } match {
        case Right((txTry, inputs)) => txTry.map(tx => (tx, inputs.map(_.box).toIndexedSeq, dataInputs))
        case Left(e) => Failure(
          new Exception(s"Failed to find boxes to assemble a transaction for $outputs, \nreason: $e")
        )
      }
    }
  }.flatten

  def generateSignedTransaction(requests: Seq[TransactionGenerationRequest],
                                inputsRaw: Seq[String],
                                dataInputsRaw: Seq[String]): Try[ErgoTransaction] = {
    generateUnsignedTransaction(requests, inputsRaw, dataInputsRaw).flatMap { case (unsignedTx, inputs, dataInputs) =>
      walletVars.proverOpt match {
        case Some(prover) =>
          signTransaction(prover, unsignedTx, inputs, dataInputs, stateContext)
        case None =>
          Failure(new Exception(s"Cannot sign the transaction $unsignedTx, wallet locked or not initialized"))
      }
    }
  }

  private def prepareTransaction(payTo: Seq[ErgoBoxCandidate],
                                 selectionResult: BoxSelectionResult[TrackedBox],
                                 dataInputBoxes: IndexedSeq[ErgoBox],
                                 changeAddressOpt: Option[ProveDlog]): Try[UnsignedErgoTransaction] = Try {
    require(
      selectionResult.changeBoxes.isEmpty || changeAddressOpt.isDefined,
      "Does not have change address to send change to"
    )

    val dataInputs = dataInputBoxes.map(dataInputBox => DataInput(dataInputBox.id))
    val changeBoxCandidates = selectionResult.changeBoxes.map { changeBox =>
      val assets = changeBox.tokens.map(t => Digest32 @@ idToBytes(t._1) -> t._2).toIndexedSeq
      new ErgoBoxCandidate(changeBox.value, changeAddressOpt.get, walletHeight(), assets.toColl)
    }
    val inputBoxes = selectionResult.boxes.toIndexedSeq
    new UnsignedErgoTransaction(
      inputBoxes.map(_.box.id).map(id => new UnsignedInput(id)),
      dataInputs,
      (payTo ++ changeBoxCandidates).toIndexedSeq
    )
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
    secretStorage.secret match {

      case None => throw new Exception("Master key is not available after unlock")

      case Some(masterKey) =>

        // first, we're trying to find in the database paths written by clients prior 3.3.0 and convert them
        // into a new format (pubkeys with paths stored instead of paths)
        val oldPaths = storage.readPaths()
        if (oldPaths.nonEmpty) {
          val oldDerivedSecrets = masterKey +: oldPaths.map {
            path => masterKey.derive(path).asInstanceOf[ExtendedSecretKey]
          }
          val oldPubKeys = oldDerivedSecrets.map(_.publicKey)
          oldPubKeys.foreach(storage.addKey)
          storage.removePaths()
        }

        // now we read previously stored, or just stored during the conversion procedure above, public keys
        var pubKeys = storage.readAllKeys().toIndexedSeq

        //If no public keys in the database yet, add master's public key into it
        if (pubKeys.isEmpty) {
          if (walletSettings.usePreEip3Derivation) {
            // If usePreEip3Derivation flag is set in the wallet settings, the first key is the master key
            val masterPubKey = masterKey.publicKey
            storage.addKey(masterPubKey)
            pubKeys = scala.collection.immutable.IndexedSeq(masterPubKey)
          } else {
            // If no usePreEip3Derivation flag is set, add first derived key (for m/44'/429'/0'/0/0) to the db
            val firstSk = nextKey(masterKey).result.map(_._3).toOption
            val firstPk = firstSk.map(_.publicKey)
            firstPk.foreach { pk =>
              storage.addKey(pk)
              storage.updateChangeAddress(P2PKAddress(pk.key))
            }

            pubKeys = firstPk.toIndexedSeq
          }
        }

        // Secrets corresponding to public keys
        val secretsPk = pubKeys.map { pk =>
          val path = pk.path.toPrivateBranch
          masterKey.derive(path).asInstanceOf[ExtendedSecretKey]
        }

        // If no master key in the secrets corresponding to public keys,
        // add master key so then it is not available to the user but presents in the prover
        val secrets = if (secretsPk.headOption.contains(masterKey)) {
          secretsPk
        } else {
          masterKey +: secretsPk
        }
        val prover = ErgoProvingInterpreter(secrets, parameters)
        walletVars = walletVars.withProver(prover)
    }
  } match {
    case Success(_) =>
    case Failure(t) =>
      log.error("Unlock failed: ", t)
  }

/*
  private def inputsFor(targetAmount: Long,
                        targetAssets: TokensMap = Map.empty): Seq[ErgoBox] = {
    val unspentBoxes = registry.walletUnspentBoxes()
    boxSelector
      .select(unspentBoxes.toIterator, walletFilter, targetAmount, targetAssets)
      .toSeq
      .flatMap(_.boxes)
      .map(_.box)
  }*/

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
    DerivationPath.nextPath(secrets, walletSettings.usePreEip3Derivation)
  }

  // call nextPath and derive next key from it
  private def nextKey(masterKey: ExtendedSecretKey): DeriveNextKeyResult = {
    val derivationResult = nextPath().flatMap { path =>
      val secret = masterKey.derive(path).asInstanceOf[ExtendedSecretKey]
      processSecretAddition(secret).map { _ =>
        (path, P2PKAddress(secret.publicKey.key), secret)
      }
    }
    DeriveNextKeyResult(derivationResult)
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

  /**
    * Generate new transaction fulfilling given requests
    *
    * @param requests
    * @param inputsRaw
    * @param dataInputsRaw
    * @param sign
    */
  final case class GenerateTransaction(requests: Seq[TransactionGenerationRequest],
                                       inputsRaw: Seq[String],
                                       dataInputsRaw: Seq[String],
                                       sign: Boolean)

  /**
    * Request to generate commitments for an unsigned transaction
    *
    * @param utx - unsigned transaction
    * @param secrets - optionally, externally provided secrets
    * @param inputsOpt - optionally, externally provided inputs
    * @param dataInputsOpt - optionally, externally provided inputs
    */
  case class GenerateCommitmentsFor(utx: UnsignedErgoTransaction,
                                    secrets: Option[Seq[ExternalSecret]],
                                    inputsOpt: Option[Seq[ErgoBox]],
                                    dataInputsOpt: Option[Seq[ErgoBox]])

  /**
    * Response for commitments generation request
    *
    * @param response - hints to sign a transaction
    */
  case class GenerateCommitmentsResponse(response: Try[TransactionHintsBag])

  /**
    * A request to sign a transaction
    *
    * @param utx - unsigned transaction
    * @param secrets - additional secrets given to the prover
    * @param hints - hints used for transaction signing (commitments and partial proofs)
    * @param boxesToSpend - boxes the transaction is spending
    * @param dataBoxes - read-only inputs of the transaction
    */
  case class SignTransaction(utx: UnsignedErgoTransaction,
                             secrets: Seq[ExternalSecret],
                             hints: TransactionHintsBag,
                             boxesToSpend: Option[Seq[ErgoBox]],
                             dataBoxes: Option[Seq[ErgoBox]])


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
    * @param unspentOnly         - return only unspent boxes
    * @param considerUnconfirmed - consider mempool (filter our unspent boxes spent in the pool if unspent = true, add
    *                            boxes created in the pool for both values of unspentOnly).
    */
  final case class GetWalletBoxes(unspentOnly: Boolean, considerUnconfirmed: Boolean)

  /**
    * Get boxes related to a scan
    *
    * @param scanId - scan identifier
    * @param unspentOnly - return only unspent boxes
    * @param considerUnconfirmed - consider mempool (filter our unspent boxes spent in the pool if unspent = true, add
    *                            boxes created in the pool for both values of unspentOnly).
    */
  final case class GetScanBoxes(scanId: ScanId, unspentOnly: Boolean, considerUnconfirmed: Boolean)

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
    * Result of "deriveNextKey" operation
    */
  case class DeriveNextKeyResult(result: Try[(DerivationPath, P2PKAddress, ExtendedSecretKey)])

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

  /**
    * A request to extract hints from given transaction
    *
    * @param tx           - transaction to extract hints from
    * @param real         - public keys corresponing to the secrets known
    * @param simulated    - public keys to simulate
    * @param inputsOpt - optionally, externally provided inputs
    * @param dataInputsOpt - optionally, externally provided inputs
    */
  case class ExtractHints(tx: ErgoTransaction,
                          real: Seq[SigmaBoolean],
                          simulated: Seq[SigmaBoolean],
                          inputsOpt: Option[Seq[ErgoBox]],
                          dataInputsOpt: Option[Seq[ErgoBox]])

  /**
    * Result of hints generation operation
    *
    * @param transactionHintsBag - hints for transaction
    */
  case class ExtractHintsResult(transactionHintsBag: TransactionHintsBag)


  /**
    * Add association between a scan and a box (and add the box to the database if it is not there)
    *
    * @param box
    * @param scanIds
    *
    */
  case class AddBox(box: ErgoBox, scanIds: Set[ScanId])

  /**
    * Wrapper for a result of AddBox processing
    *
    * @param status
    */
  case class AddBoxResponse(status: Try[Unit])

  def signTransaction(proverOpt: Option[ErgoProvingInterpreter],
                      tx: UnsignedErgoTransaction,
                      secrets: Seq[ExternalSecret],
                      hints: TransactionHintsBag,
                      boxesToSpend: Seq[ErgoBox],
                      dataBoxes: Seq[ErgoBox],
                      parameters: Parameters,
                      stateContext: ErgoStateContext): Try[ErgoTransaction] = {
    val proverSecrets = proverOpt.map(_.secretKeys).getOrElse(Seq.empty)
    val secretsWrapped = secrets.map(_.key).toIndexedSeq
    val secretsProver = ErgoProvingInterpreter(secretsWrapped ++ proverSecrets, parameters)
    secretsProver
      .sign(tx, boxesToSpend.toIndexedSeq, dataBoxes.toIndexedSeq, stateContext, hints)
      .map(ErgoTransaction.apply)
  }

  def signTransaction(prover: ErgoProvingInterpreter,
                      unsignedTx: UnsignedErgoTransaction,
                      inputBoxes: IndexedSeq[ErgoBox],
                      dataInputBoxes: IndexedSeq[ErgoBox],
                      stateContext: ErgoLikeStateContext): Try[ErgoTransaction] = {
    prover.sign(unsignedTx, inputBoxes, dataInputBoxes, stateContext, TransactionHintsBag.empty)
      .map(ErgoTransaction.apply)
      .fold(
        e => Failure(new Exception(s"Failed to sign boxes due to ${e.getMessage}: $inputBoxes", e)),
        tx => Success(tx))
  }

  // A helper which is deserializing Base16-encoded boxes to ErgoBox instances
  def stringsToBoxes(strings: Seq[String]): Seq[ErgoBox] =
    strings.map(in => Base16.decode(in).flatMap(ErgoBoxSerializer.parseBytesTry)).map(_.get)

}
