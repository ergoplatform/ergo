package org.ergoplatform.nodeView.wallet

import java.io.File
import java.util

import akka.actor.{Actor, ActorRef}
import cats.Traverse
import org.ergoplatform.ErgoBox._
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader}
import org.ergoplatform.nodeView.wallet.persistence._
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest, TransactionRequest}
import org.ergoplatform.nodeView.wallet.scanning.{ExternalAppRequest, ExternalApplication}
import org.ergoplatform.nodeView.wallet.scanning.ExternalApplication.AppId
import org.ergoplatform.settings._
import org.ergoplatform.utils.{AssetUtils, BoxUtils}
import org.ergoplatform.wallet.boxes.{BoxCertainty, BoxSelector, ChainStatus, TrackedBox}
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.protocol.context.TransactionContext
import org.ergoplatform.wallet.secrets.{DerivationPath, ExtendedSecretKey, Index, JsonSecretStorage}
import scorex.core.VersionTag
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.ChangedState
import scorex.core.utils.ScorexEncoding
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}
import sigmastate.Values.{ByteArrayConstant, IntConstant}
import sigmastate.eval.Extensions._
import sigmastate.eval._
import sigmastate.interpreter.ContextExtension
import sigmastate.utxo.CostTable
import org.ergoplatform.wallet.Constants.{MiningRewardsQueueId, PaymentsAppId}

import scala.util.{Failure, Random, Success, Try}

class ErgoWalletActor(settings: ErgoSettings, boxSelector: BoxSelector)
  extends Actor
    with ScorexLogging
    with ScorexEncoding {

  import cats.implicits._

  import ErgoWalletActor._
  import IdUtils._

  private val walletSettings: WalletSettings = settings.walletSettings
  //todo: update parameters
  private val parameters: Parameters = LaunchParameters

  private var secretStorageOpt: Option[JsonSecretStorage] = None

  private val storage: WalletStorage = settings.walletStorage
  private val registry: WalletRegistry = settings.walletRegistry
  private var offChainRegistry: OffChainRegistry = OffChainRegistry.init(registry)

  private var walletVars = WalletVars.initial(storage, settings)

  private implicit val ergoAddressEncoder = walletVars.addressEncoder


  // State context used to sign transactions and check that coins found in the blockchain are indeed belonging
  // to the wallet (by executing testing transactions against them).
  // The state context is being updated by listening to state updates.
  private def stateContext: ErgoStateContext = storage.readStateContext

  private def height: Int = stateContext.currentHeight


  /**
    * Extracts all outputs which contain tracked bytes from the given transaction.
    */
  private def extractWalletOutputs(tx: ErgoTransaction,
                                   inclusionHeight: Option[Int],
                                   walletVars: WalletVars): Seq[TrackedBox] = {

    val trackedBytes: Seq[Array[Byte]] = walletVars.trackedBytes
    val miningScriptsBytes: Seq[Array[Byte]] = walletVars.miningScriptsBytes
    val externalApplications: Seq[ExternalApplication] = walletVars.externalApplications

    tx.outputs.flatMap { bx =>
      val appsTriggered = externalApplications.filter(_.trackingRule.filter(bx))
        .map(app => app.appId -> app.initialCertainty)
        .toMap

      val miningIncomeTriggered = miningScriptsBytes.exists(ms => bx.propositionBytes.sameElements(ms))

      //tweak for tests
      lazy val miningStatus: (AppId, BoxCertainty) = if (settings.chainSettings.monetary.minerRewardDelay > 0) {
        MiningRewardsQueueId -> BoxCertainty.Certain
      } else {
        PaymentsAppId -> BoxCertainty.Certain
      }

      val prePaymentStatuses = if (miningIncomeTriggered) appsTriggered + miningStatus else appsTriggered

      val statuses: Map[AppId, BoxCertainty] = if (prePaymentStatuses.nonEmpty) {
        prePaymentStatuses
      } else {
        val paymentsTriggered = trackedBytes.exists(bs => bx.propositionBytes.sameElements(bs))

        if (paymentsTriggered) {
          Map(PaymentsAppId -> BoxCertainty.Certain)
        } else {
          Map.empty
        }
      }

      if (statuses.nonEmpty) {
        val tb = TrackedBox(tx.id, bx.index, inclusionHeight, None, None, bx, statuses)
        log.debug("New tracked box: " + tb.boxId)
        Some(tb)
      } else {
        None
      }
    }
  }

  /**
    * Extracts all inputs from the given transaction.
    */
  private def extractAllInputs(tx: ErgoTransaction): Seq[EncodedBoxId] = tx.inputs.map(x => encodedBoxId(x.boxId))

  private def scanBlockTransactions(blockId: ModifierId, transactions: Seq[ErgoTransaction]): Unit = {

    //todo: replace with Bloom filter?
    val previousBoxIds = registry.walletUnspentBoxes().map(tb => encodedBoxId(tb.box.id))

    val resolvedBoxes = registry.uncertainBoxes(MiningRewardsQueueId).flatMap { tb =>
      //todo: more efficient resolving, just by height
      if (resolve(tb.box)) Some(tb.copy(applicationStatuses = Map(PaymentsAppId -> BoxCertainty.Certain))) else None
    }

    //outputs, input ids, related transactions
    type ScanResults = (Seq[TrackedBox], Seq[(ModifierId, EncodedBoxId, TrackedBox)], Seq[WalletTransaction])
    val initialScanResults: ScanResults = (resolvedBoxes, Seq.empty, Seq.empty)

    val scanRes = transactions.foldLeft((initialScanResults, previousBoxIds)) { case ((scanResults, accBoxIds), tx) =>
      val txInputIds = tx.inputs.map(x => encodedBoxId(x.boxId))
      val outputs = extractWalletOutputs(tx, Some(height), walletVars)

      val boxIds: Seq[EncodedBoxId] = accBoxIds ++ outputs.map(x => EncodedBoxId @@ x.boxId)
      val relatedInputIds = txInputIds.filter(x => boxIds.contains(x))

      if (outputs.nonEmpty || relatedInputIds.nonEmpty) {
        val spentBoxes = relatedInputIds.map { inpId =>
          registry.getBox(decodedBoxId(inpId))
            .orElse(scanResults._1.find(tb => tb.box.id.sameElements(decodedBoxId(inpId)))).get //todo: .get
        }
        val walletAppIds = (spentBoxes ++ outputs).flatMap(_.applicationStatuses.keys).toSet
        val wtx = WalletTransaction(tx, height, walletAppIds.toSeq)

        val newRel = (scanResults._2: Seq[(ModifierId, EncodedBoxId, TrackedBox)]) ++
          relatedInputIds.zip(spentBoxes).map(t => (tx.id, t._1, t._2))
        (scanResults._1 ++ outputs, newRel, scanResults._3 :+ wtx) -> boxIds
      } else {
        scanResults -> accBoxIds
      }
    }._1

    val outputs = scanRes._1
    val inputs = scanRes._2
    val affectedTransactions = scanRes._3

    // function effects: updating registry and offchainRegistry datasets
    registry.updateOnBlock(outputs, inputs, affectedTransactions)(blockId, height)
    val walletUnspent = registry.walletUnspentBoxes()
    val newOnChainIds = outputs.map(x => encodedBoxId(x.box.id))
    offChainRegistry = offChainRegistry.updateOnBlock(height, walletUnspent, newOnChainIds)
  }

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ChangedState[_]])
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
        readSecretStorage.fold(
          e => log.info(
            s"Failed to read wallet. Manual initialization is required to sign transactions. Cause: ${e.getCause}"),
          secretStorage => {
            log.info("Wallet loaded successfully")
            secretStorageOpt = Some(secretStorage)
          }
        )
    }
  }

  override def receive: Receive =
    walletInit orElse
      walletCommands orElse
      onStateChanged orElse
      scanLogic orElse
      readers

  private def scanLogic: Receive = {
    //scan mempool transaction
    case ScanOffChain(tx) =>
      val resolvedTrackedBoxes = extractWalletOutputs(tx, None, walletVars)
      val inputs = extractAllInputs(tx)
      offChainRegistry = offChainRegistry.updated(resolvedTrackedBoxes, inputs)

    //scan block transactions
    case ScanOnChain(block) =>
      scanBlockTransactions(block.id, block.transactions)

    case Rollback(version: VersionTag) =>
      registry.rollback(version).fold(
        e => log.error(s"Failed to rollback wallet registry to version $version due to: $e"), _ => ())
  }

  private def readers: Receive = {
    case ReadBalances(chainStatus) =>
      sender() ! (if (chainStatus.onChain) registry.fetchDigest() else offChainRegistry.digest)

    case ReadPublicKeys(from, until) =>
      sender() ! walletVars.publicKeys.slice(from, until)

    case GetFirstSecret =>
      if (walletVars.proverOpt.nonEmpty) {
        walletVars.proverOpt.foreach(_.secrets.headOption.foreach(s => sender() ! Success(s)))
      } else {
        sender() ! Failure(new Exception("Wallet is locked"))
      }

    //todo: returns only confirmed boxes now is it okay?
    case GetWalletBoxes(unspent) =>
      val currentHeight = height
      sender() ! (if (unspent) registry.walletUnspentBoxes() else registry.walletConfirmedBoxes(0))
        .map(tb => WalletBox(tb, tb.inclusionHeightOpt.map(currentHeight - _)))
        .sortBy(_.trackedBox.inclusionHeightOpt)

    case GetAppBoxes(appId, unspent) =>
      val currentHeight = height
      sender() ! (if (unspent) registry.unspentBoxes(appId) else registry.confirmedBoxes(appId, 0))
        .map(tb => WalletBox(tb, tb.inclusionHeightOpt.map(currentHeight - _)))
        .sortBy(_.trackedBox.inclusionHeightOpt)

    case GetUncertainBoxes(appId) =>
      val currentHeight = height
      sender() ! registry.uncertainBoxes(appId)
        .map(tb => WalletBox(tb, tb.inclusionHeightOpt.map(currentHeight - _)))
        .sortBy(_.trackedBox.inclusionHeightOpt)

    case GetTransactions =>
      sender() ! registry.allWalletTxs()
        .sortBy(-_.inclusionHeight)
        .map(tx => AugWalletTransaction(tx, height - tx.inclusionHeight))

    case GetTransaction(id) =>
      sender() ! registry.getTx(id)
        .map(tx => AugWalletTransaction(tx, height - tx.inclusionHeight))

    case ReadRandomPublicKey =>
      val publicKeys = walletVars.publicKeys
      sender() ! publicKeys(Random.nextInt(publicKeys.size))

    case ReadApplications =>
      sender() ! walletVars.externalApplications
  }

  private def onStateChanged: Receive = {
    case ChangedState(s: ErgoStateReader@unchecked) =>
      storage.updateStateContext(s.stateContext)
  }

  //Secret is set in form of keystore file of testMnemonic in the config
  private def secretIsSet: Boolean = secretStorageOpt.nonEmpty || walletSettings.testMnemonic.nonEmpty

  private def walletInit: Receive = {
    //Init wallet (w. mnemonic generation) if secret is not set yet
    case InitWallet(pass, mnemonicPassOpt) if !secretIsSet =>
      //Read high-quality random bits from Java's SecureRandom
      val entropy = scorex.utils.Random.randomBytes(settings.walletSettings.seedStrengthBits / 8)
      val mnemonicTry = new Mnemonic(walletSettings.mnemonicPhraseLanguage, walletSettings.seedStrengthBits)
        .toMnemonic(entropy)
        .map { mnemonic =>
          val secretStorage = JsonSecretStorage
            .init(Mnemonic.toSeed(mnemonic, mnemonicPassOpt), pass)(settings.walletSettings.secretStorage)
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
    case UnlockWallet(pass) =>
      secretStorageOpt match {
        case Some(secretStorage) =>
          sender() ! secretStorage.unlock(pass)
          processUnlock(secretStorage)
        case None =>
          sender() ! Failure(new Exception("Wallet not initialized"))
      }

    case LockWallet =>
      walletVars = walletVars.resetProver()
      secretStorageOpt.foreach(_.lock())

    case GetLockStatus =>
      sender() ! (secretIsSet -> walletVars.proverOpt.isDefined)

    case GenerateTransaction(requests, inputsRaw) =>
      sender() ! generateTransactionWithOutputs(requests, inputsRaw)

    case DeriveKey(encodedPath) =>
      withWalletLockHandler(sender()) {
        _.secret.foreach { rootSecret =>
          DerivationPath.fromEncoded(encodedPath).foreach {
            case path if !path.publicBranch =>
              val secret = rootSecret.derive(path).asInstanceOf[ExtendedSecretKey]
              processSecretAddition(secret)
              sender() ! Success(P2PKAddress(secret.publicKey.key))
            case path =>
              sender() ! Failure(new Exception(
                s"A private path is expected, but the public one given: $path"))
          }
        }
      }

    case DeriveNextKey =>
      withWalletLockHandler(sender()) {
        _.secret.foreach { rootSecret =>
          sender() ! nextPath.map { path =>
            val secret = rootSecret.derive(path).asInstanceOf[ExtendedSecretKey]
            processSecretAddition(secret)
            path -> P2PKAddress(secret.publicKey.key)
          }
        }
      }

    case UpdateChangeAddress(address) =>
      storage.updateChangeAddress(address)

    case RemoveApplication(appId) =>
      val res = Try(storage.removeApplication(appId))
      res.foreach(app => walletVars = walletVars.removeApplication(appId))
      sender() ! res

    case AddApplication(appRequest) =>
      val res = storage.addApplication(appRequest)
      res.foreach(app => walletVars = walletVars.addApplication(app))
      sender() ! res


    case MakeCertain(appId: AppId, boxId: BoxId) =>
      sender() ! registry.makeCertain(appId, boxId)

    case StopTracking(appId: AppId, boxId: BoxId) =>
      sender() ! registry.removeApp(appId, boxId)
  }

  private def withWalletLockHandler(callback: ActorRef)
                                   (body: JsonSecretStorage => Unit): Unit =
    secretStorageOpt match {
      case Some(secretStorage) if !secretStorage.isLocked =>
        body(secretStorage)
      case Some(_) =>
        callback ! Failure(new Exception("Wallet is locked"))
      case None =>
        callback ! Failure(new Exception("Wallet is not initialized"))
    }

  private type FilterFn = TrackedBox => Boolean
  /**
    * This filter is selecting boxes which are onchain and not spent offchain yet.
    * This filter is used when wallet is looking through its boxes to assemble a transaction.
    */
  private val onChainFilter: FilterFn = (trackedBox: TrackedBox) => trackedBox.chainStatus.onChain &&
    offChainRegistry.onChainBalances.exists(_.id == encodedBoxId(trackedBox.box.id))

  /**
    * This filter is not filtering out anything, used when the wallet works with externally provided boxes.
    */
  private val noFilter: FilterFn = (_: TrackedBox) => true

  /**
    * Tries to prove the given box in order to define whether it could be spent by this wallet.
    */
  private def resolve(box: ErgoBox): Boolean = {
    val testingTx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(box.id)),
      IndexedSeq(new ErgoBoxCandidate(1L, Constants.TrueLeaf, creationHeight = height))
    )

    val transactionContext = TransactionContext(IndexedSeq(box), IndexedSeq(), testingTx, selfIndex = 0)
    val context = new ErgoContext(stateContext, transactionContext, ContextExtension.empty,
      parameters.maxBlockCost, CostTable.interpreterInitCost)

    walletVars.proverOpt.flatMap(_.prove(box.ergoTree, context, testingTx.messageToSign).toOption).isDefined
  }

  private def requestsToBoxCandidates(requests: Seq[TransactionRequest]): Try[Seq[ErgoBoxCandidate]] =
    Traverse[List].sequence {
      requests.toList
        .map {
          case PaymentRequest(address, value, assets, registers) =>
            Success(new ErgoBoxCandidate(value, address.script, height, assets.toColl, registers))
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
                    R6 -> IntConstant(decimals)
                  ) ++ registers.getOrElse(Map())
                  (addressOpt orElse walletVars.publicKeys.headOption)
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
                        height,
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
        TrackedBox(box.transactionId, box.index, Some(1), None, None, box, Map(PaymentsAppId -> BoxCertainty.Certain))
      }
      .toIterator
  }

  /**
    * Generates new transaction according to a given requests using available boxes.
    */
  private def generateTransactionWithOutputs(requests: Seq[TransactionRequest],
                                             inputsRaw: Seq[String]): Try[ErgoTransaction] =
    walletVars.proverOpt match {
      case Some(prover) =>
        Traverse[List]
          .sequence {
            inputsRaw
              .toList
              .map(in => Base16.decode(in).flatMap(ErgoBoxSerializer.parseBytesTry))
          }
          .flatMap { inputs =>
            requestsToBoxCandidates(requests).flatMap { payTo =>
              require(walletVars.publicKeys.nonEmpty, "No public keys in the prover to extract change address from")
              require(requests.count(_.isInstanceOf[AssetIssueRequest]) <= 1, "Too many asset issue requests")
              require(payTo.forall(c => c.value >= BoxUtils.minimalErgoAmountSimulated(c, parameters)), "Minimal ERG value not met")
              require(payTo.forall(_.additionalTokens.forall(_._2 >= 0)), "Negative asset value")

              val assetIssueBox = payTo
                .zip(requests)
                .filter(_._2.isInstanceOf[AssetIssueRequest])
                .map(_._1)
                .headOption

              val targetBalance = payTo
                .map(_.value)
                .sum

              val targetAssets = payTo
                .filterNot(bx => assetIssueBox.contains(bx))
                .foldLeft(Map.empty[ModifierId, Long]) { case (acc, bx) =>
                  // TODO optimize: avoid toArray and use mapFirst
                  val boxTokens = bx.additionalTokens.toArray.map(t => bytesToId(t._1) -> t._2).toMap
                  AssetUtils.mergeAssets(boxTokens, acc)
                }

              val (inputBoxes, filter) = if (inputs.nonEmpty) {
                //inputs are provided externally, no need for filtering
                (boxesToFakeTracked(inputs), noFilter)
              } else {
                //inputs are to be selected by the wallet
                (registry.walletUnspentBoxes().toIterator, onChainFilter)
              }

              val selectionOpt = boxSelector.select(inputBoxes, filter, targetBalance, targetAssets)

              val makeTx = prepareTransaction(prover, payTo) _

              selectionOpt.map(makeTx) match {
                case Some(txTry) => txTry.map(ErgoTransaction.apply)
                case None => Failure(new Exception(s"No enough boxes to assemble a transaction for $payTo"))
              }
            }
          }

      case None =>
        Failure(new Exception(s"Cannot generateTransactionWithOutputs($requests, $inputsRaw): Wallet is locked"))
    }

  private def prepareTransaction(prover: ErgoProvingInterpreter, payTo: Seq[ErgoBoxCandidate])
                                (r: BoxSelector.BoxSelectionResult): Try[ErgoLikeTransaction] = {
    val inputs = r.boxes.toIndexedSeq

    val changeAddress = storage.readChangeAddress
      .map(_.pubkey)
      .getOrElse {
        log.warn("Change address not specified. Using root address from wallet.")
        prover.pubKeys.head
      }

    val changeBoxCandidates = r.changeBoxes.map { case (ergChange, tokensChange) =>
      val assets = tokensChange.map(t => Digest32 @@ idToBytes(t._1) -> t._2).toIndexedSeq
      new ErgoBoxCandidate(ergChange, changeAddress, height, assets.toColl)
    }

    val unsignedTx = new UnsignedErgoTransaction(
      inputs.map(_.id).map(id => new UnsignedInput(id)),
      IndexedSeq(),
      (payTo ++ changeBoxCandidates).toIndexedSeq
    )

    prover.sign(unsignedTx, inputs, IndexedSeq(), stateContext)
      .fold(e => Failure(new Exception(s"Failed to sign boxes due to ${e.getMessage}: $inputs", e)), tx => Success(tx))
  }

  private def processSecretAddition(secret: ExtendedSecretKey): Unit =
    walletVars.proverOpt.foreach { prover =>
      log.info(s"New secret created, public image: ${Base16.encode(secret.publicKey.keyBytes)}")
      val secrets = prover.secretKeys :+ secret
      val updProver = new ErgoProvingInterpreter(secrets, parameters)(prover.IR)
      walletVars = walletVars.withProver(updProver)
      storage.addPath(secret.path)
    }

  private def processUnlock(secretStorage: JsonSecretStorage): Unit = {
    val secrets = secretStorage.secret.toIndexedSeq ++ storage.readPaths.flatMap { path =>
      secretStorage.secret.toSeq.map(sk => sk.derive(path).asInstanceOf[ExtendedSecretKey])
    }
    walletVars = walletVars.withProver(ErgoProvingInterpreter(secrets, parameters))
  }

  private def inputsFor(targetAmount: Long,
                        targetAssets: Map[ModifierId, Long] = Map.empty): Seq[ErgoBox] = {
    val unspentBoxes = registry.walletUnspentBoxes()
    boxSelector
      .select(unspentBoxes.toIterator, onChainFilter, targetAmount, targetAssets)
      .toSeq
      .flatMap(_.boxes)
  }

  private def readSecretStorage: Try[JsonSecretStorage] = {
    val dir = new File(settings.walletSettings.secretStorage.secretDir)
    if (dir.exists()) {
      dir.listFiles().toList match {
        case files if files.size > 1 =>
          Failure(new Exception(s"Ambiguous secret files in dir '$dir'"))
        case headFile :: _ =>
          Success(new JsonSecretStorage(headFile, settings.walletSettings.secretStorage.encryption))
        case Nil =>
          Failure(new Exception(s"Cannot readSecretStorage: Secret file not found in dir '$dir'"))
      }
    } else {
      Failure(new Exception(s"Cannot readSecretStorage: Secret dir '$dir' doesn't exist"))
    }
  }

  private def wrapLegalExc[T](e: Throwable): Failure[T] =
    if (e.getMessage.startsWith("Illegal key size")) {
      val dkLen = settings.walletSettings.secretStorage.encryption.dkLen
      Failure[T](new Exception(s"Key of length $dkLen is not allowed on your JVM version." +
        s"Set `ergo.wallet.secretStorage.encryption.dkLen = 128` or update JVM"))
    } else {
      Failure[T](e)
    }

  /**
    * Finds next available path index for a new key.
    */
  private def nextPath: Try[DerivationPath] = {
    @scala.annotation.tailrec
    def nextPath(accPath: List[Int], rem: Seq[Seq[Int]]): Try[DerivationPath] = {
      if (!rem.forall(_.isEmpty)) {
        val maxChildIdx = rem.flatMap(_.headOption).max
        if (!Index.isHardened(maxChildIdx)) {
          Success(DerivationPath(0 +: (accPath :+ maxChildIdx + 1), publicBranch = false))
        } else {
          nextPath(accPath :+ maxChildIdx, rem.map(_.drop(1)))
        }
      } else {
        Failure(
          new Exception("Out of non-hardened index space. Try to derive hardened key specifying path manually"))
      }
    }

    val secrets = walletVars.proverOpt.toIndexedSeq.flatMap(_.secretKeys)
    if (secrets.size == 1) {
      Success(DerivationPath(Array(0, 1), publicBranch = false))
    } else {
      nextPath(List.empty, secrets.map(_.path.decodedPath.tail.toList))
    }
  }

}

object ErgoWalletActor {

  /**
    * Inner class of the wallet actor which it encapsulating its mutable state (aside of the databases
    * the actor modifies). The main intention behind the class is to make modifications of this part of the internal
    * state explicit and unit-testable.
    *
    *
    * @param proverOpt
    * @param externalApplications
    *
    * @param settings
    */
  case class WalletVars(proverOpt: Option[ErgoProvingInterpreter],
                        externalApplications: Seq[ExternalApplication])(implicit val settings: ErgoSettings) {

    private[wallet] implicit val addressEncoder: ErgoAddressEncoder =
      ErgoAddressEncoder(settings.chainSettings.addressPrefix)

    val publicKeys: Seq[P2PKAddress] = proverOpt.toSeq.flatMap(_.pubKeys.map(P2PKAddress.apply))

    val miningScriptsBytes: Seq[Array[Byte]] = proverOpt.toSeq.flatMap(_.pubKeys).map(pk =>
      ErgoScriptPredef.rewardOutputScript(settings.chainSettings.monetary.minerRewardDelay, pk)
    ).map(_.bytes)

    val trackedBytes: Seq[Array[Byte]] = proverOpt.toSeq.flatMap(_.pubKeys).map(_.propBytes.toArray)

    def resetProver(): WalletVars = this.copy(proverOpt = None)

    def withProver(prover: ErgoProvingInterpreter): WalletVars = this.copy(proverOpt = Some(prover))

    def removeApplication(appId: AppId): WalletVars =
      this.copy(externalApplications = this.externalApplications.filter(_.appId != appId))

    def addApplication(app: ExternalApplication): WalletVars =
      this.copy(externalApplications = this.externalApplications :+ app)
  }

  object WalletVars {
    def initial(storage: WalletStorage, settings: ErgoSettings): WalletVars = WalletVars(None, storage.allApplications)(settings)
  }

  final case class WatchFor(address: ErgoAddress)

  final case class ScanOffChain(tx: ErgoTransaction)

  final case class ScanOnChain(block: ErgoFullBlock)

  final case class Rollback(version: VersionTag)

  final case class GenerateTransaction(requests: Seq[TransactionRequest], inputsRaw: Seq[String])

  final case class ReadBalances(chainStatus: ChainStatus)

  final case class ReadPublicKeys(from: Int, until: Int)

  final case class InitWallet(pass: String, mnemonicPassOpt: Option[String])

  final case class RestoreWallet(mnemonic: String, passOpt: Option[String], encryptionPass: String)

  final case class UnlockWallet(pass: String)

  final case class DeriveKey(path: String)

  final case class GetWalletBoxes(unspentOnly: Boolean)

  final case class GetAppBoxes(appId: AppId, unspentOnly: Boolean)

  final case class GetUncertainBoxes(appId: AppId)

  final case class UpdateChangeAddress(address: P2PKAddress)

  final case class AddApplication(appRequest: ExternalAppRequest)

  final case class RemoveApplication(appId: AppId)

  final case class GetTransaction(id: ModifierId)

  case object GetTransactions

  case object DeriveNextKey

  case object LockWallet

  case object GetLockStatus

  case object GetFirstSecret

  case object ReadRandomPublicKey

  case object ReadApplications

  case class MakeCertain(appId: AppId, boxId: BoxId)

  case class StopTracking(appId: AppId, boxId: BoxId)

}
