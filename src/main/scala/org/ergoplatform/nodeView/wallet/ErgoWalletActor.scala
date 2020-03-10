package org.ergoplatform.nodeView.wallet

import java.io.File
import java.util

import akka.actor.{Actor, ActorRef}
import cats.Traverse
import com.github.oskin1.scakoo.immutable.CuckooFilter
import org.ergoplatform.ErgoBox._
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoBoxSerializer, ErgoTransaction, UnsignedErgoTransaction}
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
import org.ergoplatform.wallet.Constants.PaymentsAppId
import sigmastate.Values
import sigmastate.basics.DLogProtocol

import scala.concurrent.Future
import scala.util.{Failure, Random, Success, Try}

class ErgoWalletActor(settings: ErgoSettings, boxSelector: BoxSelector)
  extends Actor
    with ScorexLogging
    with ScorexEncoding {

  import cats.implicits._

  import ErgoWalletActor._
  import IdUtils._

  private implicit val ec = scala.concurrent.ExecutionContext.global

  private val walletSettings: WalletSettings = settings.walletSettings
  //todo: update parameters
  private val parameters: Parameters = LaunchParameters

  private var secretStorageOpt: Option[JsonSecretStorage] = None
  private implicit val ergoAddressEncoder: ErgoAddressEncoder = settings.addressEncoder

  private val storage: WalletStorage = WalletStorage.readOrCreate(settings)

  private var registry: WalletRegistry = WalletRegistry.readOrCreate(settings)
  private var offChainRegistry: OffChainRegistry = OffChainRegistry.init(registry)

  private var walletVars = WalletVars.initial(storage, settings)


  // State context used to sign transactions and check that coins found in the blockchain are indeed belonging
  // to the wallet (by executing testing transactions against them).
  // The state context is being updated by listening to state updates.
  private def stateContext: ErgoStateContext = storage.readStateContext

  private def height: Int = stateContext.currentHeight

  /**
    * Extracts all inputs from the given transaction.
    */
  private def extractAllInputs(tx: ErgoTransaction): Seq[EncodedBoxId] = tx.inputs.map(x => encodedBoxId(x.boxId))

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
      val resolvedTrackedBoxes = WalletScanLogic.extractWalletOutputs(tx, None, walletVars)
      val inputs = extractAllInputs(tx)
      offChainRegistry = offChainRegistry.updateOnTransaction(resolvedTrackedBoxes, inputs)

    //scan block transactions
    case ScanOnChain(block) =>
      val (reg, offReg) = WalletScanLogic.scanBlockTransactions(registry, offChainRegistry, stateContext, walletVars,
        height, block.id, block.transactions)
      registry = reg
      offChainRegistry = offReg

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
      val publicKeys = walletVars.publicKeyAddresses
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
          val unlockResult = secretStorage.unlock(pass)
          unlockResult match {
            case Success(_) =>
              log.info("Starting wallet unlock")
              Future {
                processUnlock(secretStorage)
                log.info("Wallet unlock finished")
              }
            case Failure(t) => log.warn("Wallet unlock failed with: ", t)
          }
          sender() ! unlockResult
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
          sender() ! nextPath().map { path =>
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
              require(walletVars.publicKeyAddresses.nonEmpty, "No public keys in the prover to extract change address from")
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

  private def processSecretAddition(secret: ExtendedSecretKey): Unit = {
    walletVars = walletVars.withNewSecret(secret)
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
  private def nextPath(): Try[DerivationPath] = {
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

    val secrets: IndexedSeq[ExtendedSecretKey] = walletVars.proverOpt.toIndexedSeq.flatMap(_.secretKeys)

    if (secrets.size == 1) {
      Success(DerivationPath(Array(0, 1), publicBranch = false))
    } else {
      nextPath(List.empty, secrets.map(_.path.decodedPath.tail.toList))
    }
  }

}

object ErgoWalletActor {

  //fields of WalletVars which are potentially costly to compute
  case class MutableStateCache(publicKeyAddresses: Seq[P2PKAddress],
                               trackedPubKeys: Seq[DLogProtocol.ProveDlog],
                               trackedBytes: Seq[Array[Byte]],
                               filter: CuckooFilter[Array[Byte]])

  /**
    * Inner class of the wallet actor which it encapsulating its mutable state (aside of the databases
    * the actor modifies). The main intention behind the class is to make modifications of this part of the internal
    * state explicit and unit-testable.
    *
    * @param proverOpt
    * @param externalApplications
    * @param stateCacheOpt
    * @param settings
    */
  case class WalletVars(proverOpt: Option[ErgoProvingInterpreter],
                        externalApplications: Seq[ExternalApplication],
                        stateCacheOpt: Option[MutableStateCache] = None)
                       (implicit val settings: ErgoSettings) extends ScorexLogging {

    //strategy for Cuckoo filter
    import com.github.oskin1.scakoo.TaggingStrategy.MurmurHash3Strategy

    private[wallet] implicit val addressEncoder: ErgoAddressEncoder =
      ErgoAddressEncoder(settings.chainSettings.addressPrefix)

    //this is constant actually, it is here to avoid passing settings to resolving methods
    val minerRewardDelay: Int = settings.chainSettings.monetary.minerRewardDelay

    val trackedPubKeys: Seq[DLogProtocol.ProveDlog] = stateCacheOpt.map(_.trackedPubKeys).getOrElse {
      proverOpt.toSeq.flatMap(_.pubKeys)
    }

    val publicKeyAddresses: Seq[P2PKAddress] = stateCacheOpt.map(_.publicKeyAddresses).getOrElse {
      trackedPubKeys.map(P2PKAddress.apply)
    }

    val trackedBytes: Seq[Array[Byte]] = stateCacheOpt.map(_.trackedBytes).getOrElse {
      trackedPubKeys.map(_.propBytes.toArray)
    }

    // currently only one mining key supported
    val miningScripts: Seq[Values.ErgoTree] = proverOpt.toSeq.flatMap { prover =>
      prover.pubKeys.headOption.map { pk =>
        ErgoScriptPredef.rewardOutputScript(settings.chainSettings.monetary.minerRewardDelay, pk)
      }
    }

    val miningScriptsBytes: Seq[Array[Byte]] = miningScripts.map(_.bytes)

    /**
      * Cuckoo filter for scan the boxes efficiently
      */
    val filter: CuckooFilter[Array[Byte]] = stateCacheOpt.map(_.filter).getOrElse {
      val entriesPerBucket = settings.walletSettings.keysFilter.entriesPerBucket
      val bucketsQty = settings.walletSettings.keysFilter.bucketsQty
      val f = com.github.oskin1.scakoo.mutable.CuckooFilter[Array[Byte]](entriesPerBucket, bucketsQty)
      trackedBytes.foreach(bs => f.insert(bs))
      miningScriptsBytes.foreach(msb => f.insert(msb))
      CuckooFilter.recover(f.memTable, f.entriesCount, f.entriesPerBucket)
    }

    /**
      * Clear the prover along with its secrets
      *
      * @return updated WalletVars instance
      **/
    def resetProver(): WalletVars = this.copy(proverOpt = None)

    def withProver(prover: ErgoProvingInterpreter): WalletVars = this.copy(proverOpt = Some(prover))

    /**
      * Add new secret to the prover
      *
      * @param secret - secret to add to existing ones
      * @return
      */
    def withNewSecret(secret: ExtendedSecretKey): WalletVars = {
      proverOpt match {
        case Some(prover) =>
          val (updProver, newPk) = prover.withNewSecret(secret)
          val updAddresses: Seq[P2PKAddress] = publicKeyAddresses :+ P2PKAddress(newPk)
          val updTrackedPubKeys: Seq[DLogProtocol.ProveDlog] = trackedPubKeys :+ newPk
          val newPkBytes = newPk.propBytes.toArray
          val updTrackedBytes: Seq[Array[Byte]] = trackedBytes :+ newPkBytes
          val updFilter: CuckooFilter[Array[Byte]] = filter.insert(newPkBytes).get

          val updCache = MutableStateCache(updAddresses, updTrackedPubKeys, updTrackedBytes, updFilter)

          this.copy(proverOpt = Some(updProver), stateCacheOpt = Some(updCache))
        case None =>
          log.warn(s"Trying to add new secret, but prover is not initialized")
          this
      }
    }

    def removeApplication(appId: AppId): WalletVars =
      this.copy(externalApplications = this.externalApplications.filter(_.appId != appId))

    def addApplication(app: ExternalApplication): WalletVars =
      this.copy(externalApplications = this.externalApplications :+ app)
  }

  object WalletVars {
    def initial(storage: WalletStorage, settings: ErgoSettings): WalletVars =
      WalletVars(None, storage.allApplications)(settings)
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