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

import scala.util.{Failure, Random, Success, Try}

class ErgoWalletActor(settings: ErgoSettings, boxSelector: BoxSelector)
  extends Actor
    with ScorexLogging
    with ScorexEncoding {

  import cats.implicits._

  import ErgoWalletActor._
  import IdUtils._

  private implicit val addressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(settings.chainSettings.addressPrefix)


  private val walletSettings: WalletSettings = settings.walletSettings
  private val parameters: Parameters = LaunchParameters

  private var proverOpt: Option[ErgoProvingInterpreter] = None
  private var secretStorageOpt: Option[JsonSecretStorage] = None

  private var offChainRegistry: OffChainRegistry = OffChainRegistry.empty
  private val storage: WalletStorage = settings.walletStorage
  private val registry: WalletRegistry = settings.walletRegistry

  private val externalApplications = storage.allApplications
  private val trackingRules = externalApplications.map(_.trackingRule)


  // State context used to sign transactions and check that coins found in the blockchain are indeed belonging
  // to the wallet (by executing testing transactions against them). The state context is being updated by listening
  // to state updates.
  private def stateContext: ErgoStateContext = storage.readStateContext

  private def height: Int = stateContext.currentHeight

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ChangedState[_]])
    walletSettings.testMnemonic match {
      case Some(testMnemonic) =>
        log.warn("Initializing wallet in test mode. Switch to secure mode for production usage.")
        val seed = Mnemonic.toSeed(testMnemonic)
        val rootSk = ExtendedSecretKey.deriveMasterKey(seed)
        val childSks = walletSettings.testKeysQty.toIndexedSeq.flatMap(x => (0 until x).map(rootSk.child))
        proverOpt = Some(ErgoProvingInterpreter(rootSk +: childSks, parameters))
        storage.addTrackedAddresses(proverOpt.toSeq.flatMap(_.pubKeys.map(pk => P2PKAddress(pk))))
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
    walletCommands orElse
      onStateChanged orElse
      scanLogic orElse
      readers

  private def scanLogic: Receive = {
    //scan mempool transaction
    case ScanOffChain(tx) =>
      val resolvedTrackedBoxes = extractWalletOutputs(tx)._1
      val inputs = extractAllInputs(tx)
      offChainRegistry = offChainRegistry.updated(resolvedTrackedBoxes, inputs)

    //scan block transactions
    case ScanOnChain(block) =>
      //extract wallet-related outputs and all the inputs from all the transactions in the block
      val (walletAndAppsTrackedBoxes, allInputs) = block.transactions
        .foldLeft((Seq.empty[TrackedBox], Seq.empty[TrackedBox]), Seq.empty[(ModifierId, EncodedBoxId)]) {
          case ((outAcc, inAcc), tx) =>
            val outputs = extractWalletOutputs(tx)
            val inputs = extractAllInputs(tx)
            ((outAcc._1 ++ outputs._1) -> (outAcc._2 ++ outputs._2), inAcc ++ inputs.map(tx.id -> _))
        }

      val walletTrackedBoxes = walletAndAppsTrackedBoxes._1
      val walletOutputs = walletTrackedBoxes.map(tb => tb.creationTxId -> tb.box)

      val outIds = registry.readAllBoxes.map(x => encodedBoxId(x.box.id)) ++
        walletOutputs.map(x => encodedBoxId(x._2.id))

      //leave only spent inputs
      val walletInputs = allInputs.filter(x => outIds.contains(x._2))
      val walletTxIds = walletInputs.map(_._1) ++ walletOutputs.map(_._1)
      val walletTxs = block.transactions.filter(tx => walletTxIds.contains(tx.id))



    case Rollback(version: VersionTag, height: Int) =>
      // remove postponed blocks which were rolled back.
      storage.readLatestPostponedBlockHeight.foreach { latestHeight =>
        storage.removeBlocks(height, latestHeight)
      }
      registry.rollback(version).fold(
        e => log.error(s"Failed to rollback wallet registry to version $version due to: $e"), _ => ())
  }

  private def readers: Receive = {
    case ReadBalances(chainStatus) =>
      sender() ! (if (chainStatus.onChain) registry.readIndex else offChainRegistry.readIndex)

    case ReadPublicKeys(from, until) =>
      sender() ! publicKeys.slice(from, until)

    case GetFirstSecret =>
      if (proverOpt.nonEmpty) {
        proverOpt.foreach(_.secrets.headOption.foreach(s => sender() ! Success(s)))
      } else {
        sender() ! Failure(new Exception("Wallet is locked"))
      }

    case GetBoxes(unspent) =>
      val currentHeight = height
      sender() ! (if (unspent) registry.readCertainUnspentBoxes else registry.readCertainBoxes)
        .map(tb => WalletBox(tb, tb.inclusionHeightOpt.map(currentHeight - _)))
        .sortBy(_.trackedBox.inclusionHeightOpt)

    case GetTransactions =>
      sender() ! registry.readTransactions
        .sortBy(-_.inclusionHeight)
        .map(tx => AugWalletTransaction(tx, height - tx.inclusionHeight))

    case GetTransaction(id) =>
      sender() ! registry.getTransactionById(id)
        .map(tx => AugWalletTransaction(tx, height - tx.inclusionHeight))

    case ReadRandomPublicKey =>
      sender() ! publicKeys(Random.nextInt(publicKeys.size))

    case ReadTrackedAddresses =>
      sender() ! trackedAddresses.toIndexedSeq
  }

  private def onStateChanged: Receive = {
    case ChangedState(s: ErgoStateReader@unchecked) =>
      storage.updateStateContext(s.stateContext)
  }

  private def walletCommands: Receive = {
    case InitWallet(pass, mnemonicPassOpt) if secretStorageOpt.isEmpty =>
      //Read high-quality random bits from Java's SecureRandom
      val entropy = scorex.utils.Random.randomBytes(settings.walletSettings.seedStrengthBits / 8)
      val mnemonicTry = new Mnemonic(walletSettings.mnemonicPhraseLanguage, walletSettings.seedStrengthBits)
        .toMnemonic(entropy)
        .map { mnemonic =>
          val secretStorage = JsonSecretStorage
            .init(Mnemonic.toSeed(mnemonic, mnemonicPassOpt), pass)(settings.walletSettings.secretStorage)
          secretStorageOpt = Some(secretStorage)
          mnemonic
        }
        .fold(catchLegalExc, res => Success(res))

      util.Arrays.fill(entropy, 0: Byte)
      sender() ! mnemonicTry
      self ! UnlockWallet(pass)
      log.info("Wallet is initialized")

    case RestoreWallet(mnemonic, passOpt, encryptionPass) if secretStorageOpt.isEmpty =>
      val secretStorage = JsonSecretStorage
        .restore(mnemonic, passOpt, encryptionPass)(settings.walletSettings.secretStorage)
      secretStorageOpt = Some(secretStorage)
      sender() ! Success(())
      self ! UnlockWallet(encryptionPass)
      log.info("Wallet is restored")

    case _: RestoreWallet | _: InitWallet =>
      sender() ! Failure(new Exception("Wallet is already initialized. Clear keystore to re-init it."))

    case UnlockWallet(pass) =>
      secretStorageOpt match {
        case Some(secretStorage) =>
          sender() ! secretStorage.unlock(pass)
          processUnlock(secretStorage)
        case None =>
          sender() ! Failure(new Exception("Wallet not initialized"))
      }

    case LockWallet =>
      proverOpt = None
      secretStorageOpt.foreach(_.lock())

    case GetLockStatus =>
      sender() ! (secretStorageOpt.isDefined -> proverOpt.isDefined)

    case WatchFor(address) =>
      storage.addTrackedAddress(address)

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

  private def publicKeys: Seq[P2PKAddress] = proverOpt.toSeq.flatMap(_.pubKeys.map(P2PKAddress.apply))

  private def trackedAddresses: Seq[ErgoAddress] = storage.readTrackedAddresses

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
    * Tries to prove given box in order to define whether it could be spent by this wallet.
    */
  private def resolve(box: ErgoBox): Boolean = {
    val testingTx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(box.id)),
      IndexedSeq(new ErgoBoxCandidate(1L, Constants.TrueLeaf, creationHeight = height))
    )

    val transactionContext = TransactionContext(IndexedSeq(box), IndexedSeq(), testingTx, selfIndex = 0)
    val context = new ErgoContext(stateContext, transactionContext, ContextExtension.empty,
      parameters.maxBlockCost, CostTable.interpreterInitCost)

    proverOpt.flatMap(_.prove(box.ergoTree, context, testingTx.messageToSign).toOption).isDefined
  }

  /**
    * Extracts all outputs which contain tracked bytes from the given transaction.
    */
  private def extractWalletOutputs(tx: ErgoTransaction): (Seq[TrackedBox], Seq[TrackedBox]) = {
    val trackedBytes: Seq[Array[Byte]] = trackedAddresses.map(_.script.bytes)
    val walletBoxes = tx.outputs.filter { bx =>
      trackedBytes.exists(bs => bx.propositionBytes.sameElements(bs))
    }.map(bx => TrackedBox(tx.id, bx.index, None, None, None, bx, BoxCertainty.Certain, Constants.DefaultAppId))

    val appBoxes = tx.outputs.flatMap { bx =>
      val appsTriggered = externalApplications.filter(_.trackingRule.filter(bx))
      appsTriggered.map(app => TrackedBox(tx, bx.index, None, bx, BoxCertainty.Uncertain, app.appId.toShort))
    }
    (walletBoxes, appBoxes)
  }

  /**
    * Extracts all inputs from the given transaction.
    */
  private def extractAllInputs(tx: ErgoTransaction): Seq[EncodedBoxId] = tx.inputs.map(x => encodedBoxId(x.boxId))

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
                  (addressOpt orElse publicKeys.headOption)
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
        TrackedBox(box.transactionId, box.index, Some(1), None, None, box, BoxCertainty.Certain, Constants.DefaultAppId)
      }
      .toIterator
  }

  /**
    * Generates new transaction according to a given requests using available boxes.
    */
  private def generateTransactionWithOutputs(requests: Seq[TransactionRequest],
                                             inputsRaw: Seq[String]): Try[ErgoTransaction] =
    proverOpt match {
      case Some(prover) =>
        Traverse[List]
          .sequence {
            inputsRaw
              .toList
              .map(in => Base16.decode(in).flatMap(ErgoBoxSerializer.parseBytesTry))
          }
          .flatMap { inputs =>
            requestsToBoxCandidates(requests).flatMap { payTo =>
              require(prover.pubKeys.nonEmpty, "No public keys in the prover to extract change address from")
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
                (registry.readCertainUnspentBoxes.toIterator, onChainFilter)
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
        Failure(new Exception("Wallet is locked"))
    }

  private def prepareTransaction(prover: ErgoProvingInterpreter, payTo: Seq[ErgoBoxCandidate])
                                (r: BoxSelector.BoxSelectionResult): Try[ErgoLikeTransaction] = {
    val inputs = r.boxes.toIndexedSeq

    val changeAddress = storage.readChangeAddress
      .map(_.pubkey)
      .getOrElse {
        log.warn("Change address not specified. Using random address from wallet.")
        prover.pubKeys(Random.nextInt(prover.pubKeys.size))
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
      .fold(e => Failure(new Exception(s"Failed to sign boxes: $inputs", e)), tx => Success(tx))
  }

  /**
    * Updates indexes according to a given wallet-critical data.
    */
  private def processBlock(id: ModifierId,
                           height: Int,
                           inputs: Seq[(ModifierId, EncodedBoxId)],
                           outputs: Seq[(ModifierId, ErgoBox)],
                           txs: Seq[ErgoTransaction]): Unit = {
    // re-create interpreter in order to avoid IR context bloating.
    proverOpt = proverOpt.map(oldInterpreter => ErgoProvingInterpreter(oldInterpreter.secretKeys, parameters))
    val prevUncertainBoxes = registry.readUncertainBoxes
    val (resolved, unresolved) = (outputs ++ prevUncertainBoxes.map(b => b.creationTxId -> b.box))
      .filterNot { case (_, o) => inputs.map(_._2).contains(encodedBoxId(o.id)) }
      .partition { case (_, o) => resolve(o) }
    val resolvedTrackedBoxes = resolved.map { case (txId, bx) =>
      TrackedBox(txId, bx.index, Some(height), None, None, bx, BoxCertainty.Certain, Constants.DefaultAppId)
    }
    val unresolvedTrackedBoxes = unresolved.map { case (txId, bx) =>
      TrackedBox(txId, bx.index, Some(height), None, None, bx, BoxCertainty.Uncertain, Constants.DefaultAppId)
    }

    log.info(
      s"Processing ${resolved.size} resolved boxes: [${resolved.map(_._2.id).mkString(", ")}], " +
      s"${unresolved.size} unresolved boxes: [${unresolved.map(_._2.id).mkString(", ")}]."
    )

    val walletTxs = txs.map(WalletTransaction(_, height, Constants.DefaultAppId))

    registry.updateOnBlock(resolvedTrackedBoxes, unresolvedTrackedBoxes, inputs, walletTxs)(id, height)

    val newOnChainIds = (resolvedTrackedBoxes ++ unresolvedTrackedBoxes).map(x => encodedBoxId(x.box.id))
    offChainRegistry = offChainRegistry.updateOnBlock(height, registry.readCertainUnspentBoxes, newOnChainIds)
  }

  private def processBlock(id: ModifierId,
                           height: Int,
                           txs: Seq[ErgoTransaction]): Unit = {
    val (outputs, inputs) = txs
      .foldLeft(Seq.empty[(ModifierId, ErgoBox)], Seq.empty[(ModifierId, EncodedBoxId)]) {
        case ((outAcc, inAcc), tx) =>
          (outAcc ++ extractWalletOutputs(tx).map(tx.id -> _), inAcc ++ extractAllInputs(tx).map(tx.id -> _))
      }
    processBlock(id, height, inputs, outputs, txs)
  }

  private def processSecretAddition(secret: ExtendedSecretKey): Unit =
    proverOpt.foreach { prover =>
      log.info(s"New secret created, public image: ${Base16.encode(secret.publicKey.keyBytes)}")
      val secrets = proverOpt.toIndexedSeq.flatMap(_.secretKeys) :+ secret
      proverOpt = Some(new ErgoProvingInterpreter(secrets, parameters)(prover.IR))
      storage.addTrackedAddress(P2PKAddress(secret.publicKey.key))
      storage.addPath(secret.path)
    }

  private def processUnlock(secretStorage: JsonSecretStorage): Unit = {
    val secrets = secretStorage.secret.toIndexedSeq ++ storage.readPaths.flatMap { path =>
      secretStorage.secret.toSeq.map(_.derive(path).asInstanceOf[ExtendedSecretKey])
    }
    proverOpt = Some(ErgoProvingInterpreter(secrets, parameters))
    storage.addTrackedAddresses(proverOpt.toSeq.flatMap(_.pubKeys.map(pk => P2PKAddress(pk))))
    // process postponed blocks when prover is available.
    val lastProcessedHeight = registry.readIndex.height
    storage.readLatestPostponedBlockHeight.foreach { lastPostponedHeight =>
      val blocks = storage.readBlocks(lastProcessedHeight, lastPostponedHeight)
      blocks.sortBy(_.height).foreach { case PostponedBlock(id, height, txs) =>
        processBlock(id, height, txs)
      }
      // remove processed blocks from the storage.
      storage.removeBlocks(lastProcessedHeight, lastPostponedHeight)
    }
  }

  private def inputsFor(targetAmount: Long,
                        targetAssets: Map[ModifierId, Long] = Map.empty): Seq[ErgoBox] =
    boxSelector
      .select(registry.readCertainUnspentBoxes.toIterator, onChainFilter, targetAmount, targetAssets)
      .toSeq
      .flatMap(_.boxes)

  private def readSecretStorage: Try[JsonSecretStorage] = {
    val dir = new File(settings.walletSettings.secretStorage.secretDir)
    if (dir.exists()) {
      dir.listFiles().toList match {
        case files if files.size > 1 =>
          Failure(new Exception("Ambiguous secret files"))
        case headFile :: _ =>
          Success(new JsonSecretStorage(headFile, settings.walletSettings.secretStorage.encryption))
        case Nil =>
          Failure(new Exception("Secret file not found"))
      }
    } else {
      Failure(new Exception("Secret dir does not exist"))
    }
  }

  private def catchLegalExc[T](e: Throwable): Failure[T] =
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
    def nextPath(accPath: List[Int], rem: Seq[List[Int]]): Try[DerivationPath] = {
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

    val secrets = proverOpt.toIndexedSeq.flatMap(_.secretKeys)
    if (secrets.size == 1) {
      Success(DerivationPath(List(0, 1), publicBranch = false))
    } else {
      nextPath(List.empty, secrets.map(_.path.decodedPath.tail))
    }
  }

}

object ErgoWalletActor {

  final case class WatchFor(address: ErgoAddress)

  final case class ScanOffChain(tx: ErgoTransaction)

  final case class ScanOnChain(block: ErgoFullBlock)

  final case class Rollback(version: VersionTag, height: Int)

  final case class GenerateTransaction(requests: Seq[TransactionRequest], inputsRaw: Seq[String])

  final case class ReadBalances(chainStatus: ChainStatus)

  final case class ReadPublicKeys(from: Int, until: Int)

  final case class InitWallet(pass: String, mnemonicPassOpt: Option[String])

  final case class RestoreWallet(mnemonic: String, passOpt: Option[String], encryptionPass: String)

  final case class UnlockWallet(pass: String)

  final case class DeriveKey(path: String)

  final case class GetBoxes(unspentOnly: Boolean)

  final case class UpdateChangeAddress(address: P2PKAddress)

  final case class GetTransaction(id: ModifierId)

  case object GetTransactions

  case object DeriveNextKey

  case object LockWallet

  case object GetLockStatus

  case object ReadRandomPublicKey

  case object ReadTrackedAddresses

  case object GetFirstSecret

}
