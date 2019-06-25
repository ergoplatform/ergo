package org.ergoplatform.nodeView.wallet

import java.io.File
import java.util

import akka.actor.{Actor, ActorRef}
import org.ergoplatform.ErgoBox._
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
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

  import ErgoWalletActor._
  import IdUtils._

  private implicit val addressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(settings.chainSettings.addressPrefix)

  private var proverOpt: Option[ErgoProvingInterpreter] = None

  private var secretStorageOpt: Option[JsonSecretStorage] = None

  private var offChainRegistry: OffChainRegistry = OffChainRegistry.empty

  private val walletSettings: WalletSettings = settings.walletSettings

  private val storage: WalletStorage = WalletStorage.readOrCreate(settings)

  private val registry: WalletRegistry = WalletRegistry.readOrCreate(settings)

  private val parameters: Parameters = LaunchParameters

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
    case ScanOffChain(tx) =>
      val outputs = extractOutputs(tx)
      val inputs = extractInputs(tx)
      val resolved = outputs.filter(resolve)
      val resolvedTrackedBoxes = resolved.map { bx =>
        TrackedBox(tx.id, bx.index, None, None, None, bx, BoxCertainty.Certain)
      }

      offChainRegistry = offChainRegistry.updated(resolvedTrackedBoxes, inputs)

    case ScanOnChain(block) =>
      val (outputs, inputs) = block.transactions
        .foldLeft(Seq.empty[(ModifierId, ErgoBox)], Seq.empty[(ModifierId, EncodedBoxId)]) {
          case ((outAcc, inAcc), tx) =>
            (outAcc ++ extractOutputs(tx).map(tx.id -> _), inAcc ++ extractInputs(tx).map(tx.id -> _))
        }
      if (outputs.nonEmpty || inputs.nonEmpty) {
        if (proverOpt.isDefined) {
          processBlock(block.id, block.height, inputs, outputs)
        } else if (walletSettings.postponedScanning) {
          // save wallet-critical data from block to process it later.
          val postponedBlock = PostponedBlock(block.id, block.height, inputs, outputs)
          storage.putBlock(postponedBlock)
        }
      }

    case Rollback(version: VersionTag, height: Int) =>
      // remove postponed blocks which were rolled back.
      storage.readLatestPostponedBlockHeight.foreach { latestHeight =>
        (height to latestHeight).foreach(storage.removeBlock)
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

    case GetBoxes =>
      val currentHeight = height
      sender() ! registry.readCertainUnspentBoxes.map { tb =>
        WalletBox(tb, tb.inclusionHeightOpt.map(currentHeight - _))
      }

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

    case RestoreWallet(mnemonic, passOpt, encryptionPass) if secretStorageOpt.isEmpty =>
      val secretStorage = JsonSecretStorage
        .restore(mnemonic, passOpt, encryptionPass)(settings.walletSettings.secretStorage)
      secretStorageOpt = Some(secretStorage)
      sender() ! Success(())
      self ! UnlockWallet(encryptionPass)

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

    case WatchFor(address) =>
      storage.addTrackedAddress(address)

    case GenerateTransaction(requests) =>
      sender() ! generateTransactionWithOutputs(requests)

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

  private def onChainFilter(trackedBox: TrackedBox): Boolean = trackedBox.chainStatus.onChain

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
  private def extractOutputs(tx: ErgoTransaction): Seq[ErgoBox] = {
    val trackedBytes: Seq[Array[Byte]] = trackedAddresses.map(_.contentBytes)
    tx.outputs.filter(bx => trackedBytes.exists(t => bx.propositionBytes.containsSlice(t)))
  }

  /**
    * Extracts all inputs from the given transaction.
    */
  private def extractInputs(tx: ErgoTransaction): Seq[EncodedBoxId] = tx.inputs.map(x => encodedBoxId(x.boxId))

  private def requestsToBoxCandidates(requests: Seq[TransactionRequest]): Try[Seq[ErgoBoxCandidate]] = Try {
    requests.map {
      case PaymentRequest(address, value, assets, registers) =>
        new ErgoBoxCandidate(value, address.script, height, assets.getOrElse(Seq.empty).toColl, registers.getOrElse(Map.empty))
      case AssetIssueRequest(addressOpt, amount, name, description, decimals) =>
        val firstInput = inputsFor(
          requests
            .collect { case pr: PaymentRequest => pr.value }
            .sum
        ).headOption.getOrElse(throw new Exception("Can't issue asset with no inputs"))
        val assetId = Digest32 !@@ firstInput.id
        val nonMandatoryRegisters = scala.Predef.Map(
          R4 -> ByteArrayConstant(name.getBytes("UTF-8")),
          R5 -> ByteArrayConstant(description.getBytes("UTF-8")),
          R6 -> IntConstant(decimals)
        )
        val lockWithAddress = (addressOpt orElse publicKeys.headOption)
          .getOrElse(throw new Exception("No address available for box locking"))
        val minimalErgoAmount =
          BoxUtils.minimalErgoAmountSimulated(lockWithAddress.script, Colls.fromItems(assetId -> amount), nonMandatoryRegisters, parameters)
        new ErgoBoxCandidate(minimalErgoAmount, lockWithAddress.script, height, Colls.fromItems(assetId -> amount), nonMandatoryRegisters)
      case other => throw new Exception(s"Unknown TransactionRequest type: $other")
    }
  }

  /**
    * Generates new transaction according to a given requests using available boxes.
    */
  private def generateTransactionWithOutputs(requests: Seq[TransactionRequest]): Try[ErgoTransaction] =
    proverOpt match {
      case Some(prover) =>
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

          boxSelector.select(
            registry.readCertainUnspentBoxes.toIterator, onChainFilter, targetBalance, targetAssets).map { r =>
            val inputs = r.boxes.toIndexedSeq

            val changeAddress = prover.pubKeys(Random.nextInt(prover.pubKeys.size))

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
          } match {
            case Some(txTry) => txTry.map(ErgoTransaction.apply)
            case None => Failure(new Exception(s"No enough boxes to assemble a transaction for $payTo"))
          }
        }

      case None =>
        Failure(new Exception("Wallet is locked"))
    }

  /**
    * Updates indexes according to a given wallet-critical data.
    */
  private def processBlock(id: ModifierId,
                           height: Int,
                           inputs: Seq[(ModifierId, EncodedBoxId)],
                           outputs: Seq[(ModifierId, ErgoBox)]): Unit = {
    // re-create interpreter in order to avoid IR context bloating.
    proverOpt = proverOpt.map(oldInterpreter => ErgoProvingInterpreter(oldInterpreter.secretKeys, parameters))
    val prevUncertainBoxes = registry.readUncertainBoxes
    val (resolved, unresolved) = (outputs ++ prevUncertainBoxes.map(b => b.creationTxId -> b.box))
      .filterNot { case (_, o) => inputs.map(_._2).contains(encodedBoxId(o.id)) }
      .partition { case (_, o) => resolve(o) }
    val resolvedTrackedBoxes = resolved.map { case (txId, bx) =>
      TrackedBox(txId, bx.index, Some(height), None, None, bx, BoxCertainty.Certain)
    }
    val unresolvedTrackedBoxes = unresolved.map { case (txId, bx) =>
      TrackedBox(txId, bx.index, Some(height), None, None, bx, BoxCertainty.Uncertain)
    }

    registry.updateOnBlock(resolvedTrackedBoxes, unresolvedTrackedBoxes, inputs)(id, height)

    val newOnChainIds = (resolvedTrackedBoxes ++ unresolvedTrackedBoxes).map(x => encodedBoxId(x.box.id))
    offChainRegistry = offChainRegistry.updateOnBlock(height, registry.readCertainUnspentBoxes, newOnChainIds)
  }

  private def processSecretAddition(secret: ExtendedSecretKey): Unit =
    proverOpt.foreach { prover =>
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
      blocks.sortBy(_.height).foreach { case PostponedBlock(id, height, inputs, outputs) =>
        processBlock(id, height, inputs, outputs)
      }
      // remove processed blocks from storage.
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

  final case class GenerateTransaction(requests: Seq[TransactionRequest])

  final case class ReadBalances(chainStatus: ChainStatus)

  final case class ReadPublicKeys(from: Int, until: Int)

  final case class InitWallet(pass: String, mnemonicPassOpt: Option[String])

  final case class RestoreWallet(mnemonic: String, passOpt: Option[String], encryptionPass: String)

  final case class UnlockWallet(pass: String)

  final case class DeriveKey(path: String)

  case object DeriveNextKey

  case object LockWallet

  case object ReadRandomPublicKey

  case object ReadTrackedAddresses

  case object GetFirstSecret

  case object GetBoxes

}
