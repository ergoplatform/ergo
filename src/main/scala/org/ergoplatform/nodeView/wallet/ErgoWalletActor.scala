package org.ergoplatform.nodeView.wallet

import java.io.File
import java.util

import akka.actor.Actor
import io.iohk.iodb.LSMStore
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
import org.ergoplatform.wallet.secrets.{ExtendedSecretKey, JsonSecretStorage}
import scorex.core.VersionTag
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.ChangedState
import scorex.core.utils.ScorexEncoding
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}
import sigmastate.Values.{ByteArrayConstant, IntConstant}
import sigmastate.interpreter.ContextExtension

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

  private val walletSettings: WalletSettings = settings.walletSettings

  private val storage: WalletStorage = {
    val dir = new File(s"${settings.directory}/wallet/storage")
    dir.mkdirs()
    new WalletStorage(new LSMStore(dir), settings)
  }

  private val registry: WalletRegistry = {
    val dir = new File(s"${settings.directory}/wallet/registry")
    dir.mkdirs()
    new WalletRegistry(new LSMStore(dir))
  }

  private var offChainRegistry: OffChainRegistry = OffChainRegistry.empty(height)

  private val parameters: Parameters = LaunchParameters

  // State context used to sign transactions and check that coins found in the blockchain are indeed belonging
  // to the wallet (by executing testing transactions against them). The state context is being updating by listening
  // to state updates.
  private def stateContext: ErgoStateContext = storage.readStateContext

  private def height = storage.readHeight

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ChangedState[_]])
    walletSettings.testMnemonic match {
      case Some(testMnemonic) =>
        log.warn("Initializing wallet in test mode. Switch to secure mode for production usage.")
        val seed = Mnemonic.toSeed(testMnemonic)
        val rootSk = ExtendedSecretKey.deriveMasterKey(seed)
        val childSks = walletSettings.testKeysQty.toIndexedSeq.flatMap(x => (0 until x).map(rootSk.child))
        proverOpt = Some(ErgoProvingInterpreter((rootSk +: childSks).map(_.key), parameters))
        storage.addTrackedAddresses(proverOpt.toSeq.flatMap(_.pubKeys.map(pk => P2PKAddress(pk))))
      case None =>
        log.info("Trying to read wallet in secure mode ..")
        readSecretStorage.fold(
          e => log.info(
            s"Failed to read wallet. Manual initialization is required to sign transactions. Cause: ${e.getCause}"),
          _ => log.info("Wallet loaded successfully")
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
      val (resolved, _) = outputs
        .filterNot(o => inputs.contains(encodedId(o.id)))
        .partition(resolve)
      val resolvedTrackedBoxes = resolved.map { bx =>
        TrackedBox(tx.id, bx.index, None, None, None, bx, BoxCertainty.Certain)
      }

      offChainRegistry = offChainRegistry.updated(resolvedTrackedBoxes, inputs)

    case ScanOnChain(block) =>
      storage.updateHeight(block.header.height)
      val (outputs, inputs) = block.transactions
        .foldLeft(Seq.empty[(ModifierId, ErgoBox)], Seq.empty[(ModifierId, EncodedBoxId)]) {
          case ((outAcc, inAcc), tx) =>
            (outAcc ++ extractOutputs(tx).map(tx.id -> _), inAcc ++ extractInputs(tx).map(tx.id -> _))
        }
      if (outputs.nonEmpty || inputs.nonEmpty) {
        if (proverOpt.isDefined) {
          processBlock(block.id, block.height, inputs, outputs)
        } else {
          // save wallet-critical data from block to process it later.
          val postponedBlock = PostponedBlock(block.id, block.height, inputs, outputs)
          storage.putBlock(postponedBlock)
        }
      }

    case Rollback(version: VersionTag, height: Int) =>
      // remove postponed blocks which were rolled back.
      storage.readLatestPostponedBlockHeight.foreach { h =>
        (height to h).foreach(storage.removeBlock)
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
      sender() ! registry.readCertainBoxes.map(_.box).toIterator

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
      val entropy = scorex.utils.Random.randomBytes(settings.walletSettings.seedStrengthBits / 8)
      val mnemonicTry = new Mnemonic(walletSettings.mnemonicPhraseLanguage, walletSettings.seedStrengthBits)
        .toMnemonic(entropy)
        .map { mnemonic =>
          val secretStorage = JsonSecretStorage
            .init(Mnemonic.toSeed(mnemonic, mnemonicPassOpt), pass)(settings.walletSettings.secretStorage)
          secretStorageOpt = Some(secretStorage)
          mnemonic
        }
      util.Arrays.fill(entropy, 0: Byte)

      sender() ! mnemonicTry

    case RestoreWallet(mnemonic, passOpt, encryptionPass) if secretStorageOpt.isEmpty =>
      val secretStorage = JsonSecretStorage
        .restore(mnemonic, passOpt, encryptionPass)(settings.walletSettings.secretStorage)
      secretStorageOpt = Some(secretStorage)
      sender() ! Success(())

    case RestoreWallet | InitWallet(_, _) =>
      sender() ! Failure(new Exception("Wallet is already initialized"))

    case UnlockWallet(pass) =>
      secretStorageOpt match {
        case Some(secretStorage) =>
          sender() ! secretStorage.unlock(pass)
          proverOpt = Some(ErgoProvingInterpreter(secretStorage.secret.map(_.key).toIndexedSeq, parameters))
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
  }

  private def publicKeys: Seq[P2PKAddress] = proverOpt.toSeq.flatMap(_.pubKeys.map(P2PKAddress.apply))

  private def trackedAddresses: Seq[ErgoAddress] = storage.readTrackedAddresses

  //we currently do not use off-chain boxes to create a transaction
  private def filterFn(trackedBox: TrackedBox): Boolean = trackedBox.chainStatus.onChain

  private def resolve(box: ErgoBox): Boolean = {
    val testingTx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(box.id)),
      IndexedSeq(new ErgoBoxCandidate(1L, Constants.TrueLeaf, creationHeight = height))
    )

    val transactionContext = TransactionContext(IndexedSeq(box), IndexedSeq(), testingTx, selfIndex = 0)
    val context = new ErgoContext(stateContext, transactionContext, ContextExtension.empty)

    proverOpt.flatMap(_.prove(box.ergoTree, context, testingTx.messageToSign).toOption).isDefined
  }

  private def extractOutputs(tx: ErgoTransaction): Seq[ErgoBox] = {
    val trackedBytes: Seq[Array[Byte]] = trackedAddresses.map(_.contentBytes)
    tx.outputs.filter(bx => trackedBytes.exists(t => bx.propositionBytes.containsSlice(t)))
  }

  private def extractInputs(tx: ErgoTransaction): Seq[EncodedBoxId] = tx.inputs.map(x => encodedId(x.boxId))

  private def requestsToBoxCandidates(requests: Seq[TransactionRequest]): Try[Seq[ErgoBoxCandidate]] = Try {
    requests.map {
      case PaymentRequest(address, value, assets, registers) =>
        new ErgoBoxCandidate(value, address.script, height, assets.getOrElse(Seq.empty), registers.getOrElse(Map.empty))
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
          BoxUtils.minimalErgoAmountSimulated(lockWithAddress.script, Seq(assetId -> amount), nonMandatoryRegisters, parameters)
        new ErgoBoxCandidate(minimalErgoAmount, lockWithAddress.script, height, Seq(assetId -> amount), nonMandatoryRegisters)
      case other => throw new Exception(s"Unknown TransactionRequest type: $other")
    }
  }

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
              val boxTokens = bx.additionalTokens.map(t => bytesToId(t._1) -> t._2).toMap
              AssetUtils.mergeAssets(boxTokens, acc)
            }

          boxSelector.select(
            registry.readCertainBoxes.toIterator, filterFn, targetBalance, targetAssets).map { r =>
            val inputs = r.boxes.toIndexedSeq

            val changeAddress = prover.pubKeys(Random.nextInt(prover.pubKeys.size))

            val changeBoxCandidates = r.changeBoxes.map { case (ergChange, tokensChange) =>
              val assets = tokensChange.map(t => Digest32 @@ idToBytes(t._1) -> t._2).toIndexedSeq
              new ErgoBoxCandidate(ergChange, changeAddress, height, assets)
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

  private def processBlock(id: ModifierId,
                           height: Int,
                           inputs: Seq[(ModifierId, EncodedBoxId)],
                           outputs: Seq[(ModifierId, ErgoBox)]): Unit = {
    proverOpt.foreach(_.IR.resetContext())
    val prevUncertainBoxes = registry.readUncertainBoxes
    val (resolved, unresolved) = (outputs ++ prevUncertainBoxes.map(b => b.creationTxId -> b.box))
      .filterNot { case (_, o) => inputs.map(_._2).contains(encodedId(o.id)) }
      .partition { case (_, o) => resolve(o) }
    val resolvedTrackedBoxes = resolved.map { case (txId, bx) =>
      TrackedBox(txId, bx.index, Some(height), None, None, bx, BoxCertainty.Certain)
    }
    val unresolvedTrackedBoxes = unresolved.map { case (txId, bx) =>
      TrackedBox(txId, bx.index, Some(height), None, None, bx, BoxCertainty.Uncertain)
    }

    registry.updateOnBlock(
      resolvedTrackedBoxes, unresolvedTrackedBoxes, inputs.map(_._2))(id, height)

    val newOnChainIds = (resolvedTrackedBoxes ++ unresolvedTrackedBoxes).map(x => encodedId(x.box.id))
    offChainRegistry = offChainRegistry.updateOnBlock(height, registry.readCertainBoxes, newOnChainIds)
  }

  private def inputsFor(targetAmount: Long,
                        targetAssets: Map[ModifierId, Long] = Map.empty): Seq[ErgoBox] =
    boxSelector
      .select(registry.readCertainBoxes.toIterator, filterFn, targetAmount, targetAssets)
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

  final case object LockWallet

  final case object ReadRandomPublicKey

  final case object ReadTrackedAddresses

  final case object GetFirstSecret

  final case object GetBoxes

}
