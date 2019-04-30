package org.ergoplatform.nodeView.wallet

import java.io.File

import akka.actor.Actor
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import org.ergoplatform.ErgoBox.{BoxId, R4, R5, R6}
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader}
import org.ergoplatform.nodeView.wallet.persistence.RegistryIndex
import org.ergoplatform.nodeView.wallet.persistence.RegistryOps._
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
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base16
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}
import sigmastate.Values.{ByteArrayConstant, IntConstant}
import sigmastate.interpreter.ContextExtension

import scala.collection.mutable
import scala.util.{Failure, Random, Success, Try}

class ErgoWalletActor(ergoSettings: ErgoSettings, boxSelector: BoxSelector)
  extends Actor
    with ScorexLogging
    with ScorexEncoding {

  import ErgoWalletActor._

  private implicit val addressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)

  private var proverOpt: Option[ErgoProvingInterpreter] = None

  private var secretStorageOpt: Option[JsonSecretStorage] = None

  // State context used to sign transactions and check that coins found in the blockchain are indeed belonging
  // to the wallet (by executing testing transactions against them). The state context is being updating by listening
  // to state updates.
  //todo: initialize it, e.g. by introducing StateInitialized signal in addition to StateChanged
  private var stateContext: ErgoStateContext =
    ErgoStateContext.empty(ADDigest @@ Array.fill(32)(0: Byte), ergoSettings)

  private var height = stateContext.currentHeight

  // todo: persist?
  private val trackedAddresses: mutable.Buffer[ErgoAddress] = publicKeys.toBuffer

  private val walletSettings: WalletSettings = ergoSettings.walletSettings

  private val __store = new WalletStorage

  private val registry: LSMStore = {
    val dir = new File(s"${ergoSettings.directory}/wallet/registry")
    dir.mkdirs()
    new LSMStore(dir)
  }

  private val parameters: Parameters = LaunchParameters

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ChangedState[_]])
    walletSettings.testMnemonic match {
      case Some(testMnemonic) =>
        log.warn("Initializing wallet in test mode. Switch to secure mode for production usage.")
        val seed = Mnemonic.toSeed(testMnemonic)
        val rootSk = ExtendedSecretKey.deriveMasterKey(seed)
        val childSks = walletSettings.testKeysQty.toIndexedSeq.flatMap(x => (0 until x).map(rootSk.child))
        proverOpt = Some(ErgoProvingInterpreter((rootSk +: childSks).map(_.key), parameters))
        proverOpt.foreach(_.pubKeys.foreach(pk => trackedAddresses.append(P2PKAddress(pk))))
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

  private def publicKeys: Seq[P2PKAddress] = proverOpt.toSeq.flatMap(_.pubKeys.map(P2PKAddress.apply))

  private def trackedBytes: Seq[Array[Byte]] = trackedAddresses.map(_.contentBytes)

  //we currently do not use off-chain boxes to create a transaction
  private def filterFn(trackedBox: TrackedBox): Boolean = trackedBox.chainStatus.onChain

  private def resolve(box: ErgoBox): Boolean = {
    val testingTx = UnsignedErgoLikeTransaction(
      IndexedSeq(new UnsignedInput(box.id)),
      IndexedSeq(new ErgoBoxCandidate(1L, Constants.TrueLeaf, creationHeight = height))
    )

    val transactionContext = TransactionContext(IndexedSeq(box), IndexedSeq(), testingTx, selfIndex = 0)
    val context = new ErgoContext(stateContext, transactionContext, ContextExtension.empty)

    proverOpt.flatMap(_.prove(box.ergoTree, context, testingTx.messageToSign).toOption) match {
      case Some(_) =>
        log.debug(s"Box certainty resolved for $box")
        true
      case None =>
        log.debug(s"Failed to resolve uncertainty for ${Algos.encode(box.id)} created at " +
          s"${box.creationHeight} while current height is ${stateContext.currentHeight}")
        //todo: remove after some time? remove spent after some time?
        false
    }
  }

  private def extractOutputs(tx: ErgoTransaction): Seq[ErgoBox] = tx.outputs
    .filter(bx => trackedBytes.exists(t => bx.propositionBytes.containsSlice(t)))

  private def extractInputs(tx: ErgoTransaction): Seq[BoxId] = tx.inputs.map(_.boxId)

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
            .foldLeft(Predef.Map.empty[ModifierId, Long]) { case (acc, bx) =>
              val boxTokens = bx.additionalTokens.map(t => bytesToId(t._1) -> t._2).toMap
              AssetUtils.mergeAssets(boxTokens, acc)
            }

          boxSelector.select(__store.unspentCertainBoxesIterator, filterFn, targetBalance, targetAssets).map { r =>
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

  private def inputsFor(targetAmount: Long,
                        targetAssets: Map[ModifierId, Long] = Map.empty): Seq[ErgoBox] = {
    boxSelector.select(__store.unspentCertainBoxesIterator, filterFn, targetAmount, targetAssets)
      .toSeq
      .flatMap(_.boxes)
  }

  private def readSecretStorage: Try[JsonSecretStorage] = {
    val dir = new File(ergoSettings.walletSettings.secretStorage.secretDir)
    if (dir.exists()) {
      dir.listFiles().toList match {
        case files if files.size > 1 =>
          Failure(new Exception("Ambiguous secret files"))
        case headFile :: _ =>
          Success(new JsonSecretStorage(headFile, ergoSettings.walletSettings.secretStorage.encryption))
        case Nil =>
          Failure(new Exception("Secret file not found"))
      }
    } else {
      Failure(new Exception("Secret dir does not exist"))
    }
  }

  private def scanLogic: Receive = {
    case ScanOffchain(tx) =>
      // scan and put to in-memory storage

    case ScanOnchain(fullBlock) =>
      proverOpt.foreach(_.IR.resetContext())
      height = fullBlock.header.height
      val (outputs, inputs) = fullBlock.transactions
        .foldLeft(Seq.empty[(ModifierId, ErgoBox)], Seq.empty[(ModifierId, BoxId)]) {
          case ((outAcc, inAcc), tx) =>
            (outAcc ++ extractOutputs(tx).map(tx.id -> _), inAcc ++ extractInputs(tx).map(tx.id -> _))
        }
      val (resolved, unresolved) = outputs
        .filterNot { case (_, o) => inputs.contains(o.id) }
        .partition { case (_, o) => resolve(o) }
      val resolvedTrackedBoxes = resolved.map { case (txId, bx) =>
        TrackedBox(txId, bx.index, Some(height), None, None, bx, BoxCertainty.Certain)
      }
      val unresolvedTrackedBoxes = unresolved.map { case (txId, bx) =>
        TrackedBox(txId, bx.index, Some(height), None, None, bx, BoxCertainty.Uncertain)
      }

      val transaction = for {
        _ <- putBoxes(resolvedTrackedBoxes ++ unresolvedTrackedBoxes)
        spentBoxes <- getAllBoxes.map(_.filter(x => inputs.map(_._2).contains(x.box.id)))
        _ <- updateIndex { case RegistryIndex(_, balance, tokensBalance, uncertainBoxes) =>
          val spentAmt = spentBoxes.map(_.box.value).sum
          val receivedAmt = resolvedTrackedBoxes.map(_.box.value).sum
          // todo: recalculate tokensBalance.
          RegistryIndex(height, balance - spentAmt + receivedAmt, tokensBalance, uncertainBoxes)
        }
        _ <- removeBoxes(spentBoxes.map(_.box.id))
      } yield ()

      transaction.transact(registry, idToBytes(fullBlock.id))

    case Rollback(version: VersionTag) =>
      Try(registry.rollback(ByteArrayWrapper(Base16.decode(version).get))).fold(
        e => log.error(s"Failed to rollback wallet registry to version $version due to: $e"), _ => ())
  }

  private def readers: Receive = {
    case ReadBalances(chainStatus) =>
      if (chainStatus.onChain) {
        sender() ! BalancesSnapshot(height, __store.confirmedBalance, __store.confirmedAssetBalances)
      } else {
        sender() ! BalancesSnapshot(height, __store.balancesWithUnconfirmed, __store.assetBalancesWithUnconfirmed)
      }

    case ReadPublicKeys(from, until) =>
      sender() ! publicKeys.slice(from, until)

    case GetFirstSecret =>
      if (proverOpt.nonEmpty) {
        proverOpt.foreach(_.secrets.headOption.foreach(s => sender() ! Success(s)))
      } else {
        sender() ! Failure(new Exception("Wallet is locked"))
      }

    case GetBoxes =>
      sender() ! __store.unspentCertainBoxesIterator.map(_.box)

    case ReadRandomPublicKey =>
      sender() ! publicKeys(Random.nextInt(publicKeys.size))

    case ReadTrackedAddresses =>
      sender() ! trackedAddresses.toIndexedSeq
  }

  private def onStateChanged: Receive = {
    case ChangedState(s: ErgoStateReader@unchecked) =>
      stateContext = s.stateContext
  }

  private def walletCommands: Receive = {

    case InitWallet(pass) if secretStorageOpt.isEmpty =>
      val seed = scorex.utils.Random.randomBytes(ergoSettings.walletSettings.seedStrengthBits / 8)
      val secretStorage = JsonSecretStorage
        .init(seed, pass)(ergoSettings.walletSettings.secretStorage)
      secretStorageOpt = Some(secretStorage)
      val mnemonicTry = new Mnemonic(walletSettings.mnemonicPhraseLanguage, walletSettings.seedStrengthBits)
        .toMnemonic(seed)
      sender() ! mnemonicTry

    case RestoreWallet(mnemonic, passOpt, encryptionPass) if secretStorageOpt.isEmpty =>
      val secretStorage = JsonSecretStorage
        .restore(mnemonic, passOpt, encryptionPass)(ergoSettings.walletSettings.secretStorage)
      secretStorageOpt = Some(secretStorage)
      sender() ! Success(())

    case RestoreWallet | InitWallet(_) =>
      sender() ! Failure(new Exception("Wallet is already initialized"))

    case UnlockWallet(pass) =>
      secretStorageOpt match {
        case Some(secretStorage) =>
          sender() ! secretStorage.unlock(pass)
          proverOpt = Some(ErgoProvingInterpreter(secretStorage.secret.map(_.key).toIndexedSeq, parameters))
          proverOpt.foreach(_.pubKeys.foreach(pk => trackedAddresses.append(P2PKAddress(pk))))
        case None =>
          sender() ! Failure(new Exception("Wallet not initialized"))
      }

    case LockWallet =>
      proverOpt = None
      secretStorageOpt.foreach(_.lock())

    case WatchFor(address) =>
      trackedAddresses.append(address)

    //generate a transaction paying to a sequence of boxes payTo
    case GenerateTransaction(requests) =>
      sender() ! generateTransactionWithOutputs(requests)
  }

}

object ErgoWalletActor {

  final case class WatchFor(address: ErgoAddress)

  final case class ScanOffchain(tx: ErgoTransaction)

  final case class ScanOnchain(block: ErgoFullBlock)

  final case class Rollback(version: VersionTag)

  final case class GenerateTransaction(requests: Seq[TransactionRequest])

  final case class ReadBalances(chainStatus: ChainStatus)

  final case class ReadPublicKeys(from: Int, until: Int)

  final case class InitWallet(pass: String)

  final case class RestoreWallet(mnemonic: String, passOpt: Option[String], encryptionPass: String)

  final case class UnlockWallet(pass: String)

  final case object LockWallet

  final case object ReadRandomPublicKey

  final case object ReadTrackedAddresses

  final case object GetFirstSecret

  final case object GetBoxes

}
