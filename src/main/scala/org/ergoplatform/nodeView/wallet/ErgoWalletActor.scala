package org.ergoplatform.nodeView.wallet

import java.io.File
import java.util

import akka.actor.Actor
import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.ErgoContext
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.state.{ErgoStateContext, ErgoStateReader}
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest, TransactionRequest}
import org.ergoplatform.settings._
import org.ergoplatform.utils.{AssetUtils, BoxUtils}
import org.ergoplatform.wallet.boxes.BoxCertainty.Uncertain
import org.ergoplatform.wallet.boxes.{BoxSelector, ChainStatus, TrackedBox}
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.wallet.protocol.context.TransactionContext
import org.ergoplatform.wallet.secrets.{ExtendedSecretKey, JsonSecretStorage}
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

  private val registry = new WalletStorage

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

  //todo: make resolveUncertainty(boxId, witness)
  private def resolveUncertainty(idOpt: Option[ModifierId]): Boolean = {
    (idOpt.flatMap(id => registry.byId(id)) orElse registry.nextUncertain()).exists { uncertainBox =>
      val box = uncertainBox.box

      val testingTx = UnsignedErgoLikeTransaction(
        IndexedSeq(new UnsignedInput(box.id)),
        IndexedSeq(new ErgoBoxCandidate(1L, Constants.TrueLeaf, creationHeight = height))
      )

      val transactionContext = TransactionContext(IndexedSeq(box), IndexedSeq(), testingTx, selfIndex = 0)
      val context = new ErgoContext(stateContext, transactionContext, ContextExtension.empty)

      proverOpt.flatMap(_.prove(box.ergoTree, context, testingTx.messageToSign).toOption) match {
        case Some(_) =>
          log.debug(s"Box certainty resolved for $uncertainBox")
          registry.makeTransition(uncertainBox.boxId, MakeCertain)
        case None =>
          log.debug(s"Failed to resolve uncertainty for ${uncertainBox.boxId} created at " +
            s"${uncertainBox.inclusionHeightOpt} while current height is ${stateContext.currentHeight}")
          //todo: remove after some time? remove spent after some time?
          false
      }
    }
  }

  private def scan(tx: ErgoTransaction, heightOpt: Option[Height]): Seq[TrackedBox] = {
    scanInputs(tx, heightOpt)
    tx.outputCandidates
      .zipWithIndex
      .flatMap { case (outCandidate, outIndex) =>
        scanOutput(outCandidate, outIndex.toShort, tx, heightOpt)
      }
  }

  private def scanInputs(tx: ErgoTransaction, heightOpt: Option[Height]): Boolean = {
    tx.inputs.forall { inp =>
      val boxId = bytesToId(inp.boxId)
      registry.makeTransition(boxId, ProcessSpending(tx, heightOpt))
    }
  }

  private def scanOutput(outCandidate: ErgoBoxCandidate, outIndex: Short,
                         tx: ErgoTransaction, heightOpt: Option[Height]): Option[TrackedBox] = {
    if (trackedBytes.exists(t => outCandidate.propositionBytes.containsSlice(t))) {
      val tb = TrackedBox(tx, outIndex, heightOpt, outCandidate.toBox(tx.id, outIndex), Uncertain)
      registerBox(tb)
      Some(tb)
    } else {
      None
    }
  }

  private def registerBox(trackedBox: TrackedBox): Boolean = {
    if (registry.contains(trackedBox.boxId)) {
      trackedBox.inclusionHeightOpt match {
        case Some(h) =>
          registry.makeTransition(trackedBox.boxId, CreationConfirmation(h))
        case None =>
          log.warn(s"Double registration of the off-chain box: ${trackedBox.boxId}")
          false
      }
    } else {
      registry.register(trackedBox)
      true
    }
  }

  private def scanLogic: Receive = {
    case ScanOffchain(tx) =>
      scan(tx, None).foreach { tb =>
        self ! Resolve(Some(tb.boxId))
      }

    case Resolve(idOpt: Option[ModifierId]) =>
      if (resolveUncertainty(idOpt)) {
        // If the resolving was successful, try to resolve one more random box
        self ! Resolve(None)
      }

    case ScanOnchain(fullBlock) =>
      proverOpt.foreach(_.IR.resetContext())
      height = fullBlock.header.height
      fullBlock.transactions.flatMap(tx => scan(tx, Some(height))).foreach { tb =>
        self ! Resolve(Some(tb.boxId))
      }
      // Try to resolve all just received boxes plus one more random
      self ! Resolve(None)

    //todo: update utxo root hash
    case Rollback(heightTo) =>
      height.until(heightTo, -1).foreach { h =>
        val toRemove = registry.confirmedAt(h)
        toRemove.foreach { boxId =>
          registry.makeTransition(boxId, ProcessRollback(heightTo))
        }
      }
      height = heightTo
  }

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

          boxSelector.select(registry.unspentCertainBoxesIterator, filterFn, targetBalance, targetAssets).map { r =>
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
    boxSelector.select(registry.unspentCertainBoxesIterator, filterFn, targetAmount, targetAssets)
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

  private def readers: Receive = {
    case ReadBalances(chainStatus) =>
      if (chainStatus.onChain) {
        sender() ! BalancesSnapshot(height, registry.confirmedBalance, registry.confirmedAssetBalances)
      } else {
        sender() ! BalancesSnapshot(height, registry.balancesWithUnconfirmed, registry.assetBalancesWithUnconfirmed)
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
      sender() ! registry.unspentCertainBoxesIterator.map(_.box)

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

    case InitWallet(pass, mnemonicPassOpt) if secretStorageOpt.isEmpty =>
      val entropy = scorex.utils.Random.randomBytes(ergoSettings.walletSettings.seedStrengthBits / 8)
      val mnemonicTry = new Mnemonic(walletSettings.mnemonicPhraseLanguage, walletSettings.seedStrengthBits)
        .toMnemonic(entropy)
        .map { mnemonic =>
          val secretStorage = JsonSecretStorage
            .init(Mnemonic.toSeed(mnemonic, mnemonicPassOpt), pass)(ergoSettings.walletSettings.secretStorage)
          secretStorageOpt = Some(secretStorage)
          mnemonic
        }
      util.Arrays.fill(entropy, 0: Byte)
    
      sender() ! mnemonicTry

    case RestoreWallet(mnemonic, passOpt, encryptionPass) if secretStorageOpt.isEmpty =>
      val secretStorage = JsonSecretStorage
        .restore(mnemonic, passOpt, encryptionPass)(ergoSettings.walletSettings.secretStorage)
      secretStorageOpt = Some(secretStorage)
      sender() ! Success(())

    case RestoreWallet | InitWallet(_, _) =>
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

  private[ErgoWalletActor] case class Resolve(idOpt: Option[ModifierId])

  final case class WatchFor(address: ErgoAddress)

  final case class ScanOffchain(tx: ErgoTransaction)

  final case class ScanOnchain(block: ErgoFullBlock)

  final case class Rollback(height: Int)

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
