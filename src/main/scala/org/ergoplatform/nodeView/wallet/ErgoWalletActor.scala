package org.ergoplatform.nodeView.wallet

import akka.actor.Actor
import org.ergoplatform.ErgoBox.{R4, R5, R6}
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.{ErgoContext, TransactionContext}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.nodeView.wallet.BoxCertainty.Uncertain
import org.ergoplatform.nodeView.wallet.requests.{AssetIssueRequest, PaymentRequest, TransactionRequest}
import org.ergoplatform.settings.{Constants, ErgoSettings}
import org.ergoplatform.utils.AssetUtils
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.{ModifierId, ScorexLogging, bytesToId, idToBytes}
import sigmastate.Values.{IntConstant, StringConstant}
import sigmastate.interpreter.ContextExtension

import scala.collection.{immutable, mutable}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success, Try}

case class BalancesSnapshot(height: Height, balance: Long, assetBalances: immutable.Map[ModifierId, Long])

class ErgoWalletActor(ergoSettings: ErgoSettings) extends Actor with ScorexLogging {

  import ErgoWalletActor._

  private lazy val seed = ergoSettings.walletSettings.seed

  private lazy val scanningInterval = ergoSettings.walletSettings.scanningInterval

  private val registry = new WalletStorage

  //todo: pass as a class argument, add to config
  val boxSelector: BoxSelector = DefaultBoxSelector

  private val prover = new ErgoProvingInterpreter(seed, ergoSettings.walletSettings.dlogSecretsNumber)

  private var height = 0
  private var lastBlockUtxoRootHash = ADDigest @@ Array.fill(32)(0: Byte)

  private implicit val addressEncoder: ErgoAddressEncoder = ErgoAddressEncoder(ergoSettings.chainSettings.addressPrefix)
  private val publicKeys: Seq[P2PKAddress] = Seq(prover.dlogPubkeys: _ *).map(P2PKAddress.apply)

  private val trackedAddresses: mutable.Buffer[ErgoAddress] = publicKeys.toBuffer

  private val trackedBytes: mutable.Buffer[Array[Byte]] = trackedAddresses.map(_.contentBytes)

  //we currently do not use off-chain boxes to create a transaction
  private def filterFn(trackedBox: TrackedBox) = trackedBox.chainStatus.onchain

  //todo: make resolveUncertainty(boxId, witness)
  private def resolveUncertainty(): Unit = {
    registry.nextUncertain().foreach { uncertainBox =>
      val box = uncertainBox.box

      val testingTx = UnsignedErgoLikeTransaction(
        IndexedSeq(new UnsignedInput(box.id)),
        IndexedSeq(new ErgoBoxCandidate(1L, Constants.TrueLeaf))
      )

      val stateContext = ErgoStateContext(height + 1, lastBlockUtxoRootHash)

      val transactionContext = TransactionContext(IndexedSeq(box), testingTx, selfIndex = 0)

      val context =
        new ErgoContext(stateContext, transactionContext, ergoSettings.metadata, ContextExtension.empty)

      prover.prove(box.proposition, context, testingTx.messageToSign) match {
        case Success(_) =>
          log.debug(s"Uncertain box is mine! $uncertainBox")
          registry.makeTransition(uncertainBox.boxId, MakeCertain)
        case Failure(_) =>
        //todo: remove after some time? remove spent after some time?
      }
    }
  }

  def scan(tx: ErgoTransaction, heightOpt: Option[Height]): Boolean = {
    scanInputs(tx, heightOpt)
    tx.outputCandidates
      .zipWithIndex
      .count { case (outCandidate, outIndex) => scanOutput(outCandidate, outIndex.toShort, tx, heightOpt) } > 0
  }

  protected def scanInputs(tx: ErgoTransaction, heightOpt: Option[Height]): Boolean = {
    tx.inputs.forall { inp =>
      val boxId = bytesToId(inp.boxId)
      registry.makeTransition(boxId, ProcessSpending(tx, heightOpt))
    }
  }

  private def scanOutput(outCandidate: ErgoBoxCandidate, outIndex: Short,
                         tx: ErgoTransaction, heightOpt: Option[Height]): Boolean = {
    trackedBytes.exists(t => outCandidate.propositionBytes.containsSlice(t)) &&
      registerBox(TrackedBox(tx, outIndex, heightOpt, outCandidate.toBox(tx.id, outIndex), Uncertain))
  }

  private def registerBox(trackedBox: TrackedBox): Boolean = {
    if (registry.contains(trackedBox.boxId)) {
      trackedBox.creationHeight match {
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

  private def extractFromBlock(fb: ErgoFullBlock): Int = {
    height = fb.header.height
    lastBlockUtxoRootHash = fb.header.stateRoot
    fb.transactions.count(tx => scan(tx, Some(height)))
  }

  def scanLogic: Receive = {
    case ScanOffchain(tx) =>
      if (scan(tx, None)) {
        self ! Resolve
      }

    case Resolve =>
      resolveUncertainty()
      //todo: use non-default executor?
      if (registry.uncertainExists) {
        context.system.scheduler.scheduleOnce(scanningInterval)(self ! Resolve)
      }

    case ScanOnchain(fullBlock) =>
      val txsFound = extractFromBlock(fullBlock)
      (1 to txsFound).foreach(_ => self ! Resolve)

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
        new ErgoBoxCandidate(value, address.script, assets.getOrElse(Seq.empty), registers.getOrElse(Map.empty))
      case AssetIssueRequest(address, amount, name, description, decimals) =>
        val firstInput = inputsFor(
          requests
            .foldLeft(Seq.empty[PaymentRequest]) {
              case (acc, pr: PaymentRequest) => acc :+ pr
              case (acc, _) => acc
            }
            .map(_.value)
            .sum
        ).headOption.getOrElse(throw new Exception("Can't issue asset with no inputs"))
        val assetId = Digest32 !@@ firstInput.id
        val nonMandatoryRegisters = scala.Predef.Map(
          R4 -> StringConstant(name),
          R5 -> StringConstant(description),
          R6 -> IntConstant(decimals)
        )
        new ErgoBoxCandidate(0L, address.script, Seq(assetId -> amount), nonMandatoryRegisters)
      case other => throw new Exception(s"Unknown TransactionRequest type: $other")
    }
  }

  protected def generateTransactionWithOutputs(requests: Seq[TransactionRequest]): Try[ErgoTransaction] =
    requestsToBoxCandidates(requests).map { payTo =>
      require(prover.dlogPubkeys.nonEmpty, "No public keys in the prover to extract change address from")
      require(requests.count(_.isInstanceOf[AssetIssueRequest]) <= 1, "Too many asset issue requests")
      require(payTo.forall(_.value >= 0), "Non-positive Ergo value")
      require(payTo.forall(_.additionalTokens.forall(_._2 >= 0)), "Non-positive asset value")

      val assetIssueBox = payTo
        .zip(requests)
        .filter(_._2.isInstanceOf[AssetIssueRequest])
        .map(_._1)
        .headOption

      val targetBalance = payTo.map(_.value).sum

      val targetAssets = payTo
        .filterNot(bx => assetIssueBox.contains(bx))
        .foldLeft(Predef.Map.empty[ModifierId, Long]) { case (acc, bx) =>
          val boxTokens = bx.additionalTokens.map(t => bytesToId(t._1) -> t._2).toMap
          AssetUtils.mergeAssets(boxTokens, acc)
        }

      boxSelector.select(registry.unspentBoxesIterator, filterFn, targetBalance, targetAssets).flatMap { r =>
        val inputs = r.boxes.toIndexedSeq

        val changeAddress = prover.dlogPubkeys(Random.nextInt(prover.dlogPubkeys.size))

        val changeBoxCandidates = r.changeBoxes.map { case (chb, cha) =>
          val assets = cha.map(t => Digest32 @@ idToBytes(t._1) -> t._2).toIndexedSeq
          new ErgoBoxCandidate(chb, changeAddress, assets)
        }

        val unsignedTx = new UnsignedErgoTransaction(
          inputs.map(_.id).map(id => new UnsignedInput(id)),
          (payTo ++ changeBoxCandidates).toIndexedSeq
        )

        prover.sign(unsignedTx, inputs, ergoSettings.metadata, ErgoStateContext(height, lastBlockUtxoRootHash)).toOption
      } match {
        case Some(tx) => tx
        case None => throw new Exception(s"No enough boxes to assemble a transaction for $payTo")
      }
    }

  protected def inputsFor(targetAmount: Long,
                          targetAssets: scala.Predef.Map[ModifierId, Long] = Map.empty): Seq[ErgoBox] = {
    boxSelector.select(registry.unspentBoxesIterator, filterFn, targetAmount, targetAssets).toSeq.flatMap(_.boxes)
  }

  def readers: Receive = {
    case ReadBalances(chainStatus) =>
      if (chainStatus.onchain) {
        sender() ! BalancesSnapshot(height, registry.confirmedBalance, registry.confirmedAssetBalances)
      } else {
        sender() ! BalancesSnapshot(height, registry.balancesWithUnconfirmed, registry.assetBalancesWithUnconfirmed)
      }

    case ReadPublicKeys(from, until) =>
      sender() ! publicKeys.slice(from, until)

    case ReadRandomPublicKey =>
      sender() ! publicKeys(Random.nextInt(publicKeys.size))

    case ReadTrackedAddresses =>
      sender() ! trackedAddresses.toIndexedSeq
  }

  override def receive: Receive = scanLogic orElse readers orElse {
    case WatchFor(address) =>
      trackedAddresses.append(address)
      trackedBytes.append(address.contentBytes)

    //generate a transaction paying to a sequence of boxes payTo
    case GenerateTransaction(requests) =>
      sender() ! generateTransactionWithOutputs(requests)

    case m =>
      log.warn(s"Got unhandled message $m")
  }
}

object ErgoWalletActor {

  private[ErgoWalletActor] case object Resolve

  case class WatchFor(address: ErgoAddress)

  case class ScanOffchain(tx: ErgoTransaction)

  case class ScanOnchain(block: ErgoFullBlock)

  case class Rollback(height: Int)

  case class GenerateTransaction(requests: Seq[TransactionRequest])

  case class ReadBalances(chainStatus: ChainStatus)

  case class ReadPublicKeys(from: Int, until: Int)

  case object ReadRandomPublicKey

  case object ReadTrackedAddresses

}
