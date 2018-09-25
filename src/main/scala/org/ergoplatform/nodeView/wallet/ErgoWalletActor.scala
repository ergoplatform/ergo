package org.ergoplatform.nodeView.wallet

import akka.actor.Actor
import org.ergoplatform.ErgoLikeContext.Metadata
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.nodeView.wallet.BoxCertainty.Uncertain
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.AssetUtils
import scorex.core.{ModifierId, bytesToId, idToBytes}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import sigmastate.interpreter.ContextExtension
import sigmastate.{AvlTreeData, Values}

import scala.collection.{Map, mutable}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success, Try}


case class BalancesSnapshot(height: Height, balance: Long, assetBalances: Map[ModifierId, Long])


class ErgoWalletActor(settings: ErgoSettings) extends Actor with ScorexLogging {

  import ErgoWalletActor._

  private lazy val seed = settings.walletSettings.seed

  private lazy val scanningInterval = settings.walletSettings.scanningInterval

  private val registry = new WalletStorage

  //todo: pass as a class argument, add to config
  val boxSelector: BoxSelector = DefaultBoxSelector

  private val prover = new ErgoProvingInterpreter(seed,
    settings.walletSettings.dlogSecretsNumber,
    settings.chainSettings.addressPrefix)

  private var height = 0
  private var lastBlockUtxoRootHash = ADDigest @@ Array.fill(32)(0: Byte)

  private implicit val addressEncoder = ErgoAddressEncoder(settings.chainSettings.addressPrefix)
  private val publicKeys: Seq[P2PKAddress] = Seq(prover.dlogPubkeys: _ *).map(P2PKAddress.apply)

  private val trackedAddresses: mutable.Buffer[ErgoAddress] = publicKeys.toBuffer

  private val trackedBytes: mutable.Buffer[Array[Byte]] = trackedAddresses.map(_.contentBytes)

  //todo: make resolveUncertainty(boxId, witness)
  private def resolveUncertainty(): Unit = {
    registry.nextUncertain().foreach { uncertainBox =>
      val box = uncertainBox.box

      val lastUtxoDigest = AvlTreeData(lastBlockUtxoRootHash, 32)

      val testingTx = UnsignedErgoLikeTransaction(
        IndexedSeq(new UnsignedInput(box.id)),
        IndexedSeq(new ErgoBoxCandidate(1L, Values.TrueLeaf))
      )

      val contextMetadata = Metadata(settings.chainSettings.addressPrefix)
      val context =
        ErgoLikeContext(height + 1, lastUtxoDigest, IndexedSeq(box), testingTx, box, contextMetadata, ContextExtension.empty)

      prover.prove(box.proposition, context, testingTx.messageToSign) match {
        case Success(_) =>
          log.debug(s"Uncertain box is mine! $uncertainBox")
          registry.makeTransition(uncertainBox.boxId, MakeCertain)
        case Failure(_) =>
        //todo: remove after some time? remove spent after some time?
      }
    }
  }

  protected def scanInputs(tx: ErgoTransaction, heightOpt: Option[Height]): Unit = {
    tx.inputs.foreach { inp =>
      val boxId = bytesToId(inp.boxId)
      registry.makeTransition(boxId, ProcessSpending(tx, heightOpt))
    }
  }

  def scan(tx: ErgoTransaction, heightOpt: Option[Height]): Boolean = {
    scanInputs(tx, heightOpt)

    tx.outputCandidates.zipWithIndex.count { case (outCandidate, outIndex) =>
      trackedBytes.find(t => outCandidate.propositionBytes.containsSlice(t)) match {
        case Some(_) =>
          val idxShort = outIndex.toShort
          val box = outCandidate.toBox(tx.id, idxShort)
          val boxId = bytesToId(box.id)
          if (registry.contains(boxId)) {
            heightOpt match {
              case Some(h) =>
                registry.makeTransition(boxId, CreationConfirmation(h))
              case None =>
                log.warn(s"Double registration of the offchain box: $boxId")
                false
            }
          } else {
            registry.register(TrackedBox(tx, idxShort, heightOpt, box, Uncertain))
            true
          }
        case None =>
          false
      }
    } > 0
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

  protected def generateTransactionWithOutputs(payTo: Seq[ErgoBoxCandidate]): Try[ErgoTransaction] = Try {
    require(prover.dlogPubkeys.nonEmpty, "No public keys in the prover to extract change address from")
    require(payTo.forall(_.value > 0), "Non-positive Ergo value")
    require(payTo.forall(_.additionalTokens.forall(_._2 > 0)), "Non-positive asset value")

    val targetBalance = payTo.map(_.value).sum

    val targetAssets = mutable.Map[ModifierId, Long]()

    payTo.map(_.additionalTokens).foreach { boxTokens =>
      AssetUtils.mergeAssets(targetAssets, boxTokens.map(t => bytesToId(t._1) -> t._2).toMap)
    }

    //we currently do not use off-chain boxes to create a transaction
    def filterFn(trackedBox: TrackedBox) = trackedBox.chainStatus.onchain

    boxSelector.select(registry.unspentBoxesIterator, filterFn, targetBalance, targetAssets.toMap).flatMap { r =>
      val inputs = r.boxes.toIndexedSeq

      val changeAddress = prover.dlogPubkeys(Random.nextInt(prover.dlogPubkeys.size))

      val changeBoxCandidates = r.changeBoxes.map { case (chb, cha) =>

        // todo: uncomment when sigma-state dependency will be updated from 0.9.5-SNAPSHOT
        val assets = cha.map(t => Digest32 @@ idToBytes(t._1) -> t._2).toIndexedSeq

        new ErgoBoxCandidate(chb, changeAddress, assets)
      }

      val unsignedTx = new UnsignedErgoTransaction(
        inputs.map(_.id).map(id => new UnsignedInput(id)),
        (payTo ++ changeBoxCandidates).toIndexedSeq)

      prover.sign(unsignedTx, inputs, ErgoStateContext(height, lastBlockUtxoRootHash)).toOption
    } match {
      case Some(tx) => tx
      case None => throw new Exception(s"No enough boxes to assemble a transaction for $payTo")
    }
  }

  def readers: Receive = {
    case ReadBalances(confirmed) =>
      if (confirmed) {
        sender() ! BalancesSnapshot(height, registry.confirmedBalance, registry.confirmedAssetBalances)
      } else {
        sender() ! BalancesSnapshot(height, registry.unconfirmedBalance, registry.unconfirmedAssetBalances)
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
    case GenerateTransaction(payTo) =>
      sender() ! generateTransactionWithOutputs(payTo)

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

  case class GenerateTransaction(payTo: Seq[ErgoBoxCandidate])

  case class ReadBalances(confirmed: Boolean)

  case class ReadPublicKeys(from: Int, until: Int)

  case object ReadRandomPublicKey

  case object ReadTrackedAddresses

}
