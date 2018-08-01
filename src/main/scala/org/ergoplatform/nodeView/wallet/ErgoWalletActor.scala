package org.ergoplatform.nodeView.wallet

import akka.actor.Actor
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import org.ergoplatform._
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.state.ErgoStateContext
import org.ergoplatform.nodeView.wallet.BoxCertainty.Uncertain
import scorex.core.utils.ScorexLogging
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import sigmastate.interpreter.ContextExtension
import sigmastate.{AvlTreeData, Values}

import scala.collection.mutable
import scala.util.{Failure, Success}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global



case class BalancesSnapshot(height: Height, balance: Long, assetBalances: Map[ByteArrayWrapper, Long])


class ErgoWalletActor(seed: String,
                      addressEncoder: ErgoAddressEncoder) extends Actor with ScorexLogging {

  import ErgoWalletActor._
  import Registry._

  //todo: pass as parameter, add to config
  val coinSelector: BoxSelector = new DefaultBoxSelector

  private val prover = new ErgoProvingInterpreter(seed)

  private var height = 0
  private var lastBlockUtxoRootHash = ADDigest @@ Array.fill(32)(0: Byte)

  private val trackedAddresses: mutable.Buffer[ErgoAddress] =
    mutable.Buffer(prover.dlogPubkeys: _ *).map(P2PKAddress.apply)

  private val trackedBytes: mutable.Buffer[Array[Byte]] = trackedAddresses.map(addressEncoder.contentBytes)

  //todo: make resolveUncertainty(boxId, witness)
  private def resolveUncertainty(): Unit = {
    Registry.nextUncertain().foreach { uncertainBox =>
      val box = uncertainBox.box

      val lastUtxoDigest = AvlTreeData(lastBlockUtxoRootHash, 32)

      val testingTx = UnsignedErgoLikeTransaction(
        IndexedSeq(new UnsignedInput(box.id)),
        IndexedSeq(new ErgoBoxCandidate(1L, Values.TrueLeaf))
      )

      val context =
        ErgoLikeContext(height + 1, lastUtxoDigest, IndexedSeq(box), testingTx, box, ContextExtension.empty)

      prover.prove(box.proposition, context, testingTx.messageToSign) match {
        case Success(_) =>
          log.info(s"Uncertain box is mine! $uncertainBox")
          Registry.makeTransition(uncertainBox, uncertainBox.makeCertain())
        case Failure(_) =>
        //todo: remove after some time? remove spent after some time?
      }
    }
  }

  def scan(tx: ErgoTransaction, heightOpt: Option[Height]): Boolean = {
    tx.inputs.foreach { inp =>
      val boxId = ByteArrayWrapper(inp.boxId)
      if (Registry.registryContains(boxId)) {
        Registry.makeTransition(boxId, ProcessSpending(tx, heightOpt))
      }
    }

    tx.outputCandidates.zipWithIndex.exists { case (outCandidate, outIndex) =>
      trackedBytes.find(t => outCandidate.propositionBytes.containsSlice(t)) match {
        case Some(_) =>
          val idxShort = outIndex.toShort
          val box = outCandidate.toBox(tx.serializedId, idxShort)
          val bu = heightOpt match {
            case Some(h) => UnspentOnchainBox(tx, idxShort, h, box, Uncertain)
            case None => UnspentOffchainBox(tx, idxShort, box, Uncertain)
          }
          bu.register()
          true
        case None =>
          false
      }
    }
  }

  private def extractFromBlock(fb: ErgoFullBlock): Int = {
    height = fb.header.height
    lastBlockUtxoRootHash = fb.header.stateRoot
    fb.transactions.count(tx => scan(tx, Some(height)))
  }

  override def receive: Receive = {
    case WatchFor(address) =>
      trackedAddresses.append(address)
      trackedBytes.append(addressEncoder.contentBytes(address))

    case ScanOffchain(tx) =>
      if (scan(tx, None)) {
        self ! Resolve
      }

    case Resolve =>
      resolveUncertainty()
      //todo: avoid magic number, use non-default executor? check that resolve is not scheduled already
      if (Registry.uncertainBoxes.nonEmpty) {
        context.system.scheduler.scheduleOnce(10.seconds)(self ! Resolve)
      }

    case ScanOnchain(fullBlock) =>
      val txsFound = extractFromBlock(fullBlock)
      (1 to txsFound).foreach(_ => self ! Resolve)

    //todo: update utxo root hash
    case Rollback(heightTo) =>

      height.until(heightTo, -1).foreach{h =>
        val toRemove = Registry.confirmedAt(h)
        toRemove.foreach{boxId =>
          Registry.removeFromRegistry(boxId).foreach{tb =>
            tb.transitionBack(heightTo) match {
              case Some(newBox) =>
                newBox.register()
              case None =>
                //todo: should we be here at all?
            }
          }
        }
      }

      height = heightTo
      log.warn("Rollback in the wallet is not implemented")

    case ReadBalances(confirmed) =>
      if(confirmed) {
        sender() ! BalancesSnapshot(height, confirmedBalance, confirmedAssetBalances.toMap) //todo: avoid .toMap?
      } else {
        sender() ! BalancesSnapshot(height, unconfirmedBalance, unconfirmedAssetBalances.toMap) //todo: avoid .toMap?
      }

    case ReadWalletAddresses =>
      sender() ! trackedAddresses.toIndexedSeq

    case GenerateTransaction(payTo) =>
      //todo: add assets
      val targetBalance = payTo.map(_.value).sum

      //we do not use offchain boxes to create a transaction
      def filterFn(bu: UnspentBox) = bu.onchain

      val txOpt = coinSelector.select(unspentBoxes, filterFn, targetBalance, Map()).flatMap { r =>
        val inputs = r.boxes.toIndexedSeq

        //todo: fix proposition, assets
        val changeAddress = prover.dlogPubkeys.head
        val changeBoxCandidates = r.changeBoxes.map {case (chb, cha) =>
          val assets = cha.map(t => Digest32 @@ t._1.data -> t._2).toIndexedSeq
          new ErgoBoxCandidate(chb, changeAddress, assets)
        }

        val unsignedTx = new UnsignedErgoTransaction(
          inputs.map(_.id).map(id => new UnsignedInput(id)),
          (payTo ++ changeBoxCandidates).toIndexedSeq)

        prover.sign(unsignedTx, inputs, ErgoStateContext(height, lastBlockUtxoRootHash))
      }

      sender() ! txOpt
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

  case object ReadWalletAddresses
}
