package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import io.circe.Encoder
import io.circe.syntax._
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.mining.CandidateBlock
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Algos, Constants, ErgoSettings}
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.ReceivableMessages._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.util.{Failure, Try}

class ErgoMiner(ergoSettings: ErgoSettings,
                viewHolderRef: ActorRef,
                readersHolderRef: ActorRef,
                nodeId: Array[Byte],
                timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  import ErgoMiner._

  private val startTime = timeProvider.time()
  private val votes: Array[Byte] = nodeId

  //shared mutable state
  private var isMining = false
  private var candidateOpt: Option[CandidateBlock] = None
  private var miningThreads: Seq[ActorRef] = Seq.empty

  override def preStart(): Unit = {
    viewHolderRef ! Subscribe(Seq(NodeViewHolder.EventType.SuccessfulSemanticallyValidModifier))
  }

  private def unknownMessage: Receive = {
    case m =>
      log.warn(s"Unexpected message $m")
  }

  private def miningStatus: Receive = {
    case MiningStatusRequest =>
      sender ! MiningStatusResponse(isMining, votes, candidateOpt)
  }

  private def startMining: Receive = {
    case StartMining if candidateOpt.nonEmpty && !isMining && ergoSettings.nodeSettings.mining =>
      log.info("Starting Mining")
      miningThreads = Seq(ErgoMiningThread(ergoSettings, viewHolderRef, candidateOpt.get)(context))
      miningThreads.foreach(_ ! candidateOpt.get)
      isMining = true
    case StartMining if candidateOpt.isEmpty =>
      produceCandidate(readersHolderRef, ergoSettings, nodeId)
  }

  private def receiveSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(mod) =>
      if (isMining) {
        mod match {
          case f: ErgoFullBlock if !candidateOpt.flatMap(_.parentOpt).exists(_.id sameElements f.header.id) =>
            produceCandidate(readersHolderRef, ergoSettings, nodeId)
          case _ =>
        }
      } else if (ergoSettings.nodeSettings.mining) {
        mod match {
          case f: ErgoFullBlock if f.header.timestamp >= startTime =>
            self ! StartMining
          case _ =>
        }
      }
  }

  private def receiverCandidateBlock: Receive = {
    case c: CandidateBlock =>
      procCandidateBlock(c)
    case cOpt: Option[CandidateBlock] if cOpt.nonEmpty =>
      procCandidateBlock(cOpt.get)
  }

  override def receive: Receive = receiveSemanticallySuccessfulModifier orElse
    receiverCandidateBlock orElse
    miningStatus orElse
    startMining orElse
    unknownMessage

  private def procCandidateBlock(c: CandidateBlock): Unit = {
    log.debug(s"Got candidate block $c")
    candidateOpt = Some(c)
    if (!isMining) self ! StartMining
    miningThreads.foreach(_ ! c)
  }

  //TODO rewrite from readers when state.proofsForTransactions will be ready
  def produceCandidate(readersHolderRef: ActorRef,
                       ergoSettings: ErgoSettings,
                       nodeId: Array[Byte]): Unit = {
    viewHolderRef ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[CandidateBlock]] { v =>
      log.info("Start candidate creation")
      val history = v.history
      val state = v.state
      val pool = v.pool
      val bestHeaderOpt = history.bestFullBlockOpt.map(_.header)
      if (bestHeaderOpt.isDefined || ergoSettings.nodeSettings.offlineGeneration) {

        Try {
          val coinbase: AnyoneCanSpendTransaction = {
            val txBoxes = state.anyoneCanSpendBoxesAtHeight(bestHeaderOpt.map(_.height + 1).getOrElse(0))
            AnyoneCanSpendTransaction(txBoxes.map(_.nonce), txBoxes.map(_.value))
          }

          //only transactions valid from against the current utxo state we take from the mem pool
          //todo: move magic number to testnet settings
          val txs = coinbase +: state.filterValid(pool.unconfirmed.values.toSeq)

          //we also filter transactions which are trying to spend the same box. Currently, we pick just the first one
          //of conflicting transaction. Another strategy is possible(e.g. transaction with highest fee)
          //todo: move this logic to MemPool.put? Problem we have now is that conflicting transactions are still in
          // the pool
          val txsNoConflict = fixTxsConflicts(txs)

          val (adProof, adDigest) = state.proofsForTransactions(txsNoConflict).get

          val timestamp = timeProvider.time()
          val nBits = bestHeaderOpt.map(parent => history.requiredDifficultyAfter(parent))
            .map(d => RequiredDifficulty.encodeCompactBits(d)).getOrElse(Constants.InitialNBits)
          val candidate = CandidateBlock(bestHeaderOpt, nBits, adDigest, adProof, txsNoConflict, timestamp, nodeId)
          log.debug(s"Send candidate block with ${candidate.transactions.length} transactions")
          //TODO takes a lot of time
          candidate

        }.recoverWith { case thr =>
          log.warn("Error when trying to produce a candidate block: ", thr)
          Failure(thr)
        }.toOption
      } else {
        //Do not try to mine genesis block when offlineGeneration = false
        None
      }
    }
  }

  private def fixTxsConflicts(txs: Seq[AnyoneCanSpendTransaction]): Seq[AnyoneCanSpendTransaction] = txs
    .foldLeft((Seq.empty[AnyoneCanSpendTransaction], Set.empty[ByteArrayWrapper])) { case ((s, keys), tx) =>
      val bxsBaw = tx.boxIdsToOpen.map(ByteArrayWrapper.apply)
      if (bxsBaw.forall(k => !keys.contains(k)) && bxsBaw.size == bxsBaw.toSet.size) {
        (s :+ tx) -> (keys ++ bxsBaw)
      } else {
        (s, keys)
      }
    }._1
}


object ErgoMiner extends ScorexLogging {

  case object StartMining

  case object MiningStatusRequest

  case class MiningStatusResponse(isMining: Boolean, votes: Array[Byte], candidateBlock: Option[CandidateBlock])

  implicit val jsonEncoder: Encoder[MiningStatusResponse] = (r: MiningStatusResponse) =>
    Map(
      "isMining" -> r.isMining.asJson,
      "votes" -> Algos.encode(r.votes).asJson,
      "candidateBlock" -> r.candidateBlock.map(_.asJson).getOrElse("None".asJson)
    ).asJson

}

object ErgoMinerRef {

  def props(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            nodeId: Array[Byte],
            timeProvider: NetworkTimeProvider): Props =
    Props(new ErgoMiner(ergoSettings, viewHolderRef, readersHolderRef, nodeId, timeProvider))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            nodeId: Array[Byte],
            timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, readersHolderRef, nodeId, timeProvider))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            nodeId: Array[Byte],
            timeProvider: NetworkTimeProvider,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, readersHolderRef, nodeId, timeProvider), name)
}
