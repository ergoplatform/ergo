package org.ergoplatform.local

import akka.actor.{Actor, ActorRef}
import akka.pattern._
import akka.util.Timeout
import com.typesafe.scalalogging.LazyLogging
import io.circe.Json
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
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.{GetDataFromCurrentView, SemanticallySuccessfulModifier, Subscribe}
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class ErgoMiner(ergoSettings: ErgoSettings, viewHolderRef: ActorRef, readersHolderRef: ActorRef, nodeId: Array[Byte],
                timeProvider: NetworkTimeProvider) extends Actor with LazyLogging {

  import ErgoMiner._

  private var isMining = false
  private var nonce = 0
  private var candidateOpt: Option[CandidateBlock] = None

  private val powScheme = ergoSettings.chainSettings.poWScheme
  private val startTime = timeProvider.time()
  private val votes: Array[Byte] = nodeId

  override def preStart(): Unit = {
    viewHolderRef ! Subscribe(Seq(NodeViewHolder.EventType.SuccessfulSemanticallyValidModifier))
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(mod) =>
      if (isMining) {
        mod match {
          case f: ErgoFullBlock if !candidateOpt.flatMap(_.parentOpt).exists(_.id sameElements f.header.id) =>
            produceCandidate(readersHolderRef, ergoSettings, nodeId).foreach(_.foreach(c => self ! c))

          case _ =>
        }
      } else if (ergoSettings.nodeSettings.mining) {
        mod match {
          case f: ErgoFullBlock if f.header.timestamp >= startTime =>
            self ! StartMining

          case _ =>
        }
      }

    case StartMining =>
      if (!isMining && ergoSettings.nodeSettings.mining) {
        logger.info("Starting Mining")
        isMining = true
        context.system.scheduler.scheduleOnce(5.second) {
          produceCandidate(readersHolderRef, ergoSettings, nodeId).foreach(_.foreach(c => {
            self ! c
            self ! MineBlock
          }))
        }
      }

    case c: CandidateBlock =>
      val oldHeight = candidateOpt.flatMap(_.parentOpt).map(_.height).getOrElse(0)
      val newHeight = c.parentOpt.map(_.height).getOrElse(0)
      logger.debug(s"New candidate $c. Height change $oldHeight -> $newHeight")
      if (newHeight > oldHeight) {
        nonce = 0
      }
      candidateOpt = Some(c)

    case MineBlock =>
      nonce = nonce + 1
      candidateOpt match {
        case Some(candidate) =>
          logger.info(s"Trying to prove block with parent ${candidate.parentOpt.map(_.encodedId)} and nonce $nonce")
          //Mine in separate thread for faster responses to API requests and new candidate block application
          Future(powScheme.proveBlock(candidate, nonce)).onComplete {
            case Success(Some(newBlock)) =>
              logger.info("New block found: " + newBlock)

              viewHolderRef ! LocallyGeneratedModifier(newBlock.header)
              viewHolderRef ! LocallyGeneratedModifier(newBlock.blockTransactions)
              newBlock.aDProofs.foreach { adp =>
                viewHolderRef ! LocallyGeneratedModifier(adp)
              }
              context.system.scheduler.scheduleOnce(ergoSettings.nodeSettings.miningDelay)(self ! MineBlock)
            case _ =>
              self ! MineBlock
          }
        case None =>
          //Waiting until we'll create our first candidate block
          context.system.scheduler.scheduleOnce(1.second)(self ! MineBlock)
      }

    case MiningStatusRequest =>
      sender ! MiningStatusResponse(isMining, votes, candidateOpt)

    case m =>
      logger.warn(s"Unexpected message $m")
  }

  //TODO rewrite from readers when state.proofsForTransactions will be ready
  def produceCandidate(readersHolderRef: ActorRef, ergoSettings: ErgoSettings, nodeId: Array[Byte]): Future[Option[CandidateBlock]] = {
    implicit val timeout = Timeout(ergoSettings.scorexSettings.restApi.timeout)
    (viewHolderRef ? GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[CandidateBlock]] { v =>
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
          val txs = coinbase +: state.filterValid(pool.take(10).toSeq)

          //we also filter transactions which are trying to spend the same box. Currently, we pick just the first one
          //of conflicting transaction. Another strategy is possible(e.g. transaction with highest fee)
          //todo: move this logic to MemPool.put? Problem we have now is that conflicting transactions are still in
          // the pool
          val txsNoConflict = txs.foldLeft((Seq.empty[AnyoneCanSpendTransaction], Set.empty[ByteArrayWrapper])) { case ((s, keys), tx) =>
            val bxsBaw = tx.boxIdsToOpen.map(ByteArrayWrapper.apply)
            if (bxsBaw.forall(k => !keys.contains(k)) && bxsBaw.size == bxsBaw.toSet.size) {
              (s :+ tx) -> (keys ++ bxsBaw)
            } else {
              (s, keys)
            }
          }._1

          val (adProof, adDigest) = state.proofsForTransactions(txsNoConflict).get

          val timestamp = timeProvider.time()
          val nBits = bestHeaderOpt.map(parent => history.requiredDifficultyAfter(parent))
            .map(d => RequiredDifficulty.encodeCompactBits(d)).getOrElse(Constants.InitialNBits)
          val candidate = CandidateBlock(bestHeaderOpt, nBits, adDigest, adProof, txsNoConflict, timestamp, nodeId)
          logger.debug(s"Send candidate block with ${candidate.transactions.length} transactions")
          //TODO takes a lot of time
          candidate

        }.recoverWith { case thr =>
          logger.warn("Error when trying to generate a block: ", thr)
          Failure(thr)
        }.toOption
      } else {
        //Do not try to mine genesis block when offlineGeneration = false
        None
      }
    }).mapTo[Option[CandidateBlock]]
  }

}


object ErgoMiner {

  case object StartMining

  case object MineBlock

  case object MiningStatusRequest

  case class MiningStatusResponse(isMining: Boolean, votes: Array[Byte], candidateBlock: Option[CandidateBlock]) {
    lazy val json: Json = Map(
      "isMining" -> isMining.asJson,
      "votes" -> Algos.encode(votes).asJson,
      "candidateBlock" -> candidateBlock.map(_.json).getOrElse("None".asJson)
    ).asJson
  }
}
