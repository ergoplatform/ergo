package org.ergoplatform.local

import akka.actor.{Actor, ActorRef}
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.local.ErgoMiner._
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.CandidateBlock
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder
import scorex.core.NodeViewHolder.{GetDataFromCurrentView, SemanticallySuccessfulModifier, Subscribe}
import scorex.core.utils.{NetworkTime, ScorexLogging}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Try}


class ErgoMiner(ergoSettings: ErgoSettings, viewHolder: ActorRef) extends Actor with ScorexLogging {

  private var isMining = false
  private var nonce = 0
  private var candidateOpt: Option[CandidateBlock] = None

  private val powScheme = ergoSettings.chainSettings.poWScheme
  private val startTime = NetworkTime.time()

  override def preStart(): Unit = {
    viewHolder ! Subscribe(Seq(NodeViewHolder.EventType.SuccessfulSemanticallyValidModifier))
  }


  override def receive: Receive = {
    case SemanticallySuccessfulModifier(mod) =>
      if (isMining) {
        mod match {
          case f: ErgoFullBlock if !candidateOpt.flatMap(_.parentOpt).exists(_.id sameElements f.header.id) =>
            produceCandidate()

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
        log.info("Starting Mining")
        isMining = true
        produceCandidate()
        self ! MineBlock
      }

    case cOpt: Option[CandidateBlock] =>
      cOpt.foreach { c =>
        log.debug(s"New candidate $c")
        if (c.parentOpt.map(_.height).getOrElse(0) > candidateOpt.flatMap(_.parentOpt).map(_.height).getOrElse(0)) {
          nonce = 0
        }
        candidateOpt = Some(c)
      }

    case MineBlock =>
      nonce = nonce + 1
      candidateOpt match {
        case Some(candidate) =>
          powScheme.proveBlock(candidate, nonce) match {
            case Some(newBlock) =>
              log.info("New block found: " + newBlock)

              viewHolder ! LocallyGeneratedModifier(newBlock.header)
              viewHolder ! LocallyGeneratedModifier(newBlock.blockTransactions)
              newBlock.aDProofs.foreach { adp =>
                viewHolder ! LocallyGeneratedModifier(adp)
              }
              context.system.scheduler.scheduleOnce(100.millis)(self ! MineBlock)
            case None =>
              self ! MineBlock
          }
        case None =>
          context.system.scheduler.scheduleOnce(1.second)(self ! MineBlock)
      }
  }

  private def produceCandidate(): Unit = {
    viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[CandidateBlock]] { v =>
      val bestHeaderOpt = v.history.bestFullBlockOpt.map(_.header)
      if (bestHeaderOpt.isDefined || ergoSettings.nodeSettings.offlineGeneration) {

        val coinbase: AnyoneCanSpendTransaction = {
          val txBoxes = v.state.anyoneCanSpendBoxesAtHeight(bestHeaderOpt.map(_.height + 1).getOrElse(0))
          AnyoneCanSpendTransaction(txBoxes.map(_.nonce), txBoxes.map(_.value))
        }

        Try {
          //only transactions valid from against the current utxo state we take from the mem pool
          //todo: move magic number to testnet settings
          val txs = coinbase +: v.state.filterValid(v.pool.take(1000).toSeq)

          //we also filter transactions which are trying to spend the same box. Currently, we pick just the first one
          //of conflicting transaction. Another strategy is possible(e.g. transaction with highest fee)
          //todo: move this logic to MemPool.put? Problem we have now is that conflicting transactions are still in
          // the pool
          val txsNoConflict = txs.foldLeft((Seq[AnyoneCanSpendTransaction](), Set[ByteArrayWrapper]())) { case ((s, keys), tx) =>
            val bxsBaw = tx.boxIdsToOpen.map(ByteArrayWrapper.apply)
            if (bxsBaw.forall(k => !keys.contains(k)) && bxsBaw.size == bxsBaw.toSet.size) {
              (s :+ tx) -> (keys ++ bxsBaw)
            } else {
              (s, keys)
            }
          }._1

          val (adProof, adDigest) = v.state.proofsForTransactions(txsNoConflict).get

          val timestamp = NetworkTime.time()
          val votes = ergoSettings.scorexSettings.network.nodeName.map(_.toByte).takeRight(5).toArray
          val nBits = bestHeaderOpt.map(parent => v.history.requiredDifficultyAfter(parent))
            .map(d => RequiredDifficulty.encodeCompactBits(d)).getOrElse(Constants.InitialNBits)
          CandidateBlock(bestHeaderOpt, nBits, adDigest, adProof, txsNoConflict, timestamp, votes)

        }.recoverWith { case thr =>
          log.warn("Error when trying to generate a block: ", thr)
          Failure(thr)
        }.toOption
      } else {
        //Do not try to mine genesis block when offlineGeneration = false
        None
      }
    }
  }
}


object ErgoMiner {

  case object StartMining

  case object MineBlock

}