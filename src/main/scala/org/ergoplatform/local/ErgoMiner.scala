package org.ergoplatform.local

import akka.actor.{Actor, ActorRef}
import io.iohk.iodb.ByteArrayWrapper
import org.ergoplatform.local.ErgoMiner.{MineBlock, ProduceCandidate, StartMining, StopMining}
import org.ergoplatform.modifiers.history.CandidateBlock
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.utils.ScorexLogging

import scala.util.{Failure, Try}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global


class ErgoMiner(ergoSettings: ErgoSettings, viewHolder: ActorRef) extends Actor with ScorexLogging {

  private var mining = false
  private var startingNonce = Long.MinValue

  private val powScheme = ergoSettings.chainSettings.poWScheme

  override def preStart(): Unit = {
  }

  override def receive: Receive = {
    case StartMining =>
      log.info("Starting Mining")
      self ! ProduceCandidate
      mining = true

    case ProduceCandidate =>
      viewHolder ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, Option[CandidateBlock]] { v =>
        if (v.pool.size > 0) {
          Try {
            //only transactions valid from against the current utxo state we take from the mem pool
            //todo: move magic number to testnet settings
            val txs = v.state.filterValid(v.pool.take(1000).toSeq)

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

            val bxsbaw = txsNoConflict.flatMap(_.boxIdsToOpen).map(k => ByteArrayWrapper(k))
            println("seq size: " + bxsbaw.size + " set size: " + bxsbaw.toSet.size)

            val (adProof, adDigest) = v.state.proofsForTransactions(txsNoConflict).get

            val timestamp = System.currentTimeMillis()
            val votes = Array.fill(5)(0: Byte)
            CandidateBlock(v.history.bestHeaderOpt, Constants.InitialNBits, adDigest,
              adProof, txsNoConflict, timestamp, votes)
          }.recoverWith { case thr =>
            log.warn("Error when trying to generate a block: ", thr)
            Failure(thr)
          }.toOption
        } else None
      }

    case candidateOpt: Option[CandidateBlock] =>
      candidateOpt match {
        case Some(candidateBlock) =>
          startingNonce = Long.MinValue
          self ! MineBlock(candidateBlock)
        case None =>
          context.system.scheduler.scheduleOnce(100.millis)(self ! ProduceCandidate)
      }

    case StopMining =>
      mining = false

    case MineBlock(candidate) =>
      val start = startingNonce
      val finish = start + 10
      startingNonce = finish

      log.info(s"Trying nonces from $start till $finish")

      powScheme.proveBlock(candidate, start, finish) match {
        case Some(newBlock) =>
          log.info("New block found: " + newBlock)

          viewHolder ! LocallyGeneratedModifier(newBlock.header)
          viewHolder ! LocallyGeneratedModifier(newBlock.blockTransactions)
          newBlock.aDProofs.foreach { adp =>
            viewHolder ! LocallyGeneratedModifier(adp)
          }

          context.system.scheduler.scheduleOnce(100.millis)(self ! ProduceCandidate)
        case None =>
          self ! ProduceCandidate
      }
  }
}


object ErgoMiner {

  case object StartMining

  case object ProduceCandidate

  case object StopMining

  case class MineBlock(candidate: CandidateBlock)

}