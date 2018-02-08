package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, Props}
import org.ergoplatform.local.ErgoMiningThread.MineBlock
import org.ergoplatform.mining.CandidateBlock
import org.ergoplatform.settings.ErgoSettings
import scorex.core.LocalInterface.LocallyGeneratedModifier
import scorex.core.utils.ScorexLogging
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.Random

class ErgoMiningThread(ergoSettings: ErgoSettings,
                       viewHolderRef: ActorRef,
                       startCandidate: CandidateBlock) extends Actor with ScorexLogging {

  private val powScheme = ergoSettings.chainSettings.poWScheme
  private var candidate: CandidateBlock = startCandidate


  override def preStart(): Unit = {
    context.system.scheduler.scheduleOnce(ergoSettings.nodeSettings.miningDelay) (self ! MineBlock(Random.nextLong()))
  }

  override def receive: Receive = {
    case newCandidate: CandidateBlock =>
      candidate = newCandidate

    case MineBlock(nonce) =>
      log.info(s"Trying to prove block with parent ${candidate.parentOpt.map(_.encodedId)} and nonce $nonce")
      powScheme.proveBlock(candidate, nonce) match {
        case Some(newBlock) =>
          log.info("New block found: " + newBlock)

          viewHolderRef ! LocallyGeneratedModifier(newBlock.header)
          viewHolderRef ! LocallyGeneratedModifier(newBlock.blockTransactions)
          if (ergoSettings.nodeSettings.ADState) {
            newBlock.aDProofs.foreach { adp =>
              viewHolderRef ! LocallyGeneratedModifier(adp)
            }
          }
          context.system.scheduler.scheduleOnce(ergoSettings.nodeSettings.miningDelay) {
            self ! MineBlock(Random.nextLong())
          }
        case _ =>
          self ! MineBlock(nonce + 1)
      }
  }
}

object ErgoMiningThread {

  case class MineBlock(nonce: Long)

  def props(ergoSettings: ErgoSettings, viewHolderRef: ActorRef, startCandidate: CandidateBlock): Props = {
    Props(new ErgoMiningThread(ergoSettings, viewHolderRef, startCandidate))
  }
}