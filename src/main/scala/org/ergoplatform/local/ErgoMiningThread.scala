package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.local.ErgoMiningThread.MineBlock
import org.ergoplatform.mining.CandidateBlock
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core.utils.ScorexLogging

import scala.concurrent.ExecutionContext
import scala.util.Random

class ErgoMiningThread(ergoSettings: ErgoSettings,
                       viewHolderRef: ActorRef,
                       startCandidate: CandidateBlock) extends Actor with ScorexLogging {

  implicit val ec: ExecutionContext = context.dispatcher

  private val powScheme = ergoSettings.chainSettings.powScheme
  private var candidate: CandidateBlock = startCandidate

  protected def mineCmd(nonce: Long): Unit =
    context.system.scheduler.scheduleOnce(ergoSettings.nodeSettings.miningDelay) { self ! MineBlock(nonce) }


  override def preStart(): Unit = {
    log.debug(s"Starting miner thread: ${self.path.name}")
    mineCmd(Random.nextLong())
  }

  override def postStop(): Unit = log.debug(s"Stopping miner thread: ${self.path.name}")

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
          if (ergoSettings.nodeSettings.stateType == StateType.Digest) {
            newBlock.aDProofs.foreach { adp =>
              viewHolderRef ! LocallyGeneratedModifier(adp)
            }
          }
          mineCmd(Random.nextLong())
        case _ =>
          self ! MineBlock(nonce + 1)
      }
  }
}

object ErgoMiningThread {
  def props(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock): Props =
    Props(new ErgoMiningThread(ergoSettings, viewHolderRef, startCandidate))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, startCandidate))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, startCandidate), name)

  case class MineBlock(nonce: Long)
}
