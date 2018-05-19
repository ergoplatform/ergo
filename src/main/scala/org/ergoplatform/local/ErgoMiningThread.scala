package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.local.ErgoMiningThread.MineBlock
import org.ergoplatform.mining.CandidateBlock
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.concurrent.ExecutionContext
import scala.util.Random

class ErgoMiningThread(ergoSettings: ErgoSettings,
                       viewHolderRef: ActorRef,
                       startCandidate: CandidateBlock,
                       timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  implicit val ec: ExecutionContext = context.dispatcher

  private val powScheme = ergoSettings.chainSettings.powScheme
  private var candidate: CandidateBlock = startCandidate

  protected def mineCmd(nonce: Long): Unit =
    context.system.scheduler.scheduleOnce(ergoSettings.nodeSettings.miningDelay) { self ! MineBlock }


  override def preStart(): Unit = {
    log.debug(s"Starting miner thread: ${self.path.name}")
    mineCmd(Random.nextLong())
  }

  override def postStop(): Unit = log.debug(s"Stopping miner thread: ${self.path.name}")

  override def receive: Receive = {
    case newCandidate: CandidateBlock =>
      candidate = newCandidate

    case MineBlock =>
      // timestamp is increased for at least 1 as a role of a nonce
      val newTimestamp = Math.max(candidate.timestamp + 1, timeProvider.time())
      candidate = candidate.copy(timestamp = newTimestamp)
      log.info(s"Trying to prove block with parent ${candidate.parentOpt.map(_.encodedId)} and timestamp $newTimestamp")
      powScheme.proveBlock(candidate) match {
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
          self ! MineBlock
      }
  }
}

object ErgoMiningThread {
  def props(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock,
            timeProvider: NetworkTimeProvider): Props =
    Props(new ErgoMiningThread(ergoSettings, viewHolderRef, startCandidate, timeProvider))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock,
            timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, startCandidate, timeProvider))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock,
            timeProvider: NetworkTimeProvider,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, startCandidate, timeProvider), name)

  case object MineBlock
}
