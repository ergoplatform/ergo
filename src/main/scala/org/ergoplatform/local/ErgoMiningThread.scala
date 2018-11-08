package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.local.ErgoMiningThread.MineBlock
import org.ergoplatform.mining.{CandidateBlock, PrivateKey}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.ErgoSettings
import scorex.core.NodeViewHolder.ReceivableMessages.LocallyGeneratedModifier
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContext


class ErgoMiningThread(ergoSettings: ErgoSettings,
                       viewHolderRef: ActorRef,
                       startCandidate: CandidateBlock,
                       sk: PrivateKey,
                       timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  implicit val ec: ExecutionContext = context.dispatcher

  private val powScheme = ergoSettings.chainSettings.powScheme
  private var candidate: CandidateBlock = startCandidate

  protected def mineCmd(): Unit =
    context.system.scheduler.scheduleOnce(ergoSettings.nodeSettings.miningDelay) {
      self ! MineBlock
    }


  override def preStart(): Unit = {
    log.debug(s"Starting miner thread: ${self.path.name}")
    mineCmd()
  }

  override def postStop(): Unit = log.debug(s"Stopping miner thread: ${self.path.name}")

  override def receive: Receive = {
    case newCandidate: CandidateBlock =>
      candidate = newCandidate

    case MineBlock =>
      // timestamp is increased for at least 1 as a role of a nonce
      val newTimestamp = Math.max(candidate.timestamp + 1, timeProvider.time())
      candidate = candidate.copy(timestamp = newTimestamp)
      log.info(s"Trying to prove block with parent ${candidate.parentOpt.map(_.encodedId)} and timestamp $newTimestamp " +
        s"containing ${candidate.transactions.size} transactions")
      powScheme.proveCandidate(candidate, sk) match {
        case Some(newBlock) =>
          log.info(s"New block ${newBlock.id} with ${newBlock.transactions.size} transactions found")

          viewHolderRef ! LocallyGeneratedModifier(newBlock.header)
          val sectionsToApply = if (ergoSettings.nodeSettings.stateType == StateType.Digest) {
            newBlock.blockSections
          } else {
            newBlock.mandatoryBlockSections
          }

          sectionsToApply.foreach(s => viewHolderRef ! LocallyGeneratedModifier(s))
          mineCmd()
        case _ =>
          self ! MineBlock
      }
  }
}

object ErgoMiningThread {
  def props(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock,
            sk: BigInt,
            timeProvider: NetworkTimeProvider): Props =
    Props(new ErgoMiningThread(ergoSettings, viewHolderRef, startCandidate, sk, timeProvider))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock,
            sk: BigInt,
            timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, startCandidate, sk, timeProvider))

  def apply(ergoSettings: ErgoSettings,
            viewHolderRef: ActorRef,
            startCandidate: CandidateBlock,
            sk: BigInt,
            timeProvider: NetworkTimeProvider,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, viewHolderRef, startCandidate, sk, timeProvider), name)

  case object MineBlock

}
