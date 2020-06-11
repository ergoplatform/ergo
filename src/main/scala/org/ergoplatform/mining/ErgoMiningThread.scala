package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.mining.ErgoMiningThread.MineBlock
import org.ergoplatform.settings.ErgoSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ScorexLogging

import scala.concurrent.ExecutionContext

class ErgoMiningThread(ergoSettings: ErgoSettings,
                       minerRef: ActorRef,
                       startCandidate: CandidateBlock,
                       sk: PrivateKey,
                       timeProvider: NetworkTimeProvider) extends Actor with ScorexLogging {

  implicit val ec: ExecutionContext = context.dispatcher

  private val powScheme = ergoSettings.chainSettings.powScheme
  private val NonceStep = 1000
  private var candidate: CandidateBlock = startCandidate
  private var nonce: Long = 0

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
      nonce = 0

    case MineBlock =>
      val lastNonceToCheck = nonce + NonceStep
      powScheme.proveCandidate(candidate, sk, nonce, lastNonceToCheck) match {
        case Some(newBlock) =>
          minerRef ! newBlock.header.powSolution
          mineCmd()
        case _ =>
          nonce = lastNonceToCheck
          self ! MineBlock
      }
  }

}

object ErgoMiningThread {

  def props(ergoSettings: ErgoSettings,
            minerRef: ActorRef,
            startCandidate: CandidateBlock,
            sk: BigInt,
            timeProvider: NetworkTimeProvider): Props =
    Props(new ErgoMiningThread(ergoSettings, minerRef, startCandidate, sk, timeProvider))

  def apply(ergoSettings: ErgoSettings,
            minerRef: ActorRef,
            startCandidate: CandidateBlock,
            sk: BigInt,
            timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(ergoSettings, minerRef, startCandidate, sk, timeProvider))

  case object MineBlock

}
