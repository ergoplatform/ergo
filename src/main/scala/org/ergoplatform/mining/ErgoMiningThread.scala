package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.pattern.StatusReply
import org.ergoplatform.{InputBlockFound, InputSolutionFound, NothingFound, OrderingBlockFound, OrderingSolutionFound}
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import scorex.util.ScorexLogging
import scorex.util.encode.Base16

import scala.concurrent.duration._
import scala.util.Random

/** ErgoMiningThread is a scala implementation of a miner using just CPU.
  * It tries to mimic GPU miner's behavior as to polling for new Candidates
  * and submitting solutions. Note that it is useful only for low mining difficulty
  * as its hashrate is just 1000 h/s */
class ErgoMiningThread(
  ergoSettings: ErgoSettings,
  candidateGenerator: ActorRef,
  sk: PrivateKey
) extends Actor
  with ScorexLogging {

  import org.ergoplatform.mining.ErgoMiningThread._

  private val powScheme = ergoSettings.chainSettings.powScheme
  private val NonceStep = 1000
  // true while a MineCmd is queued: at most one is ever in the mailbox, so a new candidate cannot start a second
  // chain of MineCmd steps beside the running one (MineCmd is private, so no other sender can either)
  private var mineCmdPending = false

  private def enqueueMineCmdIfIdle(): Unit = {
    if (!mineCmdPending) {
      mineCmdPending = true
      self ! MineCmd
    }
  }

  override def preStart(): Unit = {
    log.info(s"Starting miner thread: ${self.path.name}")
    // poll for new candidate periodically
    context.system.scheduler.scheduleWithFixedDelay(
      1.second,
      ergoSettings.nodeSettings.internalMinerPollingInterval,
      candidateGenerator,
      GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None)
    )(context.dispatcher, self)
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(s"Attempted mining thread restart due to ${reason.getMessage}", reason)
    super.preRestart(reason, message)
  }

  override def postStop(): Unit =
    log.info(s"Stopping miner thread: ${self.path.name}")

  override def receive: Receive = {
    case StatusReply.Success(Candidate(candidateBlock, externalVersion, _, parameters)) =>
      log.info(s"Initiating block mining")
      context.become(mining(nonce = 0, candidateBlock, externalVersion.msg.clone(), parameters, solvedBlocksCount = 0))
      enqueueMineCmdIfIdle()
    case StatusReply.Error(ex) =>
      log.error(s"Preparing candidate did not succeed", ex)
  }

  def mining(
    nonce: Int,
    candidateBlock: CandidateBlock,
    workMsg: Array[Byte],
    parameters: Parameters,
    solvedBlocksCount: Int
  ): Receive = {
    case StatusReply.Success(Candidate(cb, externalVersion, _, newParameters)) =>
      // if we get new candidate instead of a cached one, mine it: new work is a new PoW message, as a regenerated
      // candidate can keep the timestamp (a parent dated ahead of the local clock)
      if (!java.util.Arrays.equals(externalVersion.msg, workMsg)) {
        log.info(s"Switching block mining work to msg ${Base16.encode(externalVersion.msg)}")
        context.become(mining(nonce = 0, cb, externalVersion.msg.clone(), newParameters, solvedBlocksCount))
        enqueueMineCmdIfIdle()
      }
    case StatusReply.Error(ex) =>
      // the generator rejects a solution only when its candidate does not fit it, is gone or is solved already, so
      // no search resumes on this work: the thread mines again when a poll returns a candidate with a new PoW message
      log.error(s"Accepting solution or preparing candidate did not succeed", ex)
    case StatusReply.Success(()) =>
      log.info(s"Solution accepted")
      context.become(mining(nonce, candidateBlock, workMsg, parameters, solvedBlocksCount + 1))
    case MineCmd =>
      mineCmdPending = false
      val lastNonceToCheck = nonce + NonceStep
      powScheme.proveCandidate(candidateBlock, sk, nonce, lastNonceToCheck, parameters) match {
        case OrderingBlockFound(newBlock) =>
          log.info(s"Found solution for ordering block, sending it for validation")
          candidateGenerator ! OrderingSolutionFound(newBlock.header.powSolution)
        case InputBlockFound(newBlock) =>
          log.info(s"Found solution for input block, sending it for validation")
          candidateGenerator ! InputSolutionFound(newBlock.header.powSolution)
        case NothingFound =>
          log.info(s"Trying nonce $lastNonceToCheck")
          context.become(mining(lastNonceToCheck, candidateBlock, workMsg, parameters, solvedBlocksCount))
          enqueueMineCmdIfIdle()
        case _ =>
          //todo : rework ProveBlockResult hierarchy to avoid this branch
      }
    case GetSolvedBlocksCount =>
      sender() ! SolvedBlocksCount(solvedBlocksCount)
  }

}

object ErgoMiningThread {

  private case object MineCmd
  case object GetSolvedBlocksCount // metric just for testing purposes for now
  case class SolvedBlocksCount(count: Int)

  private def props(ergoSettings: ErgoSettings, minerRef: ActorRef, sk: BigInt): Props =
    Props(new ErgoMiningThread(ergoSettings, minerRef, sk))

  def apply(ergoSettings: ErgoSettings, minerRef: ActorRef, sk: BigInt)(
    implicit context: ActorRefFactory
  ): ActorRef =
    context.actorOf(props(ergoSettings, minerRef, sk), s"ErgoMiningThread-${Random.alphanumeric.take(5).mkString}")

}
