package org.ergoplatform.mining

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import akka.pattern.StatusReply
import com.google.common.primitives.Longs
import org.ergoplatform.{AutolykosSolution, InputBlockFound, InputSolutionFound, NothingFound, OrderingBlockFound, OrderingSolutionFound}
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import scorex.util.ScorexLogging

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
  private val PollCandidate = GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None)
  // solutions sent and not answered yet: an error reply while one is outstanding answers a solution, otherwise a poll
  private var solutionsAwaitingReply = 0
  // true while a MineCmd is queued: at most one is ever in the mailbox, so a new candidate or an error reply cannot
  // start a second chain of MineCmd steps beside the running one (MineCmd is private, so no other sender can either)
  private var mineCmdPending = false
  // true from an error reply to a solution until a candidate reply: the generator may no longer hold this thread's
  // candidate, and a hit on it would then be judged against the generator's next candidate and clear that one too
  private var awaitingCandidate = false

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
      PollCandidate
    )(context.dispatcher, self)
  }

  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    log.error(s"Attempted mining thread restart due to ${reason.getMessage}", reason)
    super.preRestart(reason, message)
  }

  override def postStop(): Unit =
    log.info(s"Stopping miner thread: ${self.path.name}")

  override def receive: Receive = {
    case StatusReply.Success(Candidate(candidateBlock, _, _, parameters)) =>
      log.info(s"Initiating block mining")
      context.become(mining(nonce = 0, candidateBlock, parameters, solvedBlocksCount = 0))
      enqueueMineCmdIfIdle()
    case StatusReply.Error(ex) =>
      log.error(s"Preparing candidate did not succeed", ex)
  }

  def mining(
    nonce: Long,
    candidateBlock: CandidateBlock,
    parameters: Parameters,
    solvedBlocksCount: Int
  ): Receive = {
    case StatusReply.Success(Candidate(cb, _, _, newParameters)) =>
      // if we get new candidate instead of a cached one, mine it
      if (cb.timestamp != candidateBlock.timestamp) {
        awaitingCandidate = false
        context.become(mining(nonce = 0, cb, newParameters, solvedBlocksCount))
        enqueueMineCmdIfIdle()
      } else if (awaitingCandidate) {
        // the generator kept this candidate after the rejection: search on after the rejected nonce
        awaitingCandidate = false
        enqueueMineCmdIfIdle()
      }
    case StatusReply.Error(ex) if solutionsAwaitingReply > 0 =>
      solutionsAwaitingReply -= 1
      log.error(s"Accepting solution did not succeed", ex)
      // the generator may have dropped this candidate: poll now and hash nothing until the reply says whether it
      // has a new one or kept this one (then the search resumes after the rejected nonce, recorded when it was found)
      awaitingCandidate = true
      candidateGenerator ! PollCandidate
    case StatusReply.Error(ex) =>
      log.error(s"Preparing candidate did not succeed", ex)
    case StatusReply.Success(()) =>
      log.info(s"Solution accepted")
      solutionsAwaitingReply = math.max(0, solutionsAwaitingReply - 1)
      context.become(mining(nonce, candidateBlock, parameters, solvedBlocksCount + 1))
    case MineCmd if awaitingCandidate =>
      mineCmdPending = false
    case MineCmd =>
      mineCmdPending = false
      val lastNonceToCheck = nonce + NonceStep
      powScheme.proveCandidate(candidateBlock, sk, nonce, lastNonceToCheck, parameters) match {
        case OrderingBlockFound(newBlock) =>
          log.info(s"Found solution for ordering block, sending it for validation")
          solutionSent(newBlock.header.powSolution, candidateBlock, parameters, solvedBlocksCount)
          candidateGenerator ! OrderingSolutionFound(newBlock.header.powSolution)
        case InputBlockFound(newBlock) =>
          log.info(s"Found solution for input block, sending it for validation")
          solutionSent(newBlock.header.powSolution, candidateBlock, parameters, solvedBlocksCount)
          candidateGenerator ! InputSolutionFound(newBlock.header.powSolution)
        case NothingFound =>
          log.info(s"Trying nonce $lastNonceToCheck")
          context.become(mining(lastNonceToCheck, candidateBlock, parameters, solvedBlocksCount))
          enqueueMineCmdIfIdle()
        case _ =>
          //todo : rework ProveBlockResult hierarchy to avoid this branch
      }
    case GetSolvedBlocksCount =>
      sender() ! SolvedBlocksCount(solvedBlocksCount)
  }

  // the search resumes after the found nonce if the solution is rejected
  private def solutionSent(solution: AutolykosSolution,
                           candidateBlock: CandidateBlock,
                           parameters: Parameters,
                           solvedBlocksCount: Int): Unit = {
    solutionsAwaitingReply += 1
    context.become(mining(Longs.fromByteArray(solution.n) + 1, candidateBlock, parameters, solvedBlocksCount))
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
