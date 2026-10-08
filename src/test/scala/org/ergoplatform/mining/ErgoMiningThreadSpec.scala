package org.ergoplatform.mining

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.StatusReply
import akka.testkit.{TestKit, TestProbe}
import com.google.common.primitives.Longs
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader, Parameters}
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.{AutolykosSolution, InputBlockHeaderFound, InputSolutionFound, NothingFound,
  OrderingBlockHeaderFound, ProveBlockResult, SolutionFound}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.concurrent.duration._

class ErgoMiningThreadSpec extends AnyFlatSpec with Matchers with ErgoTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  // the periodic poll fires 1 s after start and then once an hour, so any later poll in a test is the thread's own
  private val settings: ErgoSettings = {
    val empty = ErgoSettingsReader.read()
    empty.copy(
      nodeSettings = empty.nodeSettings.copy(
        mining = true,
        stateType = StateType.Utxo,
        internalMinerPollingInterval = 1.hour,
        offlineGeneration = true,
        verifyTransactions = true
      ),
      chainSettings = empty.chainSettings.copy(blockInterval = 1.seconds)
    )
  }

  /** Fake PoW with an input-block hit at the first of `hits` in [minNonce, maxNonce), reporting the hit as the
    * solution's nonce (or `reportedNonce(hit)` instead), so the miner's nonce search is observable. Every call's
    * bounds go to `calls`, if given. */
  private class FixedHitsPowScheme(hits: Seq[Long],
                                   calls: Option[ActorRef] = None,
                                   reportedNonce: Long => Long = identity) extends DefaultFakePowScheme(32, 26) {
    override def prove(parentOpt: Option[Header],
                       version: Header.Version,
                       nBits: Long,
                       stateRoot: ADDigest,
                       adProofsRoot: Digest32,
                       transactionsRoot: Digest32,
                       timestamp: Header.Timestamp,
                       extensionHash: Digest32,
                       votes: Array[Byte],
                       sk: PrivateKey,
                       minNonce: Long,
                       maxNonce: Long,
                       parameters: Parameters): ProveBlockResult = {
      calls.foreach(_ ! ((minNonce, maxNonce)))
      hits.find(h => h >= minNonce && h < maxNonce) match {
        case None => NothingFound
        case Some(hit) =>
          super.prove(parentOpt, version, nBits, stateRoot, adProofsRoot, transactionsRoot, timestamp, extensionHash,
            votes, sk, minNonce, maxNonce, parameters) match {
            case OrderingBlockHeaderFound(h) =>
              val s = h.powSolution
              val n = Longs.toByteArray(reportedNonce(hit))
              InputBlockHeaderFound(h.copy(powSolution = new AutolykosSolution(s.pk, s.w, n, s.d)))
            case other => other
          }
      }
    }
  }

  /** A real candidate, built by a real generator on a chain of its own. */
  private def realCandidate(name: String)(implicit system: ActorSystem): Candidate = {
    val chainSettings = settings.copy(directory = s"${settings.directory}-$name-${System.nanoTime()}")
    val viewHolderRef: ActorRef = ErgoNodeViewRef(chainSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)
    val realGenerator: ActorRef =
      CandidateGenerator(defaultMinerSecret.publicImage, readersHolderRef, viewHolderRef, chainSettings)
    val candidateProbe = TestProbe()
    realGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), candidateProbe.ref)
    // the first request waits for the genesis state, which can take a while on a loaded machine
    candidateProbe.expectMsgPF(60.seconds) { case StatusReply.Success(c: Candidate) => c }
  }

  private def nonceOf(s: SolutionFound): Long = Longs.fromByteArray(s.as.n)

  /** Starts a thread on `candidate`; a probe stands in for the generator. */
  private def startThread(candidate: Candidate, pow: AutolykosPowScheme)
                         (implicit system: ActorSystem): (ActorRef, TestProbe) = {
    val generator = TestProbe()
    val thread = ErgoMiningThread(settings.copy(chainSettings = settings.chainSettings.copy(powScheme = pow)),
      generator.ref, defaultMinerSecret.w)
    generator.expectMsgClass(5.seconds, classOf[GenerateCandidate])
    generator.reply(StatusReply.Success(candidate))
    (thread, generator)
  }

  // Presumes the generator on the weak-blocks tip: a rejected solution clears its candidate, and every later solution
  // is answered "Block already solved : None" until a poll makes a new one.
  it should "re-poll after a rejected solution and switch to the generator's new candidate" in new TestKit(ActorSystem()) {
    val candidate = realCandidate("drop")
    val (thread, generator) = startThread(candidate, new FixedHitsPowScheme(Seq(5L, 900L)))
    val alreadySolved = StatusReply.Error(new Exception("Block already solved : None"))

    val first = generator.expectMsgType[InputSolutionFound](10.seconds)
    nonceOf(first) shouldBe 5L
    thread.tell(StatusReply.Error(new Exception("Invalid input block! PoW valid: false")), generator.ref)

    // the thread asks for new work at once (the periodic poll is an hour away); solutions meanwhile are refused
    generator.fishForMessage(5.seconds, hint = "a poll for new work after the rejected solution") {
      case _: GenerateCandidate => true
      case _: SolutionFound => generator.reply(alreadySolved); false
    }
    val newBlock = candidate.candidateBlock.copy(timestamp = candidate.candidateBlock.timestamp + 1)
    val newCandidate = candidate.copy(candidateBlock = newBlock)
    generator.reply(StatusReply.Success(newCandidate))

    // the next solution is the new candidate's first hit; one stale hit on the old candidate may still be in flight
    var staleHits = 0
    val next = generator.fishForMessage(10.seconds, hint = "a solution on the new candidate") {
      case _: GenerateCandidate => generator.reply(StatusReply.Success(newCandidate)); false
      case s: SolutionFound if nonceOf(s) == 900L && staleHits == 0 =>
        staleHits += 1; generator.reply(alreadySolved); false
      case _: SolutionFound => true
    }.asInstanceOf[SolutionFound]
    nonceOf(next) shouldBe 5L
    system.terminate()
  }

  // Presumes a generator that keeps its candidate when it rejects a solution (#2638 option 2): the re-poll returns
  // the same candidate, and the thread searches on after the rejected nonce.
  it should "not resubmit a rejected solution's nonce while the generator keeps the candidate" in
    new TestKit(ActorSystem()) {
    val candidate = realCandidate("keep")
    val (thread, generator) = startThread(candidate, new FixedHitsPowScheme(Seq(5L, 900L)))

    val first = generator.expectMsgType[InputSolutionFound](10.seconds)
    nonceOf(first) shouldBe 5L
    thread.tell(StatusReply.Error(new Exception("Invalid input block! PoW valid: false")), generator.ref)

    val next = generator.fishForMessage(10.seconds) {
      case _: GenerateCandidate => generator.reply(StatusReply.Success(candidate)); false
      case _: SolutionFound => true
    }.asInstanceOf[SolutionFound]
    nonceOf(next) shouldBe 900L
    system.terminate()
  }

  it should "resume after a rejected nonce past Int.MaxValue without truncating or wrapping" in
    new TestKit(ActorSystem()) {
    val candidate = realCandidate("long")
    val calls = TestProbe()
    // the hit in the first batch is reported at nonce Int.MaxValue
    val pow = new FixedHitsPowScheme(Seq(5L), Some(calls.ref), _ => Int.MaxValue.toLong)
    val (thread, generator) = startThread(candidate, pow)

    calls.expectMsg(10.seconds, (0L, 1000L))
    nonceOf(generator.expectMsgType[InputSolutionFound](10.seconds)) shouldBe Int.MaxValue.toLong
    thread.tell(StatusReply.Error(new Exception("Invalid input block! PoW valid: false")), generator.ref)

    // the next batch starts right after the rejected nonce, as a Long
    calls.expectMsg(10.seconds, (Int.MaxValue.toLong + 1, Int.MaxValue.toLong + 1001))
    system.terminate()
  }
}
