package org.ergoplatform.mining

import java.util.concurrent.atomic.AtomicBoolean

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.StatusReply
import akka.testkit.{TestKit, TestProbe}
import com.google.common.primitives.{Ints, Longs}
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
import scorex.crypto.hash.{Blake2b256, Digest32}

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
    * solution's nonce (or `reportedNonce(hit)` instead), so the miner's nonce search is observable, and the
    * candidate's timestamp as the solution's `d`, so a solution names the candidate it was found on. Every call's
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
              InputBlockHeaderFound(h.copy(powSolution = new AutolykosSolution(s.pk, s.w, n, dFor(timestamp))))
            case other => other
          }
      }
    }

    protected def dFor(timestamp: Header.Timestamp): BigInt = BigInt(timestamp)
  }

  /** For a real generator: its input-block check accepts a solution only if the solution's `d` is the timestamp of
    * the candidate it is completed with, as real PoW holds only for the header it was found on. The thread's first
    * hit carries a wrong `d`, so the generator rejects it. */
  private class CandidateBoundPowScheme(hits: Seq[Long]) extends FixedHitsPowScheme(hits) {
    private val firstHit = new AtomicBoolean(true)

    override protected def dFor(timestamp: Header.Timestamp): BigInt =
      if (firstHit.getAndSet(false)) BigInt(-1) else BigInt(timestamp)

    override def checkInputBlockPoW(header: Header, parameters: Parameters): Boolean =
      header.powSolution.d == BigInt(header.timestamp)
  }

  /** `c` with its timestamp moved by `k` ms and a PoW message of its own, so it reads as new work either way. */
  private def shifted(c: Candidate, k: Int): Candidate =
    c.copy(
      candidateBlock = c.candidateBlock.copy(timestamp = c.candidateBlock.timestamp + k),
      externalVersion = c.externalVersion.copy(msg = Blake2b256(c.externalVersion.msg ++ Ints.toByteArray(k)))
    )

  private case class AfterRejection(polls: Int, newCandidates: Int, rejected: Int, refused: Int, accepted: Boolean)

  /** Answers the thread as the weak-blocks tip's generator does once it has rejected a solution and cleared its
    * candidate: a poll makes a new candidate if there is none (CandidateGenerator :185-217); a solution found on the
    * cached candidate is accepted, one found on any other is rejected, and either clears the cache (:269-285); with
    * no candidate a solution is refused (:291). New candidates are `shifted(base, firstShift)`, then the next shift.
    * Stops at the first accepted solution, after `maxMessages`, or when the thread is silent for 5 s. */
  private def answerAsTipGenerator(generator: TestProbe, base: Candidate, firstShift: Int,
                                   maxMessages: Int = 20): AfterRejection = {
    var cache: Option[Candidate] = None
    var polls, newCandidates, rejected, refused, messages = 0
    var accepted = false
    while (!accepted && messages < maxMessages) {
      messages += 1
      generator.receiveOne(5.seconds) match {
        case null => messages = maxMessages
        case _: GenerateCandidate =>
          polls += 1
          if (cache.isEmpty) {
            cache = Some(shifted(base, firstShift + newCandidates))
            newCandidates += 1
          }
          generator.reply(StatusReply.Success(cache.get))
        case s: SolutionFound =>
          cache match {
            case None =>
              refused += 1
              generator.reply(StatusReply.Error(new Exception("Block already solved : None")))
            case Some(c) if s.as.d == BigInt(c.candidateBlock.timestamp) =>
              accepted = true
              cache = None
              generator.reply(StatusReply.Success(()))
            case Some(_) =>
              rejected += 1
              cache = None
              generator.reply(StatusReply.Error(new Exception("Invalid input block! PoW valid: false")))
          }
        case other => fail(s"unexpected message to the generator: $other")
      }
    }
    AfterRejection(polls, newCandidates, rejected, refused, accepted)
  }

  private case class ToGenerator(msg: Any)
  private case class ToThread(msg: Any)

  /** Passes every message between the thread and the generator on, reporting it to `events`. */
  private class Relay(generator: ActorRef, events: ActorRef) extends Actor {
    private var thread: Option[ActorRef] = None

    override def receive: Receive = {
      case m if sender() == generator =>
        events ! ToThread(m)
        thread.foreach(_ ! m)
      case m =>
        thread = Some(sender())
        events ! ToGenerator(m)
        generator ! m
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

  // Presumes the generator on the weak-blocks tip: a rejected solution clears its candidate, the next poll makes a new
  // one, and a hit found on the old candidate is judged against the new one, rejected, and clears it as well.
  it should "after a rejected solution, hash nothing until the poll reply, then mine the generator's new candidate" in
    new TestKit(ActorSystem()) {
    val candidate = realCandidate("drop")
    val (thread, generator) = startThread(candidate, new FixedHitsPowScheme(Seq(5L, 900L)))

    nonceOf(generator.expectMsgType[InputSolutionFound](10.seconds)) shouldBe 5L
    thread.tell(StatusReply.Error(new Exception("Invalid input block! PoW valid: false")), generator.ref)

    // the periodic poll is an hour away, so a poll here is the thread's own; one poll and one new candidate, whose
    // first hit is accepted: no hit on the old candidate reaches the generator after the poll
    answerAsTipGenerator(generator, candidate, firstShift = 1) shouldBe
      AfterRejection(polls = 1, newCandidates = 1, rejected = 0, refused = 0, accepted = true)
    system.terminate()
  }

  // As above, with a new candidate arriving while the solution awaits its reply: the rejection was judged against
  // that newer candidate and cleared it, so the search the switch queued must not run on it either.
  it should "not run a search queued before the rejection on the candidate the generator has cleared" in
    new TestKit(ActorSystem()) {
    val candidate = realCandidate("queued")
    val (thread, generator) = startThread(candidate, new FixedHitsPowScheme(Seq(5L, 900L)))

    nonceOf(generator.expectMsgType[InputSolutionFound](10.seconds)) shouldBe 5L
    // both are in the mailbox before the thread reads either
    thread.tell(StatusReply.Success(shifted(candidate, 1)), generator.ref)
    thread.tell(StatusReply.Error(new Exception("Invalid input block! PoW valid: false")), generator.ref)

    answerAsTipGenerator(generator, candidate, firstShift = 2) shouldBe
      AfterRejection(polls = 1, newCandidates = 1, rejected = 0, refused = 0, accepted = true)
    system.terminate()
  }

  it should "against the real generator, poll once and get one new candidate per rejected solution" in
    new TestKit(ActorSystem()) {
    val pow = new CandidateBoundPowScheme(Seq(5L, 900L))
    val chainSettings: ErgoSettings = settings.copy(
      directory = s"${settings.directory}-real-generator-${System.nanoTime()}",
      chainSettings = settings.chainSettings.copy(powScheme = pow))
    val viewHolderRef: ActorRef = ErgoNodeViewRef(chainSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)
    val realGenerator: ActorRef =
      CandidateGenerator(defaultMinerSecret.publicImage, readersHolderRef, viewHolderRef, chainSettings)
    val events = TestProbe()
    val relay = system.actorOf(Props(new Relay(realGenerator, events.ref)))
    val thread = ErgoMiningThread(chainSettings, relay, defaultMinerSecret.w)

    // a new candidate is one whose PoW message was not seen before
    var seen = Set.empty[Seq[Byte]]
    // up to the rejection of the thread's first solution (the first poll waits for the genesis state)
    var solutionsBefore = 0
    events.fishForMessage(60.seconds, hint = "the rejection of the first solution") {
      case ToGenerator(_: SolutionFound) => solutionsBefore += 1; false
      case ToThread(StatusReply.Success(c: Candidate)) => seen += c.externalVersion.msg.toSeq; false
      case ToThread(StatusReply.Error(_)) => true
      case _ => false
    }
    solutionsBefore shouldBe 1

    // from there to the first accepted solution
    var polls, newCandidates, rejected, solutions, messages = 0
    var accepted = false
    while (!accepted && messages < 40) {
      messages += 1
      events.receiveOne(10.seconds) match {
        case null => messages = 40
        case ToGenerator(_: GenerateCandidate) => polls += 1
        case ToGenerator(_: SolutionFound) => solutions += 1
        case ToThread(StatusReply.Success(c: Candidate)) =>
          if (!seen.contains(c.externalVersion.msg.toSeq)) {
            seen += c.externalVersion.msg.toSeq
            newCandidates += 1
          }
        case ToThread(StatusReply.Error(_)) => rejected += 1
        case ToThread(StatusReply.Success(())) => accepted = true
        case _ =>
      }
    }
    info(s"after one rejection: $polls polls, $newCandidates new candidates, $solutions solutions, " +
      s"$rejected more errors, accepted: $accepted")
    (polls, newCandidates, solutions, rejected, accepted) shouldBe ((1, 1, 1, 0, true))
    system.stop(thread)
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
    // the generator kept the candidate
    generator.expectMsgType[GenerateCandidate](5.seconds)
    generator.reply(StatusReply.Success(candidate))

    // the next batch starts right after the rejected nonce, as a Long
    calls.expectMsg(10.seconds, (Int.MaxValue.toLong + 1, Int.MaxValue.toLong + 1001))
    system.terminate()
  }
}
