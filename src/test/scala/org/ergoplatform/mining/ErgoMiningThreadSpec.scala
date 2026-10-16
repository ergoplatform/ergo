package org.ergoplatform.mining

import java.util.concurrent.{CountDownLatch, TimeUnit}
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

  // the periodic poll fires 1 s after start and then once an hour, so in a test every later candidate is sent by hand
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
    * solution's nonce, so the miner's nonce search is observable, and the candidate's timestamp as the solution's
    * `d`, so a solution names the candidate it was found on. Every call's bounds go to `calls`, if given; the first
    * call then waits for `holdFirstCall`, if given, so messages sent to the thread meanwhile are all queued before it
    * reads any. */
  private class FixedHitsPowScheme(hits: Seq[Long],
                                   calls: Option[ActorRef] = None,
                                   holdFirstCall: Option[CountDownLatch] = None) extends DefaultFakePowScheme(32, 26) {
    private val firstCall = new AtomicBoolean(true)

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
      if (firstCall.getAndSet(false)) {
        holdFirstCall.foreach(latch => assert(latch.await(10L, TimeUnit.SECONDS), "the held prove call was not released"))
      }
      hits.find(h => h >= minNonce && h < maxNonce) match {
        case None => NothingFound
        case Some(hit) =>
          super.prove(parentOpt, version, nBits, stateRoot, adProofsRoot, transactionsRoot, timestamp, extensionHash,
            votes, sk, minNonce, maxNonce, parameters) match {
            case OrderingBlockHeaderFound(h) =>
              val s = h.powSolution
              InputBlockHeaderFound(h.copy(powSolution =
                new AutolykosSolution(s.pk, s.w, Longs.toByteArray(hit), dFor(timestamp))))
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

  /** `c` regenerated at the same timestamp (as with a parent dated ahead of the local clock): new PoW work. */
  private def sameTimestamp(c: Candidate): Candidate =
    c.copy(externalVersion = c.externalVersion.copy(msg = Blake2b256(c.externalVersion.msg)))

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

  /** Returns once the thread has handled every message sent to it before this call and every step those queued: a
    * step queued while the first barrier waits in the mailbox runs before the second barrier. */
  private def settle(thread: ActorRef)(implicit system: ActorSystem): Unit = {
    val barrier = TestProbe()
    (1 to 2).foreach { _ =>
      barrier.send(thread, ErgoMiningThread.GetSolvedBlocksCount)
      barrier.expectMsgType[ErgoMiningThread.SolvedBlocksCount](10.seconds)
    }
  }

  // The generator answers "Invalid input block! PoW valid: false" when its candidate is not the one the hit was found
  // on (it clears it), so a hit found after the rejection on the same candidate cannot be accepted either.
  it should "after a rejected solution, hash and send nothing until new work, then mine it from nonce 0" in
    new TestKit(ActorSystem()) {
    try {
      val candidate = realCandidate("drop")
      val calls = TestProbe()
      val (thread, generator) = startThread(candidate, new FixedHitsPowScheme(Seq(5L, 900L), Some(calls.ref)))

      calls.expectMsg(10.seconds, (0L, 1000L))
      nonceOf(generator.expectMsgType[InputSolutionFound](10.seconds)) shouldBe 5L
      thread.tell(StatusReply.Error(new Exception("Invalid input block! PoW valid: false")), generator.ref)
      settle(thread)
      calls.expectNoMessage(Duration.Zero)
      generator.expectNoMessage(Duration.Zero)

      // new work, as the periodic poll returns it once the generator has a new candidate
      val next = shifted(candidate, 1)
      thread.tell(StatusReply.Success(next), generator.ref)
      calls.expectMsg(10.seconds, (0L, 1000L))
      generator.expectMsgType[InputSolutionFound](10.seconds).as.d shouldBe BigInt(next.candidateBlock.timestamp)
    } finally {
      system.terminate()
    }
  }

  // With two or more miners on one node, the generator keeps the candidate one of them solved and answers every other
  // solution with "Block already solved" until the next block; a poll meanwhile returns that same candidate.
  it should "after a rejected solution, mine nothing on a candidate the generator returns unchanged" in
    new TestKit(ActorSystem()) {
    try {
      val candidate = realCandidate("unchanged")
      val calls = TestProbe()
      val (thread, generator) = startThread(candidate, new FixedHitsPowScheme(Seq(5L, 900L), Some(calls.ref)))

      calls.expectMsg(10.seconds, (0L, 1000L))
      nonceOf(generator.expectMsgType[InputSolutionFound](10.seconds)) shouldBe 5L
      thread.tell(StatusReply.Error(new Exception("Block already solved : Some(...)")), generator.ref)
      thread.tell(StatusReply.Success(candidate), generator.ref)
      settle(thread)
      calls.expectNoMessage(Duration.Zero)
      generator.expectNoMessage(Duration.Zero)
    } finally {
      system.terminate()
    }
  }

  // A new candidate that arrives while the solution awaits its reply is mined, from nonce 0, whatever the reply.
  it should "mine a candidate that arrived before the rejection from nonce 0" in
    new TestKit(ActorSystem()) {
    val release = new CountDownLatch(1)
    try {
      val candidate = realCandidate("queued")
      val calls = TestProbe()
      val pow = new FixedHitsPowScheme(Seq(5L, 900L), Some(calls.ref), holdFirstCall = Some(release))
      val (thread, generator) = startThread(candidate, pow)

      // the thread is held in the step that finds nonce 5, so both replies are in the mailbox before it sends that
      // solution and reads either
      calls.expectMsg(10.seconds, (0L, 1000L))
      val next = shifted(candidate, 1)
      thread.tell(StatusReply.Success(next), generator.ref)
      thread.tell(StatusReply.Error(new Exception("Invalid input block! PoW valid: false")), generator.ref)
      release.countDown()
      generator.expectMsgType[InputSolutionFound](10.seconds).as.d shouldBe BigInt(candidate.candidateBlock.timestamp)

      calls.expectMsg(10.seconds, (0L, 1000L))
      generator.expectMsgType[InputSolutionFound](10.seconds).as.d shouldBe BigInt(next.candidateBlock.timestamp)
    } finally {
      release.countDown()
      system.terminate()
    }
  }

  it should "against the real generator, mine the next polled candidate after a rejected solution, with no other error" in
    new TestKit(ActorSystem()) {
    try {
      val pow = new CandidateBoundPowScheme(Seq(5L, 900L))
      // the default polling interval: the thread waits for the periodic poll after a rejection
      val chainSettings: ErgoSettings = settings.copy(
        directory = s"${settings.directory}-real-generator-${System.nanoTime()}",
        nodeSettings = settings.nodeSettings.copy(internalMinerPollingInterval = 500.millis),
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

      // from there to the first accepted solution; the number of periodic polls depends on timing
      var polls, newCandidates, errors, solutions, messages = 0
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
          case ToThread(StatusReply.Error(_)) => errors += 1
          case ToThread(StatusReply.Success(())) => accepted = true
          case _ =>
        }
      }
      info(s"after one rejection: $polls polls, $newCandidates new candidates, $solutions solutions, " +
        s"$errors more errors, accepted: $accepted")
      (newCandidates, solutions, errors, accepted) shouldBe ((1, 1, 0, true))
      system.stop(thread)
    } finally {
      system.terminate()
    }
  }

  // As the first case, with the generator's new candidate at the old one's timestamp: different PoW work, so the
  // thread mines it from nonce 0.
  it should "after a rejected solution, mine a new candidate with the old one's timestamp from nonce 0" in
    new TestKit(ActorSystem()) {
    try {
      val candidate = realCandidate("same-timestamp")
      val calls = TestProbe()
      val (thread, generator) = startThread(candidate, new FixedHitsPowScheme(Seq(5L, 900L), Some(calls.ref)))

      calls.expectMsg(10.seconds, (0L, 1000L))
      nonceOf(generator.expectMsgType[InputSolutionFound](10.seconds)) shouldBe 5L
      thread.tell(StatusReply.Error(new Exception("Invalid input block! PoW valid: false")), generator.ref)
      settle(thread)
      calls.expectNoMessage(Duration.Zero)

      thread.tell(StatusReply.Success(sameTimestamp(candidate)), generator.ref)
      calls.expectMsg(10.seconds, (0L, 1000L))
      nonceOf(generator.expectMsgType[InputSolutionFound](10.seconds)) shouldBe 5L
    } finally {
      system.terminate()
    }
  }

  it should "while mining, switch to a new candidate with the current one's timestamp" in
    new TestKit(ActorSystem()) {
    val release = new CountDownLatch(1)
    try {
      val candidate = realCandidate("same-timestamp-mining")
      val calls = TestProbe()
      // no hit: the first step finds nothing, and the next starts at its end unless new work restarts it at nonce 0
      val pow = new FixedHitsPowScheme(Seq.empty, Some(calls.ref), holdFirstCall = Some(release))
      val (thread, generator) = startThread(candidate, pow)

      calls.expectMsg(10.seconds, (0L, 1000L))
      thread.tell(StatusReply.Success(sameTimestamp(candidate)), generator.ref)
      release.countDown()
      calls.expectMsg(10.seconds, (0L, 1000L))
    } finally {
      release.countDown()
      system.terminate()
    }
  }
}
