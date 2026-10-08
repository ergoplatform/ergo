package org.ergoplatform.mining

import java.util.concurrent.{CountDownLatch, TimeUnit}

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.StatusReply
import akka.testkit.{TestKit, TestProbe}
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader, Parameters}
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.{NothingFound, ProveBlockResult}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32

import scala.concurrent.duration._

class ErgoMiningThreadChainSpec extends AnyFlatSpec with Matchers with ErgoTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

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

  private case class ProveCall(index: Int)

  /** Fake PoW that reports each call to `observer`. The first call waits for `release` and finds nothing; every
    * later call finds a solution, which ends the nonce-search chain that made it. */
  private class CountingPowScheme(observer: ActorRef, release: CountDownLatch) extends DefaultFakePowScheme(32, 26) {
    private var calls = 0 // only the thread's actor calls prove

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
      calls += 1
      observer ! ProveCall(calls)
      if (calls == 1) {
        assert(release.await(10L, TimeUnit.SECONDS), "first prove call was not released")
        NothingFound
      } else {
        super.prove(parentOpt, version, nBits, stateRoot, adProofsRoot, transactionsRoot, timestamp, extensionHash,
          votes, sk, minNonce, maxNonce, parameters)
      }
    }
  }

  it should "run one nonce-search chain however many candidates and error replies arrive while mining" in
    new TestKit(ActorSystem()) {
    // a real candidate, built once by a real generator on a chain of its own
    val chainSettings: ErgoSettings =
      settings.copy(directory = s"${settings.directory}-thread-chain-${System.nanoTime()}")
    val viewHolderRef: ActorRef = ErgoNodeViewRef(chainSettings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)
    val realGenerator: ActorRef =
      CandidateGenerator(defaultMinerSecret.publicImage, readersHolderRef, viewHolderRef, chainSettings)
    val candidateProbe = TestProbe()
    realGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), candidateProbe.ref)
    // the first request waits for the genesis state, which can take a while on a loaded machine
    val candidate = candidateProbe.expectMsgPF(60.seconds) { case StatusReply.Success(c: Candidate) => c }
    def withTimestampOffset(i: Int): Candidate =
      candidate.copy(candidateBlock = candidate.candidateBlock.copy(timestamp = candidate.candidateBlock.timestamp + i))

    // prove calls and barrier replies go to one probe, so their order there is the order the thread made them in
    val events = TestProbe()
    val release = new CountDownLatch(1)
    val generator = TestProbe()
    val minerSettings = settings.copy(chainSettings =
      settings.chainSettings.copy(powScheme = new CountingPowScheme(events.ref, release)))
    try {
      val thread = ErgoMiningThread(minerSettings, generator.ref, defaultMinerSecret.w)
      generator.fishForMessage(5.seconds) { case _: GenerateCandidate => true; case _ => false }
      generator.reply(StatusReply.Success(candidate))
      events.expectMsg(5.seconds, ProveCall(1))

      // while the first step runs: n new candidates (distinct timestamps) and n error replies to polls
      val n = 10
      (1 to n).foreach { i =>
        thread.tell(StatusReply.Success(withTimestampOffset(i)), generator.ref)
        thread.tell(StatusReply.Error(new Exception("Candidate generation failed")), generator.ref)
      }
      release.countDown()

      // the step after the first finds a solution, which ends its chain; with one chain that is the last step, with
      // a chain per message every other chain still has a step queued ahead of the barrier
      events.expectMsg(5.seconds, ProveCall(2))
      thread.tell(ErgoMiningThread.GetSolvedBlocksCount, events.ref)
      var stepsBeforeBarrier = 0
      events.fishForMessage(10.seconds) {
        case ProveCall(_) => stepsBeforeBarrier += 1; false
        case _: ErgoMiningThread.SolvedBlocksCount => true
      }
      info(s"steps queued after $n candidates and $n error replies: $stepsBeforeBarrier")
      stepsBeforeBarrier shouldBe 0

      // once the chain has ended, a new candidate starts a new one
      thread.tell(StatusReply.Success(withTimestampOffset(n + 1)), generator.ref)
      events.expectMsg(5.seconds, ProveCall(3))
    } finally {
      release.countDown()
      system.terminate()
    }
  }
}
