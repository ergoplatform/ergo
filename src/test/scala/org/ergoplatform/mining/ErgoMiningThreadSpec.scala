package org.ergoplatform.mining

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.StatusReply
import akka.testkit.{TestKit, TestProbe}
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef}
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.ErgoTestHelpers
import com.google.common.primitives.Longs
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.Parameters
import org.ergoplatform.{AutolykosSolution, InputBlockHeaderFound, InputSolutionFound, NothingFound,
  OrderingBlockHeaderFound, OrderingSolutionFound, ProveBlockResult, SolutionFound}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class ErgoMiningThreadSpec extends AnyFlatSpec with Matchers with ErgoTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private val settings: ErgoSettings = {
    val empty = ErgoSettingsReader.read()
    empty.copy(
      nodeSettings = empty.nodeSettings.copy(
        mining = true,
        stateType = StateType.Utxo,
        internalMinerPollingInterval = 1.second,
        offlineGeneration = true,
        verifyTransactions = true
      ),
      chainSettings = empty.chainSettings.copy(blockInterval = 1.seconds)
    )
  }

  /** Fake PoW with input-block hits at fixed nonces only, so the miner's nonce search is observable. */
  private class FixedHitsPowScheme(hits: Seq[Long]) extends DefaultFakePowScheme(32, 26) {
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
                       parameters: Parameters): ProveBlockResult =
      hits.find(h => h >= minNonce && h < maxNonce) match {
        case None => NothingFound
        case Some(hit) =>
          super.prove(parentOpt, version, nBits, stateRoot, adProofsRoot, transactionsRoot, timestamp, extensionHash,
            votes, sk, minNonce, maxNonce, parameters) match {
            case OrderingBlockHeaderFound(h) =>
              val s = h.powSolution
              InputBlockHeaderFound(h.copy(powSolution = new AutolykosSolution(s.pk, s.w, Longs.toByteArray(hit), s.d)))
            case other => other
          }
      }
  }

  it should "not resubmit a rejected solution's nonce for the same candidate" in new TestKit(ActorSystem()) {
    // a real candidate, from a real generator
    val viewHolderRef: ActorRef = ErgoNodeViewRef(settings)
    val readersHolderRef: ActorRef = ErgoReadersHolderRef(viewHolderRef)
    val realGenerator: ActorRef =
      CandidateGenerator(defaultMinerSecret.publicImage, readersHolderRef, viewHolderRef, settings)
    val candidateProbe = TestProbe()
    realGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false, optPk = None), candidateProbe.ref)
    val candidate = candidateProbe.expectMsgPF(5.seconds) { case StatusReply.Success(c: Candidate) => c }

    // the miner thread talks to a probe standing in for the generator
    val generator = TestProbe()
    // input-block hits at nonces 5 and 900 of the first 1000-nonce batch
    val minerSettings = settings.copy(chainSettings = settings.chainSettings.copy(powScheme = new FixedHitsPowScheme(Seq(5L, 900L))))
    val thread = ErgoMiningThread(minerSettings, generator.ref, defaultMinerSecret.w)
    generator.expectMsgClass(5.seconds, classOf[GenerateCandidate])
    generator.reply(StatusReply.Success(candidate))

    def nextSolution(): SolutionFound = generator.fishForMessage(10.seconds) {
      case _: InputSolutionFound | _: OrderingSolutionFound => true
      case _ => false // periodic candidate polls
    }.asInstanceOf[SolutionFound]

    val first = nextSolution()
    // the generator rejects it (e.g. the candidate it was mined on is no longer the cached one)
    thread.tell(StatusReply.Error(new Exception("Invalid input block! PoW valid: false")), generator.ref)
    val second = nextSolution()

    val (n1, n2) = (Longs.fromByteArray(first.as.n), Longs.fromByteArray(second.as.n))
    n1 shouldBe 5L
    n2 should not be n1
    system.terminate()
  }
}
