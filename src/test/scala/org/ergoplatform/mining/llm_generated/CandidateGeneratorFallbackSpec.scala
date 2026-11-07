package org.ergoplatform.mining

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.StatusReply
import akka.testkit.{TestKit, TestProbe}
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.{ErgoNodeViewRef, ErgoReadersHolderRef, LocallyGeneratedOrderingBlock}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.ErgoTestHelpers
import org.ergoplatform.{InputBlockFound, OrderingBlockFound, OrderingSolutionFound}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * Positive control for the previous-candidate fallback of an ordering solution: with a PoW that accepts only
  * the previous candidate's header message, a solution for that candidate must still be accepted and forwarded.
  * The default test PoW (DefaultFakePowScheme) accepts every header, so the current candidate always fits and
  * the fallback branch is not reached by the other specs.
  */
class CandidateGeneratorFallbackSpec extends AnyFlatSpec with Matchers with ErgoTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private val candidateGenDelay: FiniteDuration    = 3.seconds
  private val blockValidationDelay: FiniteDuration = 2.seconds

  private val baseSettings: ErgoSettings = {
    val empty = ErgoSettingsReader.read()
    val nodeSettings = empty.nodeSettings.copy(
      mining                       = true,
      stateType                    = StateType.Utxo,
      internalMinerPollingInterval = 1.second,
      offlineGeneration            = true,
      verifyTransactions           = true
    )
    val chainSettings = empty.chainSettings.copy(blockInterval = 1.seconds)
    empty.copy(nodeSettings = nodeSettings, chainSettings = chainSettings)
  }

  it should "accept an ordering solution that fits only the previous candidate" in new TestKit(ActorSystem()) {
    @volatile var acceptedMsg: Option[Seq[Byte]] = None
    val keyedPow = new DefaultFakePowScheme(baseSettings.chainSettings.powScheme.k, baseSettings.chainSettings.powScheme.n) {
      override def validate(header: Header): Try[Unit] =
        if (acceptedMsg.contains(msgByHeader(header).toSeq)) Success(())
        else Failure(new Exception("PoW does not fit this header"))
    }
    val settings = baseSettings.copy(
      directory = s"${baseSettings.directory}-fallback-${System.currentTimeMillis()}",
      chainSettings = baseSettings.chainSettings.copy(powScheme = keyedPow)
    )

    try {
      val replyProbe = new TestProbe(system)
      val viewHolderProbe = new TestProbe(system)

      val realViewHolderRef: ActorRef = ErgoNodeViewRef(settings)
      val readersHolderRef: ActorRef = ErgoReadersHolderRef(realViewHolderRef)
      val candidateGenerator: ActorRef =
        CandidateGenerator(defaultMinerSecret.publicImage, readersHolderRef, viewHolderProbe.ref, settings)

      candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = false), replyProbe.ref)
      val first = replyProbe.expectMsgPF(candidateGenDelay) { case StatusReply.Success(c: Candidate) => c }
      candidateGenerator.tell(GenerateCandidate(Seq.empty, reply = true, forced = true), replyProbe.ref)
      val second = replyProbe.expectMsgPF(candidateGenDelay) { case StatusReply.Success(c: Candidate) => c }

      val solution = keyedPow.proveCandidate(first.candidateBlock, defaultMinerSecret.w, 0, 1000, first.parameters) match {
        case OrderingBlockFound(b) => b.header.powSolution
        case InputBlockFound(b)    => b.header.powSolution
        case other                 => fail(s"unexpected proveCandidate result $other")
      }
      def msgOf(c: Candidate): Seq[Byte] =
        keyedPow.msgByHeader(CandidateGenerator.completeOrderingBlock(c.candidateBlock, solution).header).toSeq
      val firstMsg = msgOf(first)
      withClue("precondition: the forced regeneration must produce a different header message") {
        msgOf(second) should not be firstMsg
      }
      acceptedMsg = Some(firstMsg)

      candidateGenerator.tell(OrderingSolutionFound(solution), replyProbe.ref)
      replyProbe.expectMsgPF(blockValidationDelay) { case r: StatusReply[_] if r.isSuccess => () }
      val forwarded = viewHolderProbe.fishForMessage(blockValidationDelay) {
        case _: LocallyGeneratedOrderingBlock => true
        case _                                => false
      }.asInstanceOf[LocallyGeneratedOrderingBlock]
      keyedPow.msgByHeader(forwarded.efb.header).toSeq shouldBe firstMsg
    } finally {
      TestKit.shutdownActorSystem(system)
    }
  }
}
