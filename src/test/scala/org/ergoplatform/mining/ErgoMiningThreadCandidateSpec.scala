package org.ergoplatform.mining

import java.util.concurrent.{CountDownLatch, TimeUnit}

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.pattern.StatusReply
import akka.testkit.{TestKit, TestProbe}
import org.bouncycastle.util.BigIntegers
import org.ergoplatform.mining.CandidateGenerator.{Candidate, GenerateCandidate}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnsignedErgoTransaction}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.nodeView.wallet.ErgoWalletReader
import org.ergoplatform.settings.ErgoSettingsReader
import org.ergoplatform.utils.{ErgoTestHelpers, HistoryTestHelpers, RandomWrapper}
import org.ergoplatform.utils.generators.ChainGenerator.applyChain
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{
  createUtxoState,
  validFullBlock,
  validTransactionsFromBoxHolder
}
import org.ergoplatform.{ErgoBox, ErgoBoxCandidate, Input}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sigma.ast.ErgoTree
import sigma.data.ProveDlog
import sigmastate.crypto.DLogProtocol.DLogProverInput

import scala.concurrent.duration._

class ErgoMiningThreadCandidateSpec extends AnyFlatSpec with Matchers with ErgoTestHelpers {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private class FixedReadersHolder(readers: Readers) extends Actor {
    override def receive: Receive = {
      case GetReaders => sender() ! readers
    }
  }

  private class CapturingPowScheme(observer: ActorRef) extends DefaultFakePowScheme(32, 26) {
    @volatile private var lastCandidate: CandidateBlock = null

    def lastProvedCandidate: CandidateBlock = lastCandidate

    override def proveCandidate(
      candidateBlock: CandidateBlock,
      sk: PrivateKey,
      minNonce: Long,
      maxNonce: Long
    ): Option[ErgoFullBlock] = {
      if (lastCandidate ne candidateBlock) {
        lastCandidate = candidateBlock
        observer ! candidateBlock
      }
      Thread.sleep(1L)
      None
    }
  }

  private class SequencedPowScheme(
    observer: ActorRef,
    solvedBlock: ErgoFullBlock,
    releaseFirstProof: CountDownLatch
  ) extends DefaultFakePowScheme(32, 26) {
    private var proofCount = 0

    override def proveCandidate(
      candidateBlock: CandidateBlock,
      sk: PrivateKey,
      minNonce: Long,
      maxNonce: Long
    ): Option[ErgoFullBlock] = {
      proofCount += 1
      observer ! ((proofCount, candidateBlock))
      if (proofCount == 1) {
        assert(releaseFirstProof.await(5L, TimeUnit.SECONDS), "first proof was not released")
        None
      } else {
        Some(solvedBlock)
      }
    }
  }

  it should "replace internal miner work when a new candidate has the same timestamp" in new TestKit(
    ActorSystem()
  ) {
    try {
      val minedWork = TestProbe()
      val polling = TestProbe()
      val generatorReplies = TestProbe()
      val viewHolder = TestProbe()
      val base = ErgoSettingsReader.read()
      val powScheme = new CapturingPowScheme(minedWork.ref)
      val settings = base.copy(
        directory = s"${base.directory}-same-timestamp-work-${System.nanoTime()}",
        nodeSettings = base.nodeSettings.copy(
          mining = true,
          stateType = StateType.Utxo,
          offlineGeneration = true,
          verifyTransactions = true,
          internalMinerPollingInterval = 100.millis
        ),
        chainSettings = base.chainSettings.copy(powScheme = powScheme)
      )

      val (initialState, initialBoxes) = createUtxoState(settings)
      val random = new RandomWrapper(Some(3))
      val now = System.currentTimeMillis()
      val (firstTransactions, firstBoxes) = validTransactionsFromBoxHolder(initialBoxes, random)
      val firstBlock = validFullBlock(None, initialState, firstTransactions, Some(now - 60000L))
      val firstState = initialState.applyModifier(firstBlock, None)(_ => ()).get
      val (secondTransactions, _) = validTransactionsFromBoxHolder(firstBoxes, random)
      val futureParent = validFullBlock(
        Some(firstBlock), firstState, secondTransactions, Some(now + 120000L)
      )
      val selectedState = firstState.applyModifier(futureParent, None)(_ => ()).get
      val history = applyChain(
        HistoryTestHelpers.generateHistory(
          verifyTransactions = true,
          stateType = StateType.Utxo,
          PoPoWBootstrap = false,
          blocksToKeep = 100
        ),
        Seq(firstBlock, futureParent)
      )
      history.bestFullBlockOpt.map(_.id) shouldBe Some(futureParent.id)
      val rewardBox: ErgoBox = futureParent.transactions.last.outputs.last
      val priorityProp: ProveDlog =
        DLogProverInput(BigIntegers.fromUnsignedByteArray("q03-priority".getBytes())).publicImage
      val unsignedPriority = new UnsignedErgoTransaction(
        IndexedSeq(Input(rewardBox.id, emptyProverResult)),
        IndexedSeq.empty,
        IndexedSeq(new ErgoBoxCandidate(
          rewardBox.value,
          ErgoTree.fromSigmaBoolean(priorityProp),
          selectedState.stateContext.currentHeight
        ))
      )
      val priorityTx = ErgoTransaction(defaultProver.sign(
        unsignedPriority,
        IndexedSeq(rewardBox),
        IndexedSeq.empty,
        selectedState.stateContext
      ).get)
      val wallet = new ErgoWalletReader {
        val walletActor: ActorRef = system.deadLetters
      }
      val readers = Readers(history, selectedState, ErgoMemPool.empty(settings), wallet)
      val readersHolder = system.actorOf(Props(new FixedReadersHolder(readers)))
      val generator = CandidateGenerator(
        defaultMinerSecret.publicImage, readersHolder, viewHolder.ref, settings
      )

      def generated(command: GenerateCandidate): Candidate = {
        generator.tell(command, generatorReplies.ref)
        generatorReplies.expectMsgPF(5.seconds) {
          case StatusReply.Success(candidate: Candidate) => candidate
        }
      }

      val initial = generated(GenerateCandidate(Seq.empty, reply = true, forced = false))
      initial.candidateBlock.timestamp shouldBe futureParent.header.timestamp + 1L
      initial.candidateBlock.transactions.map(_.id) should not contain priorityTx.id
      val worker = ErgoMiningThread(settings, polling.ref, defaultMinerSecret.w)
      val initialPoll = polling.expectMsgType[GenerateCandidate](5.seconds)
      val polledInitial = generated(initialPoll)
      polledInitial.externalVersion.msg.sameElements(initial.externalVersion.msg) shouldBe true
      worker.tell(StatusReply.Success(polledInitial), polling.ref)
      minedWork.expectMsgType[CandidateBlock](5.seconds) shouldBe polledInitial.candidateBlock

      // A timestamp change is the positive control for the worker's replacement path.
      val laterBlock = polledInitial.candidateBlock.copy(
        timestamp = polledInitial.candidateBlock.timestamp + 1L
      )
      val laterTime = polledInitial.copy(
        candidateBlock = laterBlock,
        externalVersion = powScheme.deriveExternalCandidate(
          laterBlock,
          polledInitial.externalVersion.pk,
          polledInitial.txsToInclude.map(_.id)
        )
      )
      laterTime.externalVersion.msg.sameElements(polledInitial.externalVersion.msg) shouldBe false
      worker.tell(StatusReply.Success(laterTime), polling.ref)
      minedWork.expectMsgType[CandidateBlock](5.seconds) shouldBe laterTime.candidateBlock
      worker.tell(StatusReply.Success(polledInitial), polling.ref)
      minedWork.expectMsgType[CandidateBlock](5.seconds) shouldBe polledInitial.candidateBlock

      val replacement = generated(GenerateCandidate(Seq(priorityTx), reply = true, forced = false))
      replacement.candidateBlock.timestamp shouldBe initial.candidateBlock.timestamp
      replacement.candidateBlock.transactions.map(_.id) should contain(priorityTx.id)
      replacement.candidateBlock.transactions.map(_.id) should not be (initial.candidateBlock.transactions.map(_.id))
      CandidateUtils.deriveUnprovenHeader(replacement.candidateBlock).transactionsRoot
        .sameElements(CandidateUtils.deriveUnprovenHeader(initial.candidateBlock).transactionsRoot) shouldBe false
      replacement.externalVersion.msg.sameElements(initial.externalVersion.msg) shouldBe false

      val nextPoll = polling.fishForMessage(5.seconds) {
        case _: GenerateCandidate => true
        case _ => false
      }.asInstanceOf[GenerateCandidate]
      val polledReplacement = generated(nextPoll)
      polledReplacement.externalVersion.msg.sameElements(replacement.externalVersion.msg) shouldBe true
      worker.tell(StatusReply.Success(polledReplacement), polling.ref)
      minedWork.expectMsgType[CandidateBlock](5.seconds) shouldBe polledReplacement.candidateBlock

      withClue("the worker kept hashing its old candidate after receiving different work: ") {
        CandidateUtils.deriveUnprovenHeader(powScheme.lastProvedCandidate).transactionsRoot
          .sameElements(CandidateUtils.deriveUnprovenHeader(polledReplacement.candidateBlock).transactionsRoot) shouldBe true
      }

      system.stop(worker)
      val proofCalls = TestProbe()
      val controlledPolling = TestProbe()
      val barrier = TestProbe()
      val releaseFirstProof = new CountDownLatch(1)
      try {
        val sequencedPowScheme = new SequencedPowScheme(proofCalls.ref, futureParent, releaseFirstProof)
        val controlledSettings = settings.copy(
          chainSettings = settings.chainSettings.copy(powScheme = sequencedPowScheme)
        )
        val controlledWorker = ErgoMiningThread(
          controlledSettings, controlledPolling.ref, defaultMinerSecret.w
        )
        controlledPolling.expectMsgType[GenerateCandidate](5.seconds)
        controlledWorker.tell(StatusReply.Success(polledInitial), controlledPolling.ref)
        proofCalls.expectMsg((1, polledInitial.candidateBlock))

        // The first proof queues its successor after this work change is already in the mailbox.
        controlledWorker.tell(StatusReply.Success(polledReplacement), controlledPolling.ref)
        releaseFirstProof.countDown()
        proofCalls.expectMsg((2, polledReplacement.candidateBlock))
        barrier.send(controlledWorker, ErgoMiningThread.GetSolvedBlocksCount)
        barrier.expectMsgType[ErgoMiningThread.SolvedBlocksCount](5.seconds)
        proofCalls.expectNoMessage(100.millis)

        // Once the solution consumes the only queued command, different work must restart mining.
        controlledWorker.tell(StatusReply.Success(polledInitial), controlledPolling.ref)
        proofCalls.expectMsg((3, polledInitial.candidateBlock))
      } finally {
        releaseFirstProof.countDown()
      }
    } finally {
      TestKit.shutdownActorSystem(system)
    }
  }
}
