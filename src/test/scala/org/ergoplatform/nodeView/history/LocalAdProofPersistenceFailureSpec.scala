package org.ergoplatform.nodeView.history

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import java.io.IOException
import java.util.concurrent.atomic.AtomicBoolean
import org.ergoplatform.modifiers.history.ADProofs
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{
  LocalBlockApplied,
  RecoverableFailedModification,
  SemanticallyFailedModification,
  SyntacticallyFailedModification,
  SyntacticallySuccessfulModifier
}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.{LocallyGeneratedModifier, UtxoNodeViewHolder}
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.history.storage.modifierprocessors.FullBlockSectionProcessor
import org.ergoplatform.nodeView.state.{StateType, UtxoState}
import org.ergoplatform.settings.{ErgoSettings, NetworkType}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig}
import org.ergoplatform.utils.generators.ValidBlocksGenerators.{createUtxoState, validFullBlock}
import org.ergoplatform.wallet.utils.TestFileUtils

import scala.concurrent.duration._
import scala.util.{Failure, Try}
import scala.util.control.NonFatal

/** A failed local ADProof write must not be reported as a stored, servable modifier. */
class LocalAdProofPersistenceFailureSpec extends ErgoCorePropertyTest with TestFileUtils {

  private class FailOneProofWrite(holderSettings: ErgoSettings,
                                  failNext: AtomicBoolean,
                                  failure: Throwable =
                                    new IOException("injected ADProof write failure"))
    extends UtxoNodeViewHolder(holderSettings) {

    private lazy val failingHistory: ErgoHistory = {
      val original = super.history()
      new ErgoHistory with FullBlockSectionProcessor {
        override protected val settings: ErgoSettings = holderSettings
        override protected[history] val historyStorage: HistoryStorage = original.historyStorage
        override val powScheme = settings.chainSettings.powScheme

        override def dumpToDb(id: Array[Byte], bytes: Array[Byte]): Try[Unit] =
          if (failNext.compareAndSet(true, false)) {
            Failure(failure)
          } else {
            super.dumpToDb(id, bytes)
          }
      }
    }

    override protected def history(): ErgoHistory = failingHistory
  }

  property("a failed local ADProof write cannot advertise a proof absent from serving storage") {
    new TestKit(ActorSystem()) {
      val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
        popowBootstrap = false).toSettings.copy(
          directory = createTempDir.getAbsolutePath,
          networkType = NetworkType.MainNet)
      val (sourceState, boxes) = createUtxoState(settings)
      val block = validFullBlock(None, sourceState, boxes)
      val proof = block.adProofs.get
      val failNext = new AtomicBoolean(true)
      val holder = system.actorOf(Props(new FailOneProofWrite(settings, failNext)))
      val events = TestProbe()
      val queries = TestProbe()
      val lifecycle = TestProbe()
      system.eventStream.subscribe(events.ref, classOf[RecoverableFailedModification])
      system.eventStream.subscribe(events.ref, classOf[SyntacticallySuccessfulModifier])
      system.eventStream.subscribe(events.ref, classOf[SyntacticallyFailedModification])
      system.eventStream.subscribe(events.ref, classOf[SemanticallyFailedModification])
      system.eventStream.subscribe(events.ref, classOf[LocalBlockApplied])
      lifecycle.watch(holder)

      try {
        // Omitting ADProofs makes UtxoState generate them and invoke the holder callback.
        queries.send(holder, LocallyGeneratedModifier(block.header))
        queries.send(holder, LocallyGeneratedModifier(block.blockTransactions))
        queries.send(holder, LocallyGeneratedModifier(block.extension))
        queries.send(holder, GetDataFromCurrentView[UtxoState, Boolean] { view =>
          view.history.modifierTypeAndBytesById(proof.id)
            .exists(_._1 == ADProofs.modifierTypeId)
        })

        val servable = queries.expectMsgPF(5.seconds) {
          case present: java.lang.Boolean => present.booleanValue()
        }
        val observed = events.receiveN(5, 5.seconds)
        val recoverable = observed.collect {
          case event: RecoverableFailedModification
            if event.typeId == ADProofs.modifierTypeId && event.modifierId == proof.id => event
        }
        val successes = observed.collect {
          case event: SyntacticallySuccessfulModifier
            if event.typeId == ADProofs.modifierTypeId && event.modifierId == proof.id => event
        }
        val syntacticFailures = observed.collect {
          case event: SyntacticallyFailedModification
            if event.typeId == ADProofs.modifierTypeId && event.modifierId == proof.id => event
        }
        val semanticFailures = observed.collect {
          case event: SemanticallyFailedModification
            if event.typeId == ADProofs.modifierTypeId && event.modifierId == proof.id => event
        }
        val applied = observed.collect {
          case event: LocalBlockApplied if event.header.id == block.header.id => event
        }

        failNext.get() shouldBe false
        withClue(s"ADProof ${proof.encodedId}: events=$observed, servable=$servable. ") {
          servable shouldBe false
          recoverable.size shouldBe 1
          recoverable.head.error.getClass shouldBe classOf[IOException]
          recoverable.head.error.getMessage shouldBe "injected ADProof write failure"
          successes.size shouldBe 0
          syntacticFailures.size shouldBe 0
          semanticFailures.size shouldBe 0
          applied.size shouldBe 1
        }
        events.expectNoMessage(1.second)
        lifecycle.expectNoMessage(250.millis)
      } finally {
        TestKit.shutdownActorSystem(system)
        sourceState.closeStorage()
      }
    }
  }

  property("a successful local ADProof write is servable before success is reported") {
    new TestKit(ActorSystem()) {
      val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
        popowBootstrap = false).toSettings.copy(
          directory = createTempDir.getAbsolutePath,
          networkType = NetworkType.MainNet)
      val (sourceState, boxes) = createUtxoState(settings)
      val block = validFullBlock(None, sourceState, boxes)
      val proof = block.adProofs.get
      val holder = system.actorOf(Props(new UtxoNodeViewHolder(settings)))
      val events = TestProbe()
      val queries = TestProbe()
      val lifecycle = TestProbe()
      system.eventStream.subscribe(events.ref, classOf[RecoverableFailedModification])
      system.eventStream.subscribe(events.ref, classOf[SyntacticallySuccessfulModifier])
      system.eventStream.subscribe(events.ref, classOf[SyntacticallyFailedModification])
      system.eventStream.subscribe(events.ref, classOf[SemanticallyFailedModification])
      system.eventStream.subscribe(events.ref, classOf[LocalBlockApplied])
      lifecycle.watch(holder)

      try {
        queries.send(holder, LocallyGeneratedModifier(block.header))
        queries.send(holder, LocallyGeneratedModifier(block.blockTransactions))
        queries.send(holder, LocallyGeneratedModifier(block.extension))
        queries.send(holder, GetDataFromCurrentView[UtxoState, Boolean] { view =>
          view.history.modifierTypeAndBytesById(proof.id)
            .exists(_._1 == ADProofs.modifierTypeId)
        })

        val servable = queries.expectMsgPF(5.seconds) {
          case present: java.lang.Boolean => present.booleanValue()
        }
        val observed = events.receiveN(5, 5.seconds)
        val successes = observed.collect {
          case event: SyntacticallySuccessfulModifier
            if event.typeId == ADProofs.modifierTypeId && event.modifierId == proof.id => event
        }
        val recoverable = observed.collect {
          case event: RecoverableFailedModification
            if event.typeId == ADProofs.modifierTypeId && event.modifierId == proof.id => event
        }
        val syntacticFailures = observed.collect {
          case event: SyntacticallyFailedModification
            if event.typeId == ADProofs.modifierTypeId && event.modifierId == proof.id => event
        }
        val semanticFailures = observed.collect {
          case event: SemanticallyFailedModification
            if event.typeId == ADProofs.modifierTypeId && event.modifierId == proof.id => event
        }
        val applied = observed.collect {
          case event: LocalBlockApplied if event.header.id == block.header.id => event
        }

        withClue(s"ADProof ${proof.encodedId}: events=$observed, servable=$servable. ") {
          servable shouldBe true
          successes.size shouldBe 1
          recoverable.size shouldBe 0
          syntacticFailures.size shouldBe 0
          semanticFailures.size shouldBe 0
          applied.size shouldBe 1
        }
        events.expectNoMessage(1.second)
        lifecycle.expectNoMessage(250.millis)
      } finally {
        TestKit.shutdownActorSystem(system)
        sourceState.closeStorage()
      }
    }
  }

  property("a fatal generated ADProof write aborts the full-block continuation") {
    val actorSystem = ActorSystem(
      "fatal-adproof-write-test",
      ConfigFactory
        .parseString("akka.jvm-exit-on-fatal-error = false")
        .withFallback(ConfigFactory.load())
    )
    new TestKit(actorSystem) {
      val settings = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true,
        popowBootstrap = false).toSettings.copy(
          directory = createTempDir.getAbsolutePath,
          networkType = NetworkType.MainNet)
      val (sourceState, boxes) = createUtxoState(settings)
      val block = validFullBlock(None, sourceState, boxes)
      val proof = block.adProofs.get
      val failNext = new AtomicBoolean(true)
      val fatal = new LinkageError("injected fatal ADProof write failure")
      val holder = system.actorOf(Props(new FailOneProofWrite(settings, failNext, fatal)))
      val events = TestProbe()
      val requests = TestProbe()
      system.eventStream.subscribe(events.ref, classOf[SyntacticallySuccessfulModifier])
      system.eventStream.subscribe(events.ref, classOf[LocalBlockApplied])

      try {
        NonFatal(fatal) shouldBe false

        requests.send(holder, LocallyGeneratedModifier(block.header))
        events.expectMsgPF(5.seconds) {
          case event: SyntacticallySuccessfulModifier
            if event.modifierId == block.header.id => event
        }
        requests.send(holder, LocallyGeneratedModifier(block.blockTransactions))
        events.expectMsgPF(5.seconds) {
          case event: SyntacticallySuccessfulModifier
            if event.modifierId == block.blockTransactions.id => event
        }
        requests.send(holder, LocallyGeneratedModifier(block.extension))
        events.expectMsgPF(5.seconds) {
          case event: SyntacticallySuccessfulModifier
            if event.modifierId == block.extension.id => event
        }

        awaitCond(!failNext.get(), max = 5.seconds, interval = 25.millis)
        events.expectNoMessage(1.second)

        withClue(s"ADProof ${proof.encodedId}: fatal write reached. ") {
          failNext.get() shouldBe false
        }
      } finally {
        TestKit.shutdownActorSystem(system)
        sourceState.closeStorage()
      }
    }
  }
}
