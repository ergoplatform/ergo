package org.ergoplatform.nodeView.history.extra

import akka.Done
import akka.actor.{ActorInitializationException, ActorSystem, CoordinatedShutdown}
import akka.event.Logging.Error
import akka.testkit.TestProbe
import com.typesafe.config.ConfigFactory
import org.ergoplatform.ErgoApp.InternalShutdown
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.HistoryStorage
import org.ergoplatform.nodeView.state.{ErgoState, StateType}
import org.ergoplatform.utils.{ErgoCorePropertyTest, NodeViewTestConfig}

import java.nio.file.Files
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration._

class HistoryOpenFailureShutdownSpec extends ErgoCorePropertyTest {
  private val actorConfig = ConfigFactory.parseString("""
    akka.coordinated-shutdown.terminate-actor-system = on
    akka.coordinated-shutdown.exit-jvm = off
    akka.coordinated-shutdown.run-by-actor-system-terminate = off
    akka.coordinated-shutdown.run-by-jvm-shutdown-hook = off
  """).withFallback(ConfigFactory.load())

  private def withSystems(test: (ActorSystem, ActorSystem) => Unit): Unit = {
    val target = ActorSystem("history-open-target", actorConfig)
    val observer = ActorSystem("history-open-observer", actorConfig)
    try test(target, observer)
    finally {
      Await.result(target.terminate(), 10.seconds)
      Await.result(observer.terminate(), 10.seconds)
    }
  }

  for (stateType <- Seq(StateType.Utxo, StateType.Digest)) {
    property(s"history open failure shuts down the $stateType ActorSystem and retains a state-directory marker") {
      val directory = Files.createTempDirectory("history-open-failure")
      val defaults = NodeViewTestConfig(stateType, verifyTransactions = true, popowBootstrap = false).toSettings
      val settings = defaults.copy(directory = directory.toString,
        nodeSettings = defaults.nodeSettings.copy(extraIndex = true))
      val stateDirectory = Files.createDirectories(ErgoState.stateDir(settings).toPath)
      val marker = Files.write(stateDirectory.resolve("preserved-state"), Array[Byte](42))
      val storage = HistoryStorage(settings)
      try {
        // A malformed checkpoint object throws while readOrGenerate reads its schema version.
        storage.insert(ExtraIndexer.SchemaVersionKey, Array.emptyByteArray).get
      } finally {
        storage.close()
      }
      withSystems { (target, observer) =>
        val shutdown = CoordinatedShutdown(target)
        val releaseShutdown = Promise[Done]()
        // Keep the guardian alive until its constructor-failure event has been observed.
        shutdown.addTask(CoordinatedShutdown.PhaseBeforeServiceUnbind, "observe-holder-failure") {
          () => releaseShutdown.future
        }
        try {
          val failures = TestProbe()(observer)
          target.eventStream.subscribe(failures.ref, classOf[Error])
          val death = TestProbe()(observer)
          val holder = ErgoNodeViewRef(settings)(target)
          death.watch(holder)

          val failure = failures.fishForMessage(10.seconds) {
            case Error(_: ActorInitializationException, _, _, _) => true
            case _ => false
          }.asInstanceOf[Error].cause
          val causes = Iterator.iterate(failure)(_.getCause).takeWhile(_ != null).toVector
          causes.exists(ex => ex.isInstanceOf[UnsupportedOperationException] &&
            ex.getMessage == "empty.tail") shouldBe true
          causes.flatMap(_.getStackTrace).exists(frame =>
            frame.getClassName == "org.ergoplatform.nodeView.history.ErgoHistory$" &&
              frame.getMethodName == "readOrGenerate") shouldBe true
          causes.flatMap(_.getStackTrace).exists(frame =>
            frame.getClassName == "org.ergoplatform.nodeView.ErgoNodeViewHolder" &&
              frame.getMethodName == "restoreState") shouldBe true
          death.expectTerminated(holder, 10.seconds)

          // Constructor failure and death are observed first: this is not an elapsed-silence oracle.
          shutdown.shutdownReason() shouldBe Some(InternalShutdown)
        } finally {
          releaseShutdown.trySuccess(Done)
        }
        Await.result(target.whenTerminated, 10.seconds)
        Files.readAllBytes(marker).toSeq shouldBe Seq[Byte](42)
      }
    }

    property(s"ordinary $stateType history open and restart leave the ActorSystem running") {
      val directory = Files.createTempDirectory("history-open-success")
      val defaults = NodeViewTestConfig(stateType, verifyTransactions = true, popowBootstrap = false).toSettings
      val settings = defaults.copy(directory = directory.toString,
        nodeSettings = defaults.nodeSettings.copy(extraIndex = true))
      withSystems { (target, observer) =>
        val shutdown = CoordinatedShutdown(target)
        val probe = TestProbe()(observer)
        for (_ <- 1 to 2) {
          val holder = ErgoNodeViewRef(settings)(target)
          probe.watch(holder)
          probe.send(holder, GetDataFromCurrentView[ErgoState[_], String](_.state.version.toString))
          probe.expectMsg(10.seconds, ErgoState.genesisStateVersion.toString)
          shutdown.shutdownReason() shouldBe None
          target.stop(holder)
          probe.expectTerminated(holder, 10.seconds)
        }
        shutdown.shutdownReason() shouldBe None
        target.whenTerminated.isCompleted shouldBe false
      }
    }
  }

  property("state open failure shuts down the ActorSystem and retains the blocked state path") {
    val directory = Files.createTempDirectory("state-open-failure")
    val defaults = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    val settings = defaults.copy(directory = directory.toString,
      nodeSettings = defaults.nodeSettings.copy(extraIndex = true))
    val storage = HistoryStorage(settings)
    storage.close()
    ErgoHistory.historyDir(settings).listFiles().nonEmpty shouldBe true
    val blockedState = Files.write(ErgoState.stateDir(settings).toPath, Array[Byte](42))

    withSystems { (target, observer) =>
      val shutdown = CoordinatedShutdown(target)
      val releaseShutdown = Promise[Done]()
      shutdown.addTask(CoordinatedShutdown.PhaseBeforeServiceUnbind, "observe-state-open-failure") {
        () => releaseShutdown.future
      }
      try {
        val failures = TestProbe()(observer)
        target.eventStream.subscribe(failures.ref, classOf[Error])
        val death = TestProbe()(observer)
        val holder = ErgoNodeViewRef(settings)(target)
        death.watch(holder)
        val failure = failures.fishForMessage(10.seconds) {
          case Error(_: ActorInitializationException, _, _, _) => true
          case _ => false
        }.asInstanceOf[Error].cause
        val causes = Iterator.iterate(failure)(_.getCause).takeWhile(_ != null).toVector
        causes.flatMap(_.getStackTrace).exists(frame =>
          frame.getClassName == "org.ergoplatform.nodeView.state.ErgoState$" &&
            frame.getMethodName == "readOrGenerate") shouldBe true
        death.expectTerminated(holder, 10.seconds)
        shutdown.shutdownReason() shouldBe Some(InternalShutdown)
      } finally {
        releaseShutdown.trySuccess(Done)
      }
      Await.result(target.whenTerminated, 10.seconds)
      Files.readAllBytes(blockedState).toSeq shouldBe Seq[Byte](42)
    }
  }

  property("blocked history path shuts down the ActorSystem before the first database read") {
    val directory = Files.createTempDirectory("history-path-failure")
    val defaults = NodeViewTestConfig(StateType.Utxo, verifyTransactions = true, popowBootstrap = false).toSettings
    val settings = defaults.copy(directory = directory.toString)
    val blockedHistory = Files.write(directory.resolve("history"), Array[Byte](42))

    withSystems { (target, observer) =>
      val shutdown = CoordinatedShutdown(target)
      val releaseShutdown = Promise[Done]()
      shutdown.addTask(CoordinatedShutdown.PhaseBeforeServiceUnbind, "observe-history-path-failure") {
        () => releaseShutdown.future
      }
      try {
        val failures = TestProbe()(observer)
        target.eventStream.subscribe(failures.ref, classOf[Error])
        val death = TestProbe()(observer)
        val holder = ErgoNodeViewRef(settings)(target)
        death.watch(holder)
        val failure = failures.fishForMessage(10.seconds) {
          case Error(_: ActorInitializationException, _, _, _) => true
          case _ => false
        }.asInstanceOf[Error].cause
        val causes = Iterator.iterate(failure)(_.getCause).takeWhile(_ != null).toVector
        causes.exists(_.isInstanceOf[NullPointerException]) shouldBe true
        causes.flatMap(_.getStackTrace).exists(frame =>
          frame.getClassName == "org.ergoplatform.nodeView.ErgoNodeViewHolder" &&
            frame.getMethodName == "restoreState") shouldBe true
        death.expectTerminated(holder, 10.seconds)
        shutdown.shutdownReason() shouldBe Some(InternalShutdown)
      } finally {
        releaseShutdown.trySuccess(Done)
      }
      Await.result(target.whenTerminated, 10.seconds)
      Files.readAllBytes(blockedHistory).toSeq shouldBe Seq[Byte](42)
    }
  }
}
