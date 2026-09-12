package org.ergoplatform.nodeView

import akka.actor.{Actor, ActorSystem, Props, Stash, Timers}
import akka.testkit.TestProbe
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

class UtxoSnapshotFinalizationSupportSpec extends ErgoCorePropertyTest {
  import UtxoSnapshotFinalizationSupportSpec._

  property("snapshot finalization retries only persistence, then installs once and unstashes") {
    implicit val system: ActorSystem = ActorSystem("utxo-snapshot-finalization-retry-spec")
    try {
      val probe = TestProbe()
      val actor = system.actorOf(Props(new Harness(
        probe,
        mutable.Queue(Failure(new IllegalStateException("fail once")), Success(())),
        snapshotFinalizationRetryDelay = 200.millis,
        maxSnapshotFinalizationRetries = 2
      )))

      actor ! Start
      probe.expectMsg(PersistAttempt(1))
      actor ! Ping
      actor ! RetryUtxoSnapshotFinalization(generation = 99L)
      probe.expectNoMessage(50.millis)

      probe.expectMsg(PersistAttempt(2))
      probe.expectMsg(Installed)
      probe.expectMsg(Pong)
      probe.expectNoMessage(100.millis)
    } finally {
      Await.result(system.terminate(), 10.seconds)
    }
  }

  property("snapshot finalization invokes coordinated abort after the bounded retry budget") {
    implicit val system: ActorSystem = ActorSystem("utxo-snapshot-finalization-abort-spec")
    try {
      val probe = TestProbe()
      val actor = system.actorOf(Props(new Harness(
        probe,
        mutable.Queue.fill(3)(Failure(new IllegalStateException("persistent failure"))),
        snapshotFinalizationRetryDelay = 20.millis,
        maxSnapshotFinalizationRetries = 2
      )))

      actor ! Start
      actor ! Ping
      probe.expectMsg(PersistAttempt(1))
      probe.expectMsg(PersistAttempt(2))
      probe.expectMsg(PersistAttempt(3))
      probe.expectMsg(Aborted)
      probe.expectNoMessage(100.millis)
    } finally {
      Await.result(system.terminate(), 10.seconds)
    }
  }
}

private object UtxoSnapshotFinalizationSupportSpec {
  private case object Start
  private case object Ping
  private case object Pong
  private case object Installed
  private case object Aborted
  private final case class PersistAttempt(number: Int)

  private val SnapshotHeight = 100
  private val SnapshotBlockId: ModifierId = bytesToId(Array.fill(32)(1: Byte))

  private final class Harness(probe: TestProbe,
                              outcomes: mutable.Queue[Try[Unit]],
                              override protected val snapshotFinalizationRetryDelay: FiniteDuration,
                              override protected val maxSnapshotFinalizationRetries: Int)
    extends Actor
      with Timers
      with Stash
      with ScorexLogging
      with UtxoSnapshotFinalizationSupport {

    private var attempts = 0

    override protected def persistUtxoSnapshotFinalization(height: Int,
                                                            blockId: ModifierId): Try[Unit] = {
      attempts += 1
      probe.ref ! PersistAttempt(attempts)
      outcomes.dequeue()
    }

    override protected def abortUtxoSnapshotFinalization(cause: Throwable): Unit =
      probe.ref ! Aborted

    override def receive: Receive = {
      case Start =>
        beginUtxoSnapshotFinalization(SnapshotHeight, SnapshotBlockId) {
          probe.ref ! Installed
        }
      case Ping => probe.ref ! Pong
    }
  }
}
