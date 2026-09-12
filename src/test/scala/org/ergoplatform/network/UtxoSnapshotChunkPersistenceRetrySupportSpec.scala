package org.ergoplatform.network

import akka.actor.{Actor, ActorSystem, Props, Timers}
import akka.testkit.TestProbe
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.util.{ModifierId, ScorexLogging}

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import java.util.concurrent.atomic.AtomicBoolean

class UtxoSnapshotChunkPersistenceRetrySupportSpec extends ErgoCorePropertyTest {
  import UtxoSnapshotChunkPersistenceRetrySupportSpec._

  property("local chunk persistence retry makes no immediate network request and continues once on success") {
    implicit val system: ActorSystem = ActorSystem("utxo-snapshot-chunk-local-retry-spec")
    try {
      val probe = TestProbe()
      val actor = system.actorOf(Props(new Harness(
        probe,
        mutable.Queue(Failure(new IllegalStateException("fail once")), Success(())),
        () => true,
        localChunkPersistenceRetryDelay = 200.millis,
        maxLocalChunkPersistenceRetryAttempts = 3
      )))

      actor ! Start
      probe.expectMsg(RequestedCleared)
      probe.expectNoMessage(100.millis)
      probe.expectMsg(PersistAttempt(1))
      probe.expectMsg(PersistAttempt(2))
      probe.expectMsg(Continued)
      probe.expectNoMessage(100.millis)
    } finally {
      Await.result(system.terminate(), 10.seconds)
    }
  }

  property("local chunk persistence retry aborts after its bounded attempt budget") {
    implicit val system: ActorSystem = ActorSystem("utxo-snapshot-chunk-local-abort-spec")
    try {
      val probe = TestProbe()
      val actor = system.actorOf(Props(new Harness(
        probe,
        mutable.Queue.fill(3)(Failure(new IllegalStateException("persistent failure"))),
        () => true,
        localChunkPersistenceRetryDelay = 20.millis,
        maxLocalChunkPersistenceRetryAttempts = 3
      )))

      actor ! Start
      probe.expectMsg(RequestedCleared)
      probe.expectMsg(PersistAttempt(1))
      probe.expectMsg(PersistAttempt(2))
      probe.expectMsg(PersistAttempt(3))
      probe.expectMsg(Aborted)
      probe.expectNoMessage(100.millis)
    } finally {
      Await.result(system.terminate(), 10.seconds)
    }
  }

  property("obsolete local chunk persistence retry does nothing") {
    implicit val system: ActorSystem = ActorSystem("utxo-snapshot-chunk-local-obsolete-spec")
    try {
      val probe = TestProbe()
      val pending = new AtomicBoolean(true)
      val actor = system.actorOf(Props(new Harness(
        probe,
        mutable.Queue.empty,
        () => pending.get(),
        localChunkPersistenceRetryDelay = 100.millis,
        maxLocalChunkPersistenceRetryAttempts = 3
      )))

      actor ! Start
      probe.expectMsg(RequestedCleared)
      pending.set(false)
      probe.expectNoMessage(200.millis)
    } finally {
      Await.result(system.terminate(), 10.seconds)
    }
  }
}

private object UtxoSnapshotChunkPersistenceRetrySupportSpec {
  private case object Start
  private case object RequestedCleared
  private final case class PersistAttempt(number: Int)
  private case object Continued
  private case object Aborted
  private val ChunkId: ModifierId = ModifierId @@ Algos.encode(Array.fill(32)(3: Byte))

  private final class Harness(probe: TestProbe,
                              outcomes: mutable.Queue[Try[Unit]],
                              isStillPending: () => Boolean,
                              override protected val localChunkPersistenceRetryDelay: FiniteDuration,
                              override protected val maxLocalChunkPersistenceRetryAttempts: Int)
    extends Actor
      with Timers
      with ScorexLogging
      with UtxoSnapshotChunkPersistenceRetrySupport {

    private var attempts = 0

    override protected def abortLocalChunkPersistence(cause: Throwable): Unit =
      probe.ref ! Aborted

    override def receive: Receive = localChunkPersistenceRetryReceive orElse {
      case Start =>
        beginLocalChunkPersistenceRetry(
          ChunkId,
          new IllegalStateException("initial persistence failure"),
          clearRequested = () => probe.ref ! RequestedCleared,
          persist = () => {
            attempts += 1
            probe.ref ! PersistAttempt(attempts)
            outcomes.dequeue()
          },
          isStillPending = isStillPending,
          onSuccess = () => probe.ref ! Continued
        )
    }
  }
}
