package org.ergoplatform

import akka.Done
import akka.actor.{ActorSystem, CoordinatedShutdown, PoisonPill}
import akka.testkit.{TestKit, TestProbe}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import scala.concurrent.{Await, Future, Promise}

class CriticalActorsWatcherSpec
  extends TestKit(ActorSystem("CriticalActorsWatcherSpec"))
  with AnyFlatSpecLike
  with Matchers {

  /** Register a CoordinatedShutdown task that completes the returned promise when shutdown starts.
    * Test config disables `terminate-actor-system`, so the system stays alive across runs. */
  private def shutdownProbe(sys: ActorSystem, taskName: String): Promise[Done] = {
    val p = Promise[Done]()
    CoordinatedShutdown(sys).addTask(CoordinatedShutdown.PhaseBeforeServiceUnbind, taskName) { () =>
      p.trySuccess(Done)
      Future.successful(Done)
    }
    p
  }

  "CriticalActorsWatcher" should "trigger coordinated shutdown when a watched actor terminates" in {
    val sys = ActorSystem("watcher-trigger-on-death")
    try {
      val probeA = TestProbe()(sys)
      val probeB = TestProbe()(sys)
      val probeC = TestProbe()(sys)

      val shutdownStarted = shutdownProbe(sys, "test-shutdown-probe")

      sys.actorOf(
        CriticalActorsWatcher.props(Seq(probeA.ref, probeB.ref, probeC.ref)),
        "watcher"
      )

      probeB.ref ! PoisonPill

      Await.result(shutdownStarted.future, 5.seconds) shouldBe Done
    } finally {
      Await.ready(sys.terminate(), 5.seconds)
    }
  }

  it should "trigger shutdown for an actor added via the Watch message" in {
    val sys = ActorSystem("watcher-trigger-on-dynamic-watch")
    try {
      val initial = TestProbe()(sys)
      val added   = TestProbe()(sys)

      val shutdownStarted = shutdownProbe(sys, "test-shutdown-probe-dynamic")

      val watcher = sys.actorOf(
        CriticalActorsWatcher.props(Seq(initial.ref)),
        "watcher"
      )

      watcher ! CriticalActorsWatcher.Watch(added.ref)
      // Give the watcher a moment to process the Watch before we kill `added`.
      added.ref ! PoisonPill

      Await.result(shutdownStarted.future, 5.seconds) shouldBe Done
    } finally {
      Await.ready(sys.terminate(), 5.seconds)
    }
  }

  it should "not trigger shutdown while all watched actors are alive" in {
    val sys = ActorSystem("watcher-no-shutdown-when-alive")
    try {
      val probeA = TestProbe()(sys)
      val probeB = TestProbe()(sys)

      val shutdownStarted = shutdownProbe(sys, "test-shutdown-probe-quiet")

      sys.actorOf(
        CriticalActorsWatcher.props(Seq(probeA.ref, probeB.ref)),
        "watcher"
      )

      // Wait briefly and ensure the shutdown promise has not been completed.
      intercept[java.util.concurrent.TimeoutException] {
        Await.result(shutdownStarted.future, 1.second)
      }
    } finally {
      Await.ready(sys.terminate(), 5.seconds)
    }
  }
}
