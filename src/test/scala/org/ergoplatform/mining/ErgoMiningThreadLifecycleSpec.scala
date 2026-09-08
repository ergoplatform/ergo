package org.ergoplatform.mining

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ExplicitlyTriggeredScheduler, TestActorRef, TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import org.ergoplatform.mining.CandidateGenerator.GenerateCandidate
import org.ergoplatform.settings.ErgoSettingsReader
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.duration._

class ErgoMiningThreadLifecycleSpec extends AnyFlatSpec {

  private case object Started
  private case object Restart

  private def withWorker(test: (ActorSystem, ExplicitlyTriggeredScheduler, TestProbe, TestProbe, ActorRef) => Unit): Unit = {
    implicit val system: ActorSystem = ActorSystem("miner-polling-lifecycle", ConfigFactory.parseString(
      "akka.scheduler.implementation = akka.testkit.ExplicitlyTriggeredScheduler"
    ))
    try {
      val candidates = TestProbe()
      val lifecycle = TestProbe()
      val settings = ErgoSettingsReader.read()
      val pollingSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(
        internalMinerPollingInterval = 2.seconds
      ))
      val worker = TestActorRef[ErgoMiningThread](Props(new ErgoMiningThread(pollingSettings, candidates.ref, BigInt(1)) {
        override def preStart(): Unit = {
          super.preStart()
          lifecycle.ref ! Started
        }

        override def receive: Receive = {
          case Restart => throw new IllegalStateException("restart polling worker")
          case message => super.receive(message)
        }
      }))
      lifecycle.expectMsg(Started)
      test(system, system.scheduler.asInstanceOf[ExplicitlyTriggeredScheduler], candidates, lifecycle, worker)
    } finally {
      TestKit.shutdownActorSystem(system)
    }
  }

  private def expectPoll(candidates: TestProbe, worker: ActorRef): Unit = {
    candidates.expectMsg(GenerateCandidate(Seq.empty, reply = true, forced = false))
    assert(candidates.lastSender == worker)
    candidates.expectNoMessage(Duration.Zero)
  }

  it should "cancel candidate polling when the worker stops" in withWorker { (system, scheduler, candidates, lifecycle, worker) =>
    scheduler.timePasses(999.millis)
    candidates.expectNoMessage(Duration.Zero)
    scheduler.timePasses(1.millis)
    expectPoll(candidates, worker)
    scheduler.timePasses(2.seconds)
    expectPoll(candidates, worker)

    lifecycle.watch(worker)
    system.stop(worker)
    lifecycle.expectTerminated(worker)
    // Every earlier poll was consumed before stopping; future ticks cannot be queued leftovers.
    scheduler.timePasses(6.seconds)
    candidates.expectNoMessage(Duration.Zero)
  }

  it should "replace candidate polling rather than accumulate tasks after restart" in withWorker { (_, scheduler, candidates, lifecycle, worker) =>
    scheduler.timePasses(1.second)
    expectPoll(candidates, worker)

    worker ! Restart
    lifecycle.expectMsg(Started)
    scheduler.timePasses(1.second)
    expectPoll(candidates, worker)
    // The old incarnation would poll here if its periodic task survived postStop.
    scheduler.timePasses(1.second)
    candidates.expectNoMessage(Duration.Zero)
    scheduler.timePasses(1.second)
    expectPoll(candidates, worker)
    scheduler.timePasses(2.seconds)
    expectPoll(candidates, worker)
  }
}
