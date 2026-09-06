package scorex.core.network

import akka.actor.{ActorRef, ActorSystem, Cancellable}
import akka.event.LoggingAdapter
import akka.io.Tcp
import akka.testkit.{ExplicitlyTriggeredScheduler, TestActorRef, TestProbe}
import com.typesafe.config.{Config, ConfigFactory}
import org.ergoplatform.network.message.MessageConstants.MessageCode
import org.ergoplatform.network.peer.PeerManager.ReceivableMessages.RandomPeerExcluding
import org.ergoplatform.utils.ErgoCorePropertyTest
import scorex.core.app.ScorexContext
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork

import java.util.concurrent.ThreadFactory
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

final class RecordingPeriodicTaskScheduler(config: Config, log: LoggingAdapter, threadFactory: ThreadFactory)
  extends ExplicitlyTriggeredScheduler(config, log, threadFactory) {

  private val recorded = ArrayBuffer.empty[(FiniteDuration, FiniteDuration, Cancellable)]

  def tasks: Vector[(FiniteDuration, FiniteDuration, Cancellable)] = recorded.synchronized(recorded.toVector)

  override def scheduleWithFixedDelay(initialDelay: FiniteDuration, delay: FiniteDuration)(runnable: Runnable)
                                     (implicit executor: ExecutionContext): Cancellable = {
    val task = super.scheduleWithFixedDelay(initialDelay, delay)(runnable)
    recorded.synchronized(recorded += ((initialDelay, delay, task)))
    task
  }
}

class PeriodicTaskLifecycleSpec extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private class Fixture {
    private val config = ConfigFactory.parseString(
      "akka.scheduler.implementation = \"scorex.core.network.RecordingPeriodicTaskScheduler\"")
      .withFallback(ConfigFactory.load())
    implicit val system: ActorSystem = ActorSystem("periodic-task-lifecycle", config)
    implicit val ec: ExecutionContext = system.dispatcher
    val scheduler: RecordingPeriodicTaskScheduler = system.scheduler.asInstanceOf[RecordingPeriodicTaskScheduler]
    val peerManager: TestProbe = TestProbe()
    val tcpManager: TestProbe = TestProbe()
    val controllerProbe: TestProbe = TestProbe()

    def controller(): TestActorRef[NetworkController] = {
      val actor = TestActorRef(new NetworkController(settings, peerManager.ref,
        ScorexContext(Seq.empty, None, None), tcpManager.ref, _ => Map.empty[MessageCode, ActorRef]))
      tcpManager.expectMsgType[Tcp.Bind]
      actor
    }

    def bind(actor: ActorRef): Unit = actor ! Tcp.Bound(settings.scorexSettings.network.bindAddress)

    def synchronizer(): TestActorRef[PeerSynchronizer] =
      TestActorRef(new PeerSynchronizer(controllerProbe.ref, peerManager.ref, settings.scorexSettings.network))

    def stop(actor: ActorRef): Unit = {
      val lifecycle = TestProbe()
      lifecycle.watch(actor)
      system.stop(actor)
      lifecycle.expectTerminated(actor)
      system.whenTerminated.isCompleted shouldBe false
    }
  }

  private def withFixture(test: Fixture => Unit): Unit = {
    val fixture = new Fixture
    try test(fixture)
    finally Await.result(fixture.system.terminate(), 10.seconds)
  }

  property("controller repeats stay active until its individual stop") {
    withFixture { f =>
      val controller = f.controller()
      f.scheduler.tasks shouldBe empty
      f.bind(controller)
      val tasks = f.scheduler.tasks
      tasks.map(t => t._1 -> t._2).toSet shouldBe Set(
        5.seconds -> 5.seconds,
        60.seconds -> 60.seconds,
        settings.scorexSettings.network.peerEvictionInterval -> settings.scorexSettings.network.peerEvictionInterval)
      tasks.size shouldBe 3
      tasks.forall(!_._3.isCancelled) shouldBe true
      f.scheduler.timePasses(5.seconds)
      f.peerManager.expectMsgType[RandomPeerExcluding]
      f.peerManager.reply(None)
      f.stop(controller)
      tasks.forall(_._3.isCancelled) shouldBe true
      f.scheduler.timePasses(65.seconds)
      f.peerManager.expectNoMessage(100.millis)
    }
  }

  property("a repeated Bound does not duplicate controller maintenance") {
    withFixture { f =>
      val controller = f.controller()
      f.bind(controller)
      f.bind(controller)
      f.scheduler.tasks.size shouldBe 3
      f.stop(controller)
      f.scheduler.tasks.forall(_._3.isCancelled) shouldBe true
    }
  }

  property("controller restart cancels the old instance and schedules only after rebinding") {
    withFixture { f =>
      val controller = f.controller()
      f.bind(controller)
      val oldActor = controller.underlyingActor
      val oldTasks = f.scheduler.tasks
      controller.suspend()
      controller.restart(new IllegalStateException("controlled lifecycle restart"))
      f.tcpManager.expectMsgType[Tcp.Bind]
      controller.underlyingActor should not be theSameInstanceAs(oldActor)
      oldTasks.forall(_._3.isCancelled) shouldBe true
      f.scheduler.tasks.size shouldBe 3
      f.bind(controller)
      val replacementTasks = f.scheduler.tasks.drop(3)
      replacementTasks.size shouldBe 3
      replacementTasks.forall(!_._3.isCancelled) shouldBe true
      f.stop(controller)
      replacementTasks.forall(_._3.isCancelled) shouldBe true
    }
  }

  property("peer discovery stops when its owner stops while the controller remains alive") {
    withFixture { f =>
      val synchronizer = f.synchronizer()
      val task = f.scheduler.tasks.head
      f.scheduler.tasks.size shouldBe 1
      task._1 shouldBe 2.seconds
      task._2 shouldBe settings.scorexSettings.network.getPeersInterval
      task._3.isCancelled shouldBe false
      f.scheduler.timePasses(2.seconds)
      f.controllerProbe.expectMsgType[SendToNetwork]
      f.stop(synchronizer)
      task._3.isCancelled shouldBe true
      f.scheduler.timePasses(settings.scorexSettings.network.getPeersInterval)
      f.controllerProbe.expectNoMessage(100.millis)
    }
  }

  property("peer discovery restart cancels the old repeat and starts one replacement") {
    withFixture { f =>
      val synchronizer = f.synchronizer()
      val oldActor = synchronizer.underlyingActor
      val oldTask = f.scheduler.tasks.head._3
      synchronizer.suspend()
      synchronizer.restart(new IllegalStateException("controlled lifecycle restart"))
      synchronizer.underlyingActor should not be theSameInstanceAs(oldActor)
      oldTask.isCancelled shouldBe true
      f.scheduler.tasks.size shouldBe 2
      f.scheduler.tasks.last._3.isCancelled shouldBe false
      f.scheduler.timePasses(2.seconds)
      f.controllerProbe.expectMsgType[SendToNetwork]
      f.controllerProbe.expectNoMessage(100.millis)
      f.stop(synchronizer)
      f.scheduler.tasks.forall(_._3.isCancelled) shouldBe true
    }
  }
}
