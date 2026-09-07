package org.ergoplatform.network

import akka.actor.{ActorRef, ActorSystem, Cancellable}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.modifiers.NetworkObjectTypeId
import org.ergoplatform.modifiers.history.BlockTransactions
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.CheckDelivery
import org.ergoplatform.nodeView.history.ErgoSyncInfoMessageSpec
import org.ergoplatform.utils.ErgoNodeTestConstants.settings
import org.ergoplatform.utils.generators.ConnectedPeerGenerators.connectedPeerGen
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.network.{ConnectedPeer, DeliveryTracker, ModifiersStatus}
import scorex.util.ModifierId

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

class DeliveryExpirationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {
  private implicit val system: ActorSystem = ActorSystem("delivery-expiration-spec")
  private val peer = connectedPeerGen(ActorRef.noSender).sample.get
  private val id = ModifierId @@ ("01" * 32)

  private class Timer extends Cancellable {
    private var cancelled = false
    override def cancel(): Boolean = {
      val wasActive = !cancelled
      cancelled = true
      wasActive
    }
    override def isCancelled: Boolean = cancelled
  }

  private class Requests {
    val tracker: DeliveryTracker = DeliveryTracker.empty(settings)
    val scheduled: ArrayBuffer[(CheckDelivery, Timer)] = ArrayBuffer.empty

    def request(typeId: NetworkObjectTypeId.Value, checks: Int = 0): (CheckDelivery, Timer) = {
      tracker.setRequested(typeId, id, peer, checks) { message =>
        val timer = new Timer
        scheduled += message -> timer
        timer
      }
      scheduled.last
    }
  }

  private class ExpirationConsumer(controller: ActorRef, requests: Requests)(implicit ec: ExecutionContext)
    extends ErgoNodeViewSynchronizer(controller, ActorRef.noSender, ErgoSyncInfoMessageSpec,
      settings, ErgoSyncTracker(settings.scorexSettings.network), requests.tracker) {
    override def preStart(): Unit = ()
    override def receive: Receive = checkDelivery(null)
    override def requestBlockSection(typeId: NetworkObjectTypeId.Value,
                                     ids: Seq[ModifierId],
                                     supplier: ConnectedPeer,
                                     checksDone: Int): Unit = {
      requests.request(typeId, checksDone)
      ()
    }
  }

  it should "cancel every owned timer on reset" in {
    val requests = new Requests
    val timers = Seq(Header.modifierTypeId, BlockTransactions.modifierTypeId, ErgoTransaction.modifierTypeId)
      .map(requests.request(_)._2)

    requests.tracker.reset()

    timers.foreach(_.isCancelled shouldBe true)
    requests.tracker.fullInfo.requested shouldBe empty
    requests.tracker.headersToDownload shouldBe settings.scorexSettings.network.desiredInvObjects * 8
    requests.tracker.modifiersToDownload shouldBe settings.scorexSettings.network.desiredInvObjects
  }

  it should "cancel the replaced timer while keeping the successor active" in {
    val requests = new Requests
    val first = requests.request(Header.modifierTypeId)
    val second = requests.request(Header.modifierTypeId, checks = 3)

    first._2.isCancelled shouldBe true
    second._2.isCancelled shouldBe false
    requests.tracker.getRequestedInfo(Header.modifierTypeId, id).get.checks shouldBe 3
  }

  it should "cancel a removed request timer" in {
    val requests = new Requests
    val timer = requests.request(Header.modifierTypeId)._2

    requests.tracker.setUnknown(id, Header.modifierTypeId)

    timer.isCancelled shouldBe true
    requests.tracker.getRequestedInfo(Header.modifierTypeId, id) shouldBe None
  }

  it should "keep the previous request active when replacement scheduling fails" in {
    val requests = new Requests
    val previous = requests.request(Header.modifierTypeId, checks = 2)
    val info = requests.tracker.getRequestedInfo(Header.modifierTypeId, id)

    requests.tracker.setRequested(Header.modifierTypeId, id, peer, checksDone = 3) { _ =>
      throw new IllegalStateException("test scheduler unavailable")
    }

    previous._2.isCancelled shouldBe false
    requests.tracker.getRequestedInfo(Header.modifierTypeId, id) shouldBe info
  }

  it should "assign independent identities to same-object requests across types and replacement" in {
    val requests = new Requests
    val header = requests.request(Header.modifierTypeId)
    val section = requests.request(BlockTransactions.modifierTypeId)
    val replacement = requests.request(Header.modifierTypeId)

    Set(header._1.requestId, section._1.requestId, replacement._1.requestId).size shouldBe 3
    header._2.isCancelled shouldBe true
    section._2.isCancelled shouldBe false
    replacement._2.isCancelled shouldBe false
  }

  it should "ignore queued expirations after removal and direct replacement" in {
    Seq(true, false).foreach { removeFirst =>
      val requests = new Requests
      val controller = TestProbe()
      val actor = TestActorRef(new ExpirationConsumer(controller.ref, requests)(system.dispatcher))
      try {
        val old = requests.request(Header.modifierTypeId)._1
        if (removeFirst) requests.tracker.setUnknown(id, Header.modifierTypeId)
        requests.request(Header.modifierTypeId, checks = 2)
        val successor = requests.tracker.getRequestedInfo(Header.modifierTypeId, id)

        actor ! old

        requests.tracker.getRequestedInfo(Header.modifierTypeId, id) shouldBe successor
        requests.scheduled.size shouldBe 2
        controller.expectNoMessage(50.millis)
      } finally system.stop(actor)
    }
  }

  it should "ignore queued expirations after reset without modifying successor requests" in {
    Seq(ErgoTransaction.modifierTypeId, Header.modifierTypeId, BlockTransactions.modifierTypeId).foreach { typeId =>
      val requests = new Requests
      val controller = TestProbe()
      val actor = TestActorRef(new ExpirationConsumer(controller.ref, requests)(system.dispatcher))
      try {
        val old = requests.request(typeId)._1
        requests.tracker.reset()
        requests.request(typeId, checks = 2)
        val successor = requests.tracker.getRequestedInfo(typeId, id)

        actor ! old

        requests.tracker.getRequestedInfo(typeId, id) shouldBe successor
        requests.scheduled.size shouldBe 2
        controller.expectNoMessage(50.millis)
      } finally system.stop(actor)
    }
  }

  it should "process a current expiration and ignore its duplicate after retry" in {
    val requests = new Requests
    val controller = TestProbe()
    val actor = TestActorRef(new ExpirationConsumer(controller.ref, requests)(system.dispatcher))
    try {
      val current = requests.request(Header.modifierTypeId, checks = 2)._1
      actor ! current
      requests.tracker.getRequestedInfo(Header.modifierTypeId, id).get.checks shouldBe 3
      requests.scheduled.size shouldBe 2
      controller.receiveOne(1.second) should not be null
      val successor = requests.tracker.getRequestedInfo(Header.modifierTypeId, id)

      actor ! current

      requests.tracker.getRequestedInfo(Header.modifierTypeId, id) shouldBe successor
      requests.scheduled.size shouldBe 2
      controller.expectNoMessage(50.millis)
    } finally system.stop(actor)
  }

  it should "expire a current transaction request without retry" in {
    val requests = new Requests
    val controller = TestProbe()
    val actor = TestActorRef(new ExpirationConsumer(controller.ref, requests)(system.dispatcher))
    try {
      actor ! requests.request(ErgoTransaction.modifierTypeId)._1
      requests.tracker.status(id, ErgoTransaction.modifierTypeId, Seq.empty) shouldBe ModifiersStatus.Unknown
      requests.scheduled.size shouldBe 1
      controller.expectNoMessage(50.millis)
    } finally system.stop(actor)
  }

  override protected def afterAll(): Unit = {
    Await.result(system.terminate(), 10.seconds)
    super.afterAll()
  }
}
