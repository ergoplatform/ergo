package org.ergoplatform.network

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, ChangedMempool, DisconnectedPeer, HandshakedPeer, SendLocalSyncInfo}
import org.ergoplatform.network.message.{InvData, InvSpec, Message, MessageSerializer, ModifiersData, ModifiersSpec, RequestModifierSpec}
import org.ergoplatform.network.peer.PeerInfo
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec, ErgoSyncInfoV2}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.ErgoNodeTestConstants._
import org.ergoplatform.utils.HistoryTestHelpers._
import org.ergoplatform.utils.generators.ChainGenerator._
import org.ergoplatform.utils.generators.ConnectedPeerGenerators._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.{ConnectedPeer, DeliveryTracker, SendToPeer}

import scala.concurrent.Await
import scala.concurrent.duration._

class V2BoundedProgressSpecification extends AnyPropSpec with Matchers {
  private def newHistory(): ErgoHistory =
    generateHistory(false, StateType.Digest, PoPoWBootstrap = false, blocksToKeep = 0)

  private lazy val selected = {
    val history = newHistory()
    try genHeaderChain(1000, history, diffBitsOpt = None, useRealTs = false)
    finally history.closeStorage()
  }

  private def firstPage: Seq[scorex.util.ModifierId] = selected.headers.slice(1, 400).map(_.id)

  private class Fixture {
    implicit val system: ActorSystem = ActorSystem("bounded-progress")
    private implicit val ec: scala.concurrent.ExecutionContext = system.dispatcher
    val network: TestProbe = TestProbe()
    val holder: TestProbe = TestProbe()
    val tracker: ErgoSyncTracker = ErgoSyncTracker(settings.scorexSettings.network)
    val delivery: DeliveryTracker = DeliveryTracker.empty(settings)
    val history: ErgoHistory = applyHeaderChain(newHistory(), selected)
    val peer: ConnectedPeer = ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref,
      Some(PeerInfo(defaultPeerSpec, 0L)))
    val actor: TestActorRef[ErgoNodeViewSynchronizer] = TestActorRef(Props(
      new ErgoNodeViewSynchronizer(network.ref, holder.ref, ErgoSyncInfoMessageSpec,
        settings, tracker, delivery) {
        override def preStart(): Unit = () // periodic delivery is driven explicitly below
      }))
    actor ! ChangedHistory(history)
    actor ! ChangedMempool(ErgoMemPool.empty(settings))
    tracker.updateStatus(peer, org.ergoplatform.consensus.Fork, Some(1000))

    def inventory(ids: Seq[scorex.util.ModifierId], remote: ConnectedPeer = peer): Unit =
      actor ! Message(InvSpec, Left(InvSpec.toBytes(InvData(Header.modifierTypeId, ids))), Some(remote))

    def requested(id: scorex.util.ModifierId): Unit =
      delivery.setRequested(Header.modifierTypeId, id, peer) { check =>
        system.scheduler.scheduleOnce(1.hour, holder.ref, check)
      }

    def sync(cursorPeer: Boolean = true): ErgoSyncInfoV2 = {
      val sent = network.expectMsgType[SendToNetwork](2.seconds)
      if (cursorPeer) sent.sendingStrategy shouldBe SendToPeer(peer)
      sent.message.spec shouldBe ErgoSyncInfoMessageSpec
      ErgoSyncInfoMessageSpec.parseBytesTry(sent.message.dataBytes).get.asInstanceOf[ErgoSyncInfoV2]
    }

    def periodic(): Unit = {
      tracker.statuses.update(peer, tracker.statuses(peer).copy(lastSyncSentTime = Some(0L)))
      actor ! SendLocalSyncInfo
    }

    def close(): Unit = {
      Await.result(system.terminate(), 10.seconds)
      history.closeStorage()
    }
  }

  private def withFixture(test: Fixture => Unit): Unit = {
    val fixture = new Fixture
    try test(fixture) finally fixture.close()
  }

  property("V2 full locator includes selected genesis as the bounded-progress dependency") {
    withFixture { f =>
      f.history.syncInfoV2(full = true).lastHeaders.last.id shouldBe selected.head.id
    }
  }

  property("V2 bounded progress discovers the first unknown header beyond an all-held page") {
    withFixture { f =>
      var other = applyHeaderChain(newHistory(), selected.take(450))
      try {
        val fork = genHeaderChain(550, Some(selected.headers(449)), other.difficultyCalculator,
          extensionHash = Algos.hash(Array[Byte](1)), diffBitsOpt = None, useRealTs = false).tail
        other = applyHeaderChain(other, fork)
        val initial = f.history.syncInfoV2(full = true)
        val first = other.continuationIdsV2(initial, 400).map(_._2)
        first should have size 399
        first.last shouldBe selected.headers(399).id
        f.inventory(first)
        val progressed = f.sync()
        progressed.lastHeaders.head.id shouldBe selected.last.id
        progressed.lastHeaders.map(_.height) shouldBe Seq(1000, 984, 872, 488, 400, 1)
        progressed.lastHeaders.size should be <= org.ergoplatform.nodeView.history.ErgoSyncInfoSerializer.MaxHeadersAllowed
        val next = other.continuationIdsV2(progressed, 400).map(_._2)
        next.head shouldBe selected.headers(400).id
        next should contain(fork.head.id)
        f.inventory(next)
        f.network.expectMsgType[SendToNetwork].message.spec shouldBe org.ergoplatform.network.message.RequestModifierSpec
      } finally other.closeStorage()
    }
  }

  property("V2 bounded progress advances across a fully held side-fork page") {
    withFixture { f =>
      var peerHistory = applyHeaderChain(newHistory(), selected.take(200))
      try {
        val side = genHeaderChain(802, Some(selected.headers(199)), peerHistory.difficultyCalculator,
          extensionHash = Algos.hash(Array[Byte](7)), diffBitsOpt = None, useRealTs = false).tail
        peerHistory = applyHeaderChain(peerHistory, side)
        val first = peerHistory.continuationIdsV2(f.history.syncInfoV2(full = true), 400).map(_._2)
        first should have size 399
        first.last shouldBe side.headers(199).id

        // The first offer has unknown side headers, which are requested normally.
        f.inventory(first)
        f.network.expectMsgType[SendToNetwork].message.spec shouldBe
          org.ergoplatform.network.message.RequestModifierSpec

        val updatedHistory = applyHeaderChain(f.history, side.take(200))
        updatedHistory.contains(side.headers(199).id) shouldBe true
        updatedHistory.isInBestChain(side.headers(199).id) shouldBe false
        f.actor ! ChangedHistory(updatedHistory)

        // Once the page is held, the receiver must offer the side cursor back to
        // this peer so the peer can reveal its next, previously unknown header.
        f.inventory(first)
        val progressed = f.sync()
        progressed.lastHeaders.map(_.id) should contain(side.headers(199).id)
        // Exercise the real responder dispatch. A shorter responder would
        // classify our selected A1000 tip as Older and never send this page.
        implicit val ec: scala.concurrent.ExecutionContext = f.system.dispatcher
        val responderNetwork = TestProbe()(f.system)
        val responderTracker = ErgoSyncTracker(settings.scorexSettings.network)
        responderTracker.updateStatus(f.peer, org.ergoplatform.consensus.Unknown, Some(1000))
        val responder = TestActorRef[ErgoNodeViewSynchronizer](Props(
          new ErgoNodeViewSynchronizer(responderNetwork.ref, f.holder.ref, ErgoSyncInfoMessageSpec,
            settings, responderTracker, DeliveryTracker.empty(settings)) {
            override def preStart(): Unit = ()
          }))(f.system)
        responder ! ChangedHistory(peerHistory)
        responder ! ChangedMempool(ErgoMemPool.empty(settings))
        responder ! Message(ErgoSyncInfoMessageSpec,
          Left(ErgoSyncInfoMessageSpec.toBytes(progressed)), Some(f.peer))
        val reply = responderNetwork.expectMsgType[SendToNetwork]
        reply.message.spec shouldBe InvSpec
        val next = InvSpec.parseBytesTry(reply.message.dataBytes).get.ids
        next.head shouldBe side.headers(200).id
        next should contain(side.headers(399).id)
        f.inventory(next)
        f.network.expectMsgType[SendToNetwork].message.spec shouldBe
          org.ergoplatform.network.message.RequestModifierSpec
      } finally peerHistory.closeStorage()
    }
  }

  property("V2 bounded progress does not advance when the first inventory is lost") {
    withFixture { f =>
      f.periodic()
      f.sync(cursorPeer = false).lastHeaders.map(_.height) shouldBe Seq(1000, 984, 872, 488, 1)
    }
  }

  property("V2 held pages from competing peers are paced and a disconnected peer loses its turn") {
    withFixture { f =>
      val second = ConnectedPeer(connectionIdGen.sample.get, TestProbe()(f.system).ref,
        Some(PeerInfo(defaultPeerSpec, 0L)))
      val third = ConnectedPeer(connectionIdGen.sample.get, TestProbe()(f.system).ref,
        Some(PeerInfo(defaultPeerSpec, 0L)))
      Seq(second, third).foreach(p => f.tracker.updateStatus(p, org.ergoplatform.consensus.Fork, Some(1000)))

      f.inventory(firstPage)
      f.sync().lastHeaders.map(_.height) should contain(400)
      f.inventory(firstPage, second)
      f.inventory(firstPage, third)
      f.actor ! DisconnectedPeer(third)

      val next = f.network.expectMsgType[SendToNetwork](2.seconds)
      next.sendingStrategy shouldBe SendToPeer(second)
      f.network.expectNoMessage(400.millis)
    }
  }

  property("V2 held-page queue gives every peer a turn despite another peer's new page") {
    withFixture { f =>
      val second = ConnectedPeer(connectionIdGen.sample.get, TestProbe()(f.system).ref,
        Some(PeerInfo(defaultPeerSpec, 0L)))
      val third = ConnectedPeer(connectionIdGen.sample.get, TestProbe()(f.system).ref,
        Some(PeerInfo(defaultPeerSpec, 0L)))
      Seq(second, third).foreach(p => f.tracker.updateStatus(p, org.ergoplatform.consensus.Fork, Some(1000)))

      f.inventory(firstPage)
      f.sync()
      f.inventory(firstPage, second)
      f.inventory(firstPage, third)
      Thread.sleep(125) // let the original peer's admission lock expire before its next page
      f.inventory(selected.headers.slice(2, 401).map(_.id))

      val replies = f.network.receiveN(3, 2.seconds).map(_.asInstanceOf[SendToNetwork])
      replies.map(_.sendingStrategy) shouldBe Seq(SendToPeer(second), SendToPeer(third), SendToPeer(f.peer))
      ErgoSyncInfoMessageSpec.parseBytesTry(replies.last.message.dataBytes).get
        .asInstanceOf[ErgoSyncInfoV2].lastHeaders.map(_.height) should contain(401)
    }
  }

  property("V2 queued page replacement sends its reply to the current connection handler") {
    withFixture { f =>
      val second = ConnectedPeer(connectionIdGen.sample.get, TestProbe()(f.system).ref,
        Some(PeerInfo(defaultPeerSpec, 0L)))
      val third = ConnectedPeer(connectionIdGen.sample.get, TestProbe()(f.system).ref,
        Some(PeerInfo(defaultPeerSpec, 0L)))
      val old = ConnectedPeer(connectionIdGen.sample.get, TestProbe()(f.system).ref,
        Some(PeerInfo(defaultPeerSpec, 0L)))
      val current = old.copy(handlerRef = TestProbe()(f.system).ref)
      Seq(second, third, old).foreach(p =>
        f.tracker.updateStatus(p, org.ergoplatform.consensus.Fork, Some(1000)))

      f.inventory(firstPage)
      f.sync()
      f.inventory(firstPage, second)
      f.inventory(firstPage, third)
      f.inventory(firstPage, old)
      Thread.sleep(125) // replace while the preceding two queued peers hold this slot back
      f.inventory(selected.headers.slice(2, 401).map(_.id), current)

      val replies = f.network.receiveN(3, 3.seconds).map(_.asInstanceOf[SendToNetwork])
      replies.map(_.sendingStrategy) shouldBe Seq(SendToPeer(second), SendToPeer(third), SendToPeer(current))
      replies.last.sendingStrategy.asInstanceOf[SendToPeer].chosenPeer.handlerRef shouldBe current.handlerRef
      ErgoSyncInfoMessageSpec.parseBytesTry(replies.last.message.dataBytes).get
        .asInstanceOf[ErgoSyncInfoV2].lastHeaders.map(_.height) should contain(401)
    }
  }

  property("V2 header-triggered sync retains the peer's confirmed inventory cursor") {
    withFixture { f =>
      f.inventory(firstPage)
      f.sync().lastHeaders.map(_.height) should contain(400)
      val incoming = genHeaderChain(1, Some(selected.last), f.history.difficultyCalculator,
        diffBitsOpt = None, useRealTs = false).last
      f.requested(incoming.id)
      val data = ModifiersData(Header.modifierTypeId, Map(incoming.id -> incoming.bytes))
      f.actor ! Message(ModifiersSpec, Left(ModifiersSpec.toBytes(data)), Some(f.peer))
      val afterHeader = f.sync()
      afterHeader.lastHeaders.head.id shouldBe selected.last.id
      afterHeader.lastHeaders.map(_.height) should contain(400)
      afterHeader.lastHeaders.last.id shouldBe selected.head.id
      // The Header path resets lastSyncSentTime, so a periodic retry is
      // suppressed until the normal minimum interval elapses.
      f.actor ! SendLocalSyncInfo
      f.network.expectNoMessage(50.millis)
      // After reconnect, this peer has no cursor. The Header response must not
      // have poisoned the ordinary full-summary cache with a tip-only value.
      f.actor ! DisconnectedPeer(f.peer)
      f.actor ! HandshakedPeer(f.peer)
      f.periodic()
      f.sync(cursorPeer = false).lastHeaders.map(_.height) shouldBe Seq(1000, 984, 872, 488, 1)
    }
  }

  property("V2 peer locator keeps the selected tip first even when a side cursor is higher") {
    val tip = selected.last
    val sideCursor = selected.headers(399).copy(height = tip.height + 1)
    val ordinary = ErgoSyncInfoV2(Seq(tip, selected.headers(983), selected.headers(871),
      selected.headers(487), selected.head))
    val composed = ErgoNodeViewSynchronizer.withHeaderInventoryCursor(ordinary, sideCursor)
    val wire = ErgoSyncInfoMessageSpec.parseBytesTry(ErgoSyncInfoMessageSpec.toBytes(composed)).get
      .asInstanceOf[ErgoSyncInfoV2]
    wire.lastHeaders.head.id shouldBe tip.id
    wire.height shouldBe Some(tip.height)
    wire.lastHeaders(1).id shouldBe sideCursor.id
  }

  property("V2 bounded progress rejects a requested or received header absent from history") {
    Seq(false, true).foreach { received =>
      withFixture { f =>
        val absent = scorex.util.bytesToId(Array.fill[Byte](32)(3))
        if (received) f.delivery.setReceivedDirectly(absent, Header.modifierTypeId, f.peer)
        else f.requested(absent)
        f.inventory(firstPage.dropRight(1) :+ absent)
        f.network.expectNoMessage(100.millis)
        f.periodic()
        f.sync(cursorPeer = false).lastHeaders.map(_.height) should not contain 400
      }
    }
  }

  property("V2 bounded progress requests a missing header without installing a cursor") {
    withFixture { f =>
      val absent = scorex.util.bytesToId(Array.fill[Byte](32)(4))
      f.inventory(firstPage.dropRight(1) :+ absent)
      f.network.expectMsgType[SendToNetwork].message.spec shouldBe org.ergoplatform.network.message.RequestModifierSpec
      f.periodic()
      f.sync(cursorPeer = false).lastHeaders.map(_.height) should not contain 400
    }
  }

  property("V2 bounded progress rejects duplicate, unordered, noncontiguous, and short pages") {
    Seq(firstPage.dropRight(1) :+ firstPage.head, firstPage.reverse,
      firstPage.dropRight(1) :+ selected.headers(400).id, firstPage.dropRight(1)).foreach { page =>
      withFixture { f =>
        f.inventory(page)
        f.network.expectNoMessage(50.millis)
        f.periodic()
        f.sync(cursorPeer = false).lastHeaders.map(_.height) should not contain 400
      }
    }
  }

  property("V2 bounded progress accepts a held contiguous side header") {
    withFixture { f =>
      val side = genHeaderChain(1, Some(selected.headers(398)), f.history.difficultyCalculator,
        extensionHash = Algos.hash(Array[Byte](2)), diffBitsOpt = None, useRealTs = false).last
      val progress = f.history.append(side).get._2
      f.history.contains(side.id) shouldBe true
      f.history.isInBestChain(side.id) shouldBe false
      f.inventory(firstPage.dropRight(1) :+ side.id)
      val summary = f.sync()
      summary.lastHeaders.head.id shouldBe selected.last.id
      summary.lastHeaders.map(_.id) should contain(side.id)
      f.history.reportModifierIsInvalid(side, progress).get
      f.history.typedModifierById[Header](side.id) shouldBe None
      f.periodic()
      f.sync().lastHeaders.map(_.id) should not contain side.id
    }
  }

  property("V2 bounded progress rejects a persisted-invalid side header") {
    withFixture { f =>
      val side = genHeaderChain(1, Some(selected.headers(398)), f.history.difficultyCalculator,
        extensionHash = Algos.hash(Array[Byte](2)), diffBitsOpt = None, useRealTs = false).last
      val progress = f.history.append(side).get._2
      f.history.reportModifierIsInvalid(side, progress).get
      f.history.typedModifierById[Header](side.id) shouldBe None
      f.inventory(firstPage.dropRight(1) :+ side.id)
      f.network.expectNoMessage(50.millis)
      f.periodic()
      f.sync(cursorPeer = false).lastHeaders.map(_.id) should not contain side.id
    }
  }

  property("V2 bounded progress retries duplicate inventories periodically after a deterministic responder lock drop") {
    withFixture { f =>
      var responderHistory = applyHeaderChain(newHistory(), selected.take(450))
      try {
        val fork = genHeaderChain(550, Some(selected.headers(449)), responderHistory.difficultyCalculator,
          extensionHash = Algos.hash(Array[Byte](5)), diffBitsOpt = None, useRealTs = false).tail
        responderHistory = applyHeaderChain(responderHistory, fork)
        implicit val system: ActorSystem = f.system
        implicit val ec: scala.concurrent.ExecutionContext = system.dispatcher
        val responderNetwork = TestProbe()
        val responderTracker = ErgoSyncTracker(settings.scorexSettings.network)
        responderTracker.updateStatus(f.peer, org.ergoplatform.consensus.Unknown, Some(1000))
        val responder = TestActorRef[ErgoNodeViewSynchronizer](Props(
          new ErgoNodeViewSynchronizer(responderNetwork.ref, f.holder.ref, ErgoSyncInfoMessageSpec,
            settings, responderTracker, DeliveryTracker.empty(settings)) {
            override def preStart(): Unit = ()
          }))
        responder ! ChangedHistory(responderHistory)
        responder ! ChangedMempool(ErgoMemPool.empty(settings))
        responder ! Message(ErgoSyncInfoMessageSpec,
          Left(ErgoSyncInfoMessageSpec.toBytes(f.history.syncInfoV2(full = true))), Some(f.peer))
        val first = responderNetwork.expectMsgType[SendToNetwork]
        first.message.spec shouldBe InvSpec
        InvSpec.parseBytesTry(first.message.dataBytes).get.ids shouldBe firstPage
        responderNetwork.expectMsgType[SendToNetwork].message.spec shouldBe ErgoSyncInfoMessageSpec
        f.actor ! Message(InvSpec, Left(first.message.dataBytes), Some(f.peer))
        val immediate = f.sync()
        // A future receive timestamp forces the lock branch independently of CPU/CI delays.
        responderTracker.statuses.update(f.peer, responderTracker.statuses(f.peer)
          .copy(lastSyncGetTime = Some(System.currentTimeMillis() + 1.hour.toMillis)))
        responder ! Message(ErgoSyncInfoMessageSpec,
          Left(ErgoSyncInfoMessageSpec.toBytes(immediate)), Some(f.peer))
        responderNetwork.expectNoMessage(20.millis)
        f.inventory(firstPage)
        // Depending on elapsed real time, the duplicate may be throttled or answered.
        // Both outcomes must preserve the confirmed cursor for the independent periodic retry.
        f.network.receiveWhile(20.millis) {
          case sent: SendToNetwork =>
            sent.message.spec shouldBe ErgoSyncInfoMessageSpec
            ErgoSyncInfoMessageSpec.parseBytesTry(sent.message.dataBytes).get
              .asInstanceOf[ErgoSyncInfoV2].lastHeaders shouldBe immediate.lastHeaders
        }
        f.periodic()
        val retry = f.sync()
        retry.lastHeaders shouldBe immediate.lastHeaders
        responderTracker.statuses.update(f.peer, responderTracker.statuses(f.peer)
          .copy(lastSyncGetTime = Some(0L)))
        responder ! Message(ErgoSyncInfoMessageSpec,
          Left(ErgoSyncInfoMessageSpec.toBytes(retry)), Some(f.peer))
        val sent = responderNetwork.expectMsgType[SendToNetwork]
        sent.message.spec shouldBe InvSpec
        val ids = InvSpec.parseBytesTry(sent.message.dataBytes).get.ids
        ids.head shouldBe selected.headers(400).id
        ids should contain(fork.head.id)
        f.actor ! Message(InvSpec, Left(sent.message.dataBytes), Some(f.peer))
        val request = f.network.expectMsgType[SendToNetwork]
        request.message.spec shouldBe org.ergoplatform.network.message.RequestModifierSpec
        val requestedIds = org.ergoplatform.network.message.RequestModifierSpec
          .parseBytesTry(request.message.dataBytes).get.ids
        requestedIds should contain(fork.head.id)
        requestedIds should not contain selected.headers(400).id
      } finally responderHistory.closeStorage()
    }
  }

  property("V2 scheduler retries a lost held-page cursor over serialized peer messages") {
    val localHistory = applyHeaderChain(newHistory(), selected)
    var peerHistory = applyHeaderChain(newHistory(), selected.take(450))
    try {
      val fork = genHeaderChain(550, Some(selected.headers(449)), peerHistory.difficultyCalculator,
        extensionHash = Algos.hash(Array[Byte](5)), diffBitsOpt = None, useRealTs = false).tail
      peerHistory = applyHeaderChain(peerHistory, fork)

      implicit val system: ActorSystem = ActorSystem("bounded-progress-natural-retry")
      val ec: scala.concurrent.ExecutionContext = system.dispatcher
      try {
        val localNetwork = TestProbe()
        val peerNetwork = TestProbe()
        val holder = TestProbe()
        val peer = ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref,
          Some(PeerInfo(defaultPeerSpec, 0L)))
        val localTracker = ErgoSyncTracker(settings.scorexSettings.network)
        val peerTracker = ErgoSyncTracker(settings.scorexSettings.network)
        val local = system.actorOf(Props(new ErgoNodeViewSynchronizer(localNetwork.ref, holder.ref,
          ErgoSyncInfoMessageSpec, settings, localTracker, DeliveryTracker.empty(settings))(ec)))
        val responder = TestActorRef(Props(new ErgoNodeViewSynchronizer(peerNetwork.ref, holder.ref,
          ErgoSyncInfoMessageSpec, settings, peerTracker, DeliveryTracker.empty(settings))(ec) {
          override def preStart(): Unit = ()
        }))
        val wire = new MessageSerializer(Seq(ErgoSyncInfoMessageSpec, InvSpec, RequestModifierSpec),
          settings.scorexSettings.network.magicBytes)

        def decoded(sent: SendToNetwork): Message[_] = {
          val frame = wire.serialize(sent.message)
          val message = wire.deserialize(frame, Some(peer)).get.get
          message.messageLength shouldBe frame.length
          message
        }

        def nextSent(network: TestProbe, timeout: FiniteDuration)
                    (accept: SendToNetwork => Boolean): SendToNetwork = {
          val deadline = timeout.fromNow
          var result: Option[SendToNetwork] = None
          while (result.isEmpty && deadline.hasTimeLeft()) {
            network.receiveOne(deadline.timeLeft) match {
              case sent: SendToNetwork if accept(sent) => result = Some(sent)
              case _ =>
            }
          }
          result.getOrElse(fail(s"No matching peer message within $timeout"))
        }

        local ! ChangedHistory(localHistory)
        local ! ChangedMempool(ErgoMemPool.empty(settings))
        local ! HandshakedPeer(peer)
        responder ! ChangedHistory(peerHistory)
        responder ! ChangedMempool(ErgoMemPool.empty(settings))
        responder ! HandshakedPeer(peer)

        // The first SyncInfo comes from preStart's actual SendLocalSyncInfo timer.
        val initial = nextSent(localNetwork, 10.seconds)(_.message.spec == ErgoSyncInfoMessageSpec)
        initial.sendingStrategy.choose(Seq(peer)) should contain(peer)
        val initialInfo = ErgoSyncInfoMessageSpec.parseBytesTry(decoded(initial).dataBytes).get
          .asInstanceOf[ErgoSyncInfoV2]
        initialInfo.lastHeaders.head.id shouldBe selected.last.id
        initialInfo.lastHeaders.last.id shouldBe selected.head.id
        responder ! decoded(initial)

        val first = nextSent(peerNetwork, 5.seconds)(_.message.spec == InvSpec)
        first.sendingStrategy shouldBe SendToPeer(peer)
        InvSpec.parseBytesTry(decoded(first).dataBytes).get.ids shouldBe firstPage
        local ! decoded(first)
        val immediate = nextSent(localNetwork, 5.seconds)(_.message.spec == ErgoSyncInfoMessageSpec)
        immediate.sendingStrategy shouldBe SendToPeer(peer)
        val cursor = ErgoSyncInfoMessageSpec.parseBytesTry(decoded(immediate).dataBytes).get
          .asInstanceOf[ErgoSyncInfoV2]
        cursor.lastHeaders.map(_.height) should contain(400)

        // Lose this one frame at the transport boundary. The next cursor must be
        // produced by the receiver's real periodic scheduler without a new Inv.
        val retry = nextSent(localNetwork, 40.seconds)(_.message.spec == ErgoSyncInfoMessageSpec)
        retry.sendingStrategy shouldBe SendToPeer(peer)
        val retriedCursor = ErgoSyncInfoMessageSpec.parseBytesTry(decoded(retry).dataBytes).get
          .asInstanceOf[ErgoSyncInfoV2]
        retriedCursor.lastHeaders shouldBe cursor.lastHeaders
        responder ! decoded(retry)

        val continuation = nextSent(peerNetwork, 5.seconds)(_.message.spec == InvSpec)
        continuation.sendingStrategy shouldBe SendToPeer(peer)
        val continuationIds = InvSpec.parseBytesTry(decoded(continuation).dataBytes).get.ids
        continuationIds.head shouldBe selected.headers(400).id
        continuationIds should contain(fork.head.id)
        local ! decoded(continuation)
        val request = nextSent(localNetwork, 5.seconds) { sent =>
          sent.message.spec == RequestModifierSpec &&
            RequestModifierSpec.parseBytesTry(decoded(sent).dataBytes).get.ids.contains(fork.head.id)
        }
        request.sendingStrategy shouldBe SendToPeer(peer)
        val requestedIds = RequestModifierSpec.parseBytesTry(decoded(request).dataBytes).get.ids
        requestedIds should contain(fork.head.id)
        requestedIds should not contain selected.headers(400).id
      } finally Await.result(system.terminate(), 10.seconds)
    } finally {
      peerHistory.closeStorage()
      localHistory.closeStorage()
    }
  }

  property("V2 bounded progress clears on disconnect and reconnect and rejects V1 peers") {
    withFixture { f =>
      f.inventory(firstPage)
      f.sync()
      f.actor ! DisconnectedPeer(f.peer)
      f.actor ! HandshakedPeer(f.peer)
      f.periodic()
      f.sync(cursorPeer = false).lastHeaders.map(_.height) should not contain 400
      val legacy = f.peer.copy(peerInfo = Some(PeerInfo(defaultPeerSpec.copy(protocolVersion = Version.initial), 0L)))
      f.inventory(firstPage, legacy)
      f.network.expectNoMessage(50.millis)
    }
  }

  property("V2 bounded progress survives a selected-chain extension") {
    withFixture { f =>
      f.inventory(firstPage)
      f.sync().lastHeaders.map(_.height) should contain(400)

      val oldTip = f.history.bestHeaderOpt.get
      val extension = genHeaderChain(2, Some(oldTip), f.history.difficultyCalculator,
        extensionHash = Algos.hash(Array[Byte](6)), diffBitsOpt = None, useRealTs = false).tail
      val extended = applyHeaderChain(f.history, extension)
      extended.bestHeaderIdOpt shouldBe Some(extension.last.id)
      extended.isInBestChain(oldTip.id) shouldBe true

      f.actor ! ChangedHistory(extended)
      f.periodic()
      val advertised = f.sync()
      advertised.lastHeaders.head.id shouldBe extension.last.id
      advertised.lastHeaders.map(_.height) should contain(400)
    }
  }

  property("V2 bounded progress drops its cursor when the former selected branch remains stored") {
    withFixture { f =>
      val second = ConnectedPeer(connectionIdGen.sample.get, TestProbe()(f.system).ref,
        Some(PeerInfo(defaultPeerSpec, 0L)))
      f.tracker.updateStatus(second, org.ergoplatform.consensus.Fork, Some(1000))
      f.inventory(firstPage)
      f.sync()
      f.tracker.statuses.update(second, f.tracker.statuses(second).copy(lastSyncSentTime = Some(0L)))
      f.periodic()
      f.sync().lastHeaders.map(_.height) should contain(400)
      val otherPeer = f.network.expectMsgType[SendToNetwork]
      otherPeer.sendingStrategy.choose(Seq(second)) should contain(second)
      ErgoSyncInfoMessageSpec.parseBytesTry(otherPeer.message.dataBytes).get
        .asInstanceOf[ErgoSyncInfoV2].lastHeaders.map(_.height) should not contain 400

      val oldTip = f.history.bestHeaderOpt.get
      val cursor = selected.headers(399)
      val fork = genHeaderChain(1001, Some(selected.head), f.history.difficultyCalculator,
        extensionHash = Algos.hash(Array[Byte](7)), diffBitsOpt = None, useRealTs = false).tail
      val replacement = applyHeaderChain(f.history, fork)
      replacement.bestHeaderIdOpt shouldBe Some(fork.last.id)
      replacement.contains(oldTip.id) shouldBe true
      replacement.isInBestChain(oldTip.id) shouldBe false
      replacement.contains(cursor.id) shouldBe true
      replacement.isInBestChain(cursor.id) shouldBe false

      f.actor ! ChangedHistory(replacement)
      f.periodic()
      val advertised = f.sync()
      advertised.lastHeaders.head.id shouldBe fork.last.id
      advertised.lastHeaders.map(_.id) should not contain cursor.id
    }
  }
}
