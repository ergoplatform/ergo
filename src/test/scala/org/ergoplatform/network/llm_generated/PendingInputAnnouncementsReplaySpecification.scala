package org.ergoplatform.network

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.local.ErgoStatsCollector
import org.ergoplatform.mining.{AutolykosPowScheme, InputBlockFields}
import org.ergoplatform.modifiers.InputBlockTypeId
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{
  ChangedHistory, ChangedMempool, ChangedState, ProcessInputBlock
}
import org.ergoplatform.network.message.{InvData, RequestModifierSpec}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{ErgoStateContext, UtxoStateReader}
import org.ergoplatform.settings.Parameters
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.core.network.{ConnectedPeer, DeliveryTracker, ModifiersStatus}
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class PendingInputAnnouncementsReplaySpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants.{emptyStateContext, parameters}
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings
  import org.ergoplatform.utils.generators.ChainGenerator.genChain
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators.connectionIdGen

  private val blocks = genChain(3)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  private def announcement(n: Int): InputBlockAnnouncement =
    InputBlockAnnouncement(1, blocks(1).header.copy(timestamp = n.toLong),
      InputBlockFields.empty, None)

  private def withPeers(f: (ConnectedPeer, ConnectedPeer) => Unit): Unit = {
    implicit val system: ActorSystem = ActorSystem("pending-replay-store-test")
    try {
      val first = ConnectedPeer(connectionIdGen.sample.get.copy(
        remoteAddress = new java.net.InetSocketAddress("10.0.0.1", 9000)),
        TestProbe().ref, None)
      val second = ConnectedPeer(connectionIdGen.sample.get.copy(
        remoteAddress = new java.net.InetSocketAddress("10.0.0.2", 9000)),
        TestProbe().ref, None)
      f(first, second)
    } finally Await.result(system.terminate(), 10.seconds)
  }

  private def proxy[T](cls: Class[T])(f: (Method, Array[AnyRef]) => Any): T =
    cls.cast(Proxy.newProxyInstance(cls.getClassLoader, Array(cls), new InvocationHandler {
      override def invoke(p: Any, m: Method, a: Array[AnyRef]): AnyRef =
        f(m, Option(a).getOrElse(Array.empty[AnyRef])).asInstanceOf[AnyRef]
    }))

  private class SynchronizerFixture(implicit val system: ActorSystem) {
    implicit val ec = system.dispatcher
    val cfg = settings.copy(directory = java.nio.file.Files.createTempDirectory(
      new java.io.File("target").toPath, "pending-replay-").toFile.getAbsolutePath)
    val realHistory = ErgoHistory.readOrGenerate(cfg)(null)
    val nc = TestProbe()
    val vh = TestProbe()
    val peer = ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref, None)
    val pool = ErgoMemPool.empty(cfg)
    val tracker = DeliveryTracker.empty(cfg)
    var tip = blocks.head.header
    val parent = blocks(1).header
    var parentAvailable = false
    var validations = 0
    var stateContextReads = 0
    var replayValid = true
    var alreadyKnown = false
    var advanceDuringReplay = false
    val expectedDiff = BigInt(100)
    val expectedBits = org.ergoplatform.mining.difficulty.DifficultySerializer
      .encodeCompactBits(expectedDiff)
    val announcement = new InputBlockAnnouncement(1,
      blocks(2).header.copy(nBits = expectedBits), InputBlockFields.empty, None) {
      override def valid(pow: AutolykosPowScheme, ps: Parameters,
                         bits: Option[Long]): Boolean = {
        validations += 1
        bits shouldBe Some(expectedBits)
        replayValid
      }
    }
    def state(h: Header): UtxoStateReader = {
      val ctx = new ErgoStateContext(Seq(h), None, emptyStateContext.genesisStateDigest,
        parameters, emptyStateContext.validationSettings, emptyStateContext.votingData)(
        cfg.chainSettings)
      proxy(classOf[UtxoStateReader]) { (m, _) =>
        if (m.getName == "stateContext") {
          stateContextReads += 1
          ctx
        } else throw new AssertionError(m.getName)
      }
    }
    val hr = proxy(classOf[ErgoHistory]) { (m, args) => m.getName match {
      case "fullBlockHeight" => Int.box(tip.height)
      case "bestFullBlockIdOpt" => Some(tip.id)
      case "getInputBlock" if args.head == announcement.id =>
        // Change the reader after take's gate, before the processing height check.
        if (advanceDuringReplay) tip = announcement.header.copy(height = announcement.header.height + 1)
        if (alreadyKnown) Some(announcement) else None
      case "modifierById" | "typedModifierById" if args.head == parent.id =>
        if (parentAvailable) Some(parent) else None
      case "requiredDifficultyAfter" =>
        args.head shouldBe parent
        expectedDiff
      case _ => m.invoke(realHistory, args: _*)
    }}
    val stats = TestActorRef(new ErgoStatsCollector(TestProbe().ref, nc.ref,
      ErgoSyncTracker(cfg.scorexSettings.network), cfg))
    val ref = TestActorRef(new ErgoNodeViewSynchronizer(nc.ref, vh.ref,
      ErgoSyncInfoMessageSpec, cfg, ErgoSyncTracker(cfg.scorexSettings.network), tracker))
    vh.expectMsgType[org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetNodeViewChanges]
    ref ! ChangedHistory(hr)
    ref ! ChangedMempool(pool)
    ref ! ChangedState(state(tip))

    def info: io.circe.ACursor = {
      val probe = TestProbe()
      probe.send(stats, ErgoStatsCollector.GetNodeInfo)
      val nodeInfo = probe.expectMsgType[ErgoStatsCollector.NodeInfo]
      ErgoStatsCollector.NodeInfo.jsonEncoder(nodeInfo).hcursor
        .downField("pendingInputAnnouncements")
    }

    def announce(): Unit = {
      tracker.setRequested(InputBlockTypeId.value, announcement.id, peer)(
        _ => akka.actor.Cancellable.alreadyCancelled)
      ref.underlyingActor.processInputBlock(
        announcement, hr, pool, peer, Some(state(tip))) shouldBe false
      nc.fishForMessage(3.seconds) {
        case stn: SendToNetwork if stn.message.spec == RequestModifierSpec =>
          stn.message.data.get shouldBe InvData(Header.modifierTypeId, Seq(parent.id))
          stn.sendingStrategy shouldBe scorex.core.network.SendToPeer(peer)
          true
        case _ => false
      }
    }

    def applyParent(): Unit = {
      tip = parent
      parentAvailable = true
      ref ! ChangedHistory(hr)
      ref ! ChangedState(state(parent))
    }
  }

  private def withSynchronizer(f: SynchronizerFixture => Unit): Unit = {
    implicit val system: ActorSystem = ActorSystem("pending-replay-test")
    val fixture = new SynchronizerFixture
    try f(fixture)
    finally {
      Await.result(system.terminate(), 10.seconds)
      fixture.realHistory.closeStorage()
    }
  }

  private def rejectedReplay(invalid: Boolean, known: Boolean = false): Unit = {
    withSynchronizer { f =>
      f.announce()
      f.replayValid = !invalid
      f.alreadyKnown = known
      f.advanceDuringReplay = !invalid && !known
      f.applyParent()
      f.validations shouldBe (if (invalid) 1 else 0)
      f.vh.expectNoMessage(100.millis)
      val json = f.info
      json.get[Int]("size") shouldBe Right(0)
      json.get[Long]("replayed") shouldBe Right(1L)
      json.get[Long]("replayNotForwarded") shouldBe Right(if (invalid) 0L else 1L)
      json.get[Long]("replayInvalid") shouldBe Right(if (invalid) 1L else 0L)
      json.downField("drops").get[Long]("staleParent") shouldBe Right(0L)
      val penalties = f.nc.receiveWhile(100.millis) {
        case p: scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer => p
        case _: SendToNetwork => fail("A rejected replay must not request input-block data")
      }
      if (invalid) {
        penalties shouldBe Seq(
          scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer(
            f.peer.connectionId.remoteAddress,
            org.ergoplatform.network.peer.PenaltyType.MisbehaviorPenalty))
        f.tracker.status(f.announcement.id, InputBlockTypeId.value, Seq.empty) shouldBe
          ModifiersStatus.Unknown
      } else penalties shouldBe empty
    }
  }

  property("invalid replay increments only replayInvalid and penalizes the original sender") {
    rejectedReplay(invalid = true)
  }

  property("a replay falling below the tip after the gate is not counted as invalid") {
    rejectedReplay(invalid = false)
  }

  property("an already-known replay is not counted as invalid") {
    rejectedReplay(invalid = false, known = true)
  }

  property("an empty store skips state context reads on history changes before and after replay") {
    withSynchronizer { f =>
      val initialReads = f.stateContextReads
      (1 to 3).foreach(_ => f.ref ! ChangedHistory(f.hr))
      f.stateContextReads shouldBe initialReads

      f.announce()
      val heldReads = f.stateContextReads
      f.ref ! ChangedHistory(f.hr)
      f.stateContextReads shouldBe heldReads + 1
      f.validations shouldBe 0
      f.info.get[Int]("size") shouldBe Right(1)
      f.applyParent()
      f.validations shouldBe 1
      f.vh.expectMsg(ProcessInputBlock(f.announcement, f.peer))
      f.info.get[Int]("size") shouldBe Right(0)

      val drainedReads = f.stateContextReads
      (1 to 3).foreach(_ => f.ref ! ChangedHistory(f.hr))
      f.stateContextReads shouldBe drainedReads
      f.validations shouldBe 1
    }
  }

  property("the fairness filter never changes the busiest-host, incoming-host, oldest victim") {
    withPeers { (peer, _) =>
      val scenarios = for {
        hosts <- Gen.choose(1, 6)
        counts <- Gen.listOfN(hosts, Gen.choose(1, 5))
        incoming <- Gen.choose(0, hosts)
        headroom <- Gen.choose(0, 12)
        arrivalKeys <- Gen.listOfN(counts.sum, Gen.choose(-100, 100))
      } yield (counts, incoming, headroom, arrivalKeys)
      forAll(scenarios) { case (counts, incoming, headroom, arrivalKeys) =>
        val hostsInArrivalOrder = counts.zipWithIndex.flatMap { case (count, host) =>
          Seq.fill(count)(host)
        }.zip(arrivalKeys).sortBy(_._2).map(_._1).toVector
        val entries = hostsInArrivalOrder.zipWithIndex
        val entryCap = counts.sum + headroom
        val fairShare = entryCap / (counts.size + (if (incoming == counts.size) 1 else 0))
        val incomingCount = counts.lift(incoming).getOrElse(0) + 1
        val filtered = entries.filter { case (host, _) =>
          incomingCount <= fairShare || counts(host) >= fairShare
        }
        def priority(entry: (Int, Int)): (Int, Int) =
          (counts(entry._1), if (entry._1 == incoming) 1 else 0)
        val victim = entries.maxBy(priority)
        filtered.maxBy(priority) shouldBe victim
        // All busiest hosts survive, so neither the host tie-break nor FIFO changes.
        entries.filter(e => counts(e._1) == counts.max).foreach { entry =>
          filtered should contain (entry)
        }

        def host(n: Int): ConnectedPeer = peer.copy(connectionId = peer.connectionId.copy(
          remoteAddress = new java.net.InetSocketAddress(s"10.0.0.${n + 1}", 9000)))
        val held = entries.map { case (h, id) => announcement(id + 1) -> host(h) }
        val next = announcement(held.size + 1)
        def bytes(a: InputBlockAnnouncement): Long =
          InputBlockAnnouncement.serializer.toBytes(a).length.toLong
        // Force byte pressure even when the entry cap has headroom. Any one victim fits.
        val byteCap = held.map(e => bytes(e._1)).sum + bytes(next) - held.map(e => bytes(e._1)).min
        val store = new PendingInputAnnouncements(entryCap, byteCap, counts.max + 1,
          1000, () => 0L)
        var discarded = Vector.empty[InputBlockAnnouncement]
        store.onDiscard = (a, _) => discarded :+= a
        held.foreach { case (a, p) => store.add(a, p) shouldBe true }
        store.add(next, host(incoming)) shouldBe true
        discarded.map(_.id) shouldBe Vector(held(victim._2)._1.id)
        store.evictions shouldBe 1L
        store.take(blocks.head.header).map(_._1.id) shouldBe
          held.filterNot(_._1.id == held(victim._2)._1.id).map(_._1.id) :+ next.id
        store.size shouldBe 0
        store.byteSize shouldBe 0L
      }
    }
  }
}
