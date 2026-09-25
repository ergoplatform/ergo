package org.ergoplatform.network

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.mining.{AutolykosPowScheme, InputBlockFields}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{
  ChangedHistory, ChangedMempool, ChangedState, ProcessInputBlock, RemoteBlockApplied
}
import org.ergoplatform.network.message.{InvData, RequestModifierSpec}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{ErgoStateContext, UtxoStateReader}
import org.ergoplatform.settings.Parameters
import org.ergoplatform.subblocks.InputBlockAnnouncement
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.scalacheck.Gen
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.util.ModifierId
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class PendingInputAnnouncementsSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings
  import org.ergoplatform.utils.ErgoCoreTestConstants.{emptyStateContext, parameters}
  import org.ergoplatform.utils.generators.ChainGenerator.genChain
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators.connectionIdGen

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  private val blocks = genChain(3)
  private def announcement(n: Int, height: Int = 2): InputBlockAnnouncement =
    InputBlockAnnouncement(1, blocks(1).header.copy(height = height, timestamp = n.toLong),
      InputBlockFields.empty, None)

  private def proxy[T](cls: Class[T])(f: (Method, Array[AnyRef]) => Any): T =
    cls.cast(Proxy.newProxyInstance(cls.getClassLoader, Array(cls), new InvocationHandler {
      override def invoke(p: Any, m: Method, a: Array[AnyRef]): AnyRef =
        f(m, Option(a).getOrElse(Array.empty[AnyRef])).asInstanceOf[AnyRef]
    }))

  private def withPeers(f: (ConnectedPeer, ConnectedPeer) => Unit): Unit = {
    implicit val system: ActorSystem = ActorSystem("pending-store-test")
    try {
      f(ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref, None),
        ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref, None))
    } finally Await.result(system.terminate(), 10.seconds)
  }

  property("global entry and byte caps evict oldest and count evictions") {
    withPeers { (p, q) =>
      val a = announcement(1)
      val b = announcement(2)
      val c = announcement(3)
      val bytes = InputBlockAnnouncement.serializer.toBytes(a).length.toLong
      Seq(new PendingInputAnnouncements(2, bytes * 10, 10, 1000, () => 0L),
        new PendingInputAnnouncements(10, bytes * 2, 10, 1000, () => 0L)).foreach { s =>
        s.add(a, p) shouldBe true
        s.add(b, q) shouldBe true
        s.add(c, p) shouldBe true
        s.size shouldBe 2
        s.byteSize shouldBe bytes * 2
        s.evictions shouldBe 1
        s.take(blocks.head.header).map(_._1.id) shouldBe Seq(b.id, c.id)
        s.byteSize shouldBe 0
      }
      val tiny = new PendingInputAnnouncements(2, bytes - 1, 2, 1000, () => 0L)
      tiny.add(a, p) shouldBe false
      tiny.size shouldBe 0
    }
  }

  property("per-peer admission cannot evict another peer's entries") {
    withPeers { (p, q) =>
      val s = new PendingInputAnnouncements(3, 100000, 1, 1000, () => 0L)
      s.add(announcement(1), p) shouldBe true
      s.add(announcement(2), p) shouldBe false
      s.add(announcement(3), q) shouldBe true
      s.size shouldBe 2
      s.evictions shouldBe 0
    }
  }

  property("identical serialized announcements deduplicate without refreshing expiry") {
    withPeers { (p, q) =>
      var now = 0L
      val s = new PendingInputAnnouncements(3, 100000, 2, 1000, () => now)
      val a = announcement(1)
      s.add(a, p) shouldBe true
      now = 900
      s.add(a, q) shouldBe false
      s.size shouldBe 1
      now = 1001
      s.take(blocks.head.header) shouldBe empty
      s.size shouldBe 0
      s.byteSize shouldBe 0
    }
  }

  property("stale +1 and nonviable known +2 parents drop; unknown +2 parents wait for TTL") {
    withPeers { (p, _) =>
      val s = new PendingInputAnnouncements(10, 100000, 10, 1000, () => 0L)
      var discarded = Set.empty[ModifierId]
      s.onDiscard = (a, _) => discarded += a.id
      val stale = announcement(1, 1)
      val oldParent = announcement(2).copy(header = announcement(2).header.copy(
        parentId = blocks.last.id))
      s.add(stale, p)
      s.add(oldParent, p)
      val current = announcement(3, 3).copy(header = announcement(3, 3).header.copy(
        parentId = blocks(1).id))
      val forked = announcement(4, 3).copy(header = announcement(4, 3).header.copy(
        parentId = blocks.last.id))
      val unknown = announcement(5, 3)
      s.add(current, p)
      s.add(forked, p)
      s.add(unknown, p)
      s.take(blocks.head.header, 64, Map(blocks(1).id -> blocks(1).header,
        blocks.last.id -> blocks.last.header).get) shouldBe empty
      s.size shouldBe 2
      discarded shouldBe Set(stale.id, oldParent.id, forked.id)
      s.take(blocks(1).header).map(_._1.id) shouldBe Seq(current.id)
      s.size shouldBe 0
      s.byteSize shouldBe 0
    }
  }

  property("an unknown +2 parent survives repeated takes and leaves only by TTL") {
    withPeers { (p, _) =>
      var now = 0L
      val s = new PendingInputAnnouncements(10, 100000, 10, 1000, () => now)
      s.add(announcement(20, 3), p) shouldBe true
      (1 to 3).foreach { _ =>
        s.take(blocks.head.header, 64, _ => None) shouldBe empty
        s.size shouldBe 1
        now += 300
      }
      now = 1000
      s.take(blocks.head.header) shouldBe empty
      s.size shouldBe 0
      s.drops("expired") shouldBe 1L
    }
  }

  property("a +2 root under a known sibling that extends the tip is held until the tip moves") {
    withPeers { (p, _) =>
      val s = new PendingInputAnnouncements(10, 100000, 10, 1000, () => 0L)
      val sibling = blocks(1).header.copy(timestamp = 7654L)
      val root = announcement(21, 3).copy(header = announcement(21, 3).header.copy(
        parentId = sibling.id))
      s.add(root, p) shouldBe true
      s.take(blocks.head.header, 64, Map(sibling.id -> sibling).get) shouldBe empty
      s.size shouldBe 1
      s.take(sibling).map(_._1.id) shouldBe Seq(root.id)
    }
  }

  property("a winning +2 parent survives while the applied tip is on the losing fork") {
    withPeers { (p, _) =>
      val tip = blocks.head.header
      val parent = blocks(1).header.copy(parentId = blocks.last.id)
      val root = announcement(22, tip.height + 2).copy(
        header = announcement(22, tip.height + 2).header.copy(parentId = parent.id))
      val s = new PendingInputAnnouncements(10, 100000, 10, 1000, () => 0L)
      s.add(root, p) shouldBe true
      s.take(tip, 64, Map(parent.id -> parent).get, _ == parent) shouldBe empty
      s.size shouldBe 1
      s.take(parent).map(_._1.id) shouldBe Seq(root.id)
      s.add(root, p) shouldBe true
      s.take(tip, 64, Map(parent.id -> parent).get, _ => false) shouldBe empty
      s.size shouldBe 0

    }
  }

  property("TTL uses elapsed monotonic milliseconds, including a negative clock origin") {
    withPeers { (p, q) =>
      var elapsed = -5000L
      val s = new PendingInputAnnouncements(3, 100000, 2, 1000, () => elapsed)
      val a = announcement(8)
      s.add(a, p) shouldBe true
      elapsed += 999
      s.add(a, q) shouldBe false
      s.size shouldBe 1
      elapsed += 1
      s.take(blocks.head.header) shouldBe empty
      s.byteSize shouldBe 0L
    }
  }

  property("with default caps, two saturated hosts cannot evict two honest hosts' roots") {
    withPeers { (p, _) =>
      val caps = settings.matrix.pendingAnnouncements
      val s = new PendingInputAnnouncements(caps.maxEntries, caps.maxBytes, caps.perPeer, 1000, () => 0L)
      def host(n: Int): ConnectedPeer = p.copy(connectionId = p.connectionId.copy(
        remoteAddress = new java.net.InetSocketAddress(s"10.0.0.$n", 9000)))
      val attackerCount = caps.maxEntries / caps.perPeer
      attackerCount shouldBe 2
      val attackers = (1 to attackerCount).map(host)
      var serial = 0
      attackers.foreach { peer =>
        (1 to caps.perPeer).foreach { _ =>
          serial += 1
          s.add(announcement(serial), peer) shouldBe true
        }
      }
      val honest = (1 to 10).map(n => announcement(100000 + n))
      honest.zipWithIndex.foreach { case (root, n) =>
        s.add(root, host(attackerCount + 1 + n / 5)) shouldBe true
      }
      // Continued attacker traffic must preserve both honest hosts' roots.
      (1 to 1024).foreach { _ =>
        attackers.foreach { peer =>
          serial += 1
          s.add(announcement(serial), peer)
        }
      }
      s.size shouldBe caps.maxEntries
      s.evictions should be > 1L
      s.take(blocks.head.header).map(_._1.id) should contain allElementsOf honest.map(_.id)
    }
  }

  property("random add, clock, take and disconnect sequences never exceed the entry, byte or per-host caps") {
    implicit val system: ActorSystem = ActorSystem("pending-random-test")
    try {
      val peers = (1 to 4).map { n =>
        val connection = connectionIdGen.sample.get.copy(
          remoteAddress = new java.net.InetSocketAddress(s"10.0.0.$n", 9000))
        ConnectedPeer(connection, TestProbe().ref, None)
      }
      val operations = Gen.listOfN(80, for {
        op <- Gen.frequency(6 -> 0, 1 -> 1, 1 -> 2, 1 -> 3)
        host <- Gen.choose(0, 3)
        id <- Gen.choose(1, 12)
        payloadSize <- Gen.choose(0, 12)
      } yield (op, host, id, payloadSize))
      forAll(Gen.choose(1, 12), Gen.choose(1, 8), Gen.choose(1, 8), operations) {
        (entryCap, byteUnits, hostCap, steps) =>
          var now = 0L
          var operation = 0
          var held = Vector.empty[(InputBlockAnnouncement, ConnectedPeer, Long)]
          val byteCap = InputBlockAnnouncement.serializer.toBytes(announcement(1)).length.toLong * byteUnits
          val s = new PendingInputAnnouncements(entryCap, byteCap, hostCap, 1000, () => now)
          def bytes(a: InputBlockAnnouncement): Long =
            InputBlockAnnouncement.serializer.toBytes(a).length.toLong
          def occupancy: Map[String, Int] = held.groupBy(e =>
            PendingInputAnnouncements.peerHostKey(e._2)).map { case (host, es) => host -> es.size }
          s.onDiscard = { (a, peer) =>
            val arrived = held.find(e => e._1 eq a).get._3
            if (operation == 0 && now - arrived < 1000) {
              val counts = occupancy
              counts(PendingInputAnnouncements.peerHostKey(peer)) shouldBe counts.values.max
            }
            held = held.filterNot(e => e._1 eq a)
          }
          def drain(): Unit = {
            val before = s.byteSize
            val ready = s.take(blocks.head.header)
            ready.map(_._1.id) shouldBe held.map(_._1.id)
            ready.map(e => bytes(e._1)).sum shouldBe before
            held = Vector.empty
            s.size shouldBe 0
            s.byteSize shouldBe 0L
          }
          steps.foreach { case (op, host, id, payloadSize) =>
            operation = op
            op match {
              case 0 =>
                val a = announcement(id).copy(weakTxIds = Some(
                  Seq.fill(payloadSize)(Array.fill[Byte](6)(id.toByte))))
                if (s.add(a, peers(host))) held :+= ((a, peers(host), now))
              case 1 =>
                now += id * 100L
                s.expire()
              case 2 => drain()
              case 3 => s.removeConnection(peers(host).handlerRef)
            }
            s.size shouldBe held.size
            s.size should be <= entryCap
            s.byteSize should be <= byteCap
            s.byteSize shouldBe held.map(e => bytes(e._1)).sum
            all(occupancy.values.toSeq) should be <= hostCap
            val slots = held.map(e => PendingInputAnnouncements.peerHostKey(e._2) -> e._1.id)
            slots.distinct.size shouldBe slots.size
            val stats = s.fullInfo
            // Rejections never enter the store; only these drops remove admitted entries.
            stats.admitted shouldBe stats.size.toLong + stats.replayed + stats.evictions +
              Seq("expired", "staleParent", "disconnected").map(stats.drops).sum
          }
          drain()
      }
    } finally Await.result(system.terminate(), 10.seconds)
  }

  private def disposalScenario(evict: Boolean, checkClock: Boolean = false,
                               checkStats: Boolean = false, disconnect: Boolean = false): Unit = {
    implicit val system: ActorSystem = ActorSystem("pending-disposal-test")
    implicit val ec = system.dispatcher
    val cfg = settings.copy(directory = java.nio.file.Files.createTempDirectory(
      new java.io.File("target").toPath, "pending-r1-").toFile.getAbsolutePath,
      matrix = settings.matrix.copy(pendingAnnouncements =
        settings.matrix.pendingAnnouncements.copy(maxEntries = 1, perPeer = 1)))
    val history = ErgoHistory.readOrGenerate(cfg)(null)
    try {
      val nc = TestProbe()
      val peer = ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref, None)
      val other = ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref, None)
      val stats = TestActorRef(new org.ergoplatform.local.ErgoStatsCollector(
        TestProbe().ref, nc.ref, ErgoSyncTracker(cfg.scorexSettings.network), cfg))
      val tracker = DeliveryTracker.empty(cfg)
      val pool = ErgoMemPool.empty(cfg)
      val ref = TestActorRef(new ErgoNodeViewSynchronizer(nc.ref, TestProbe().ref,
        ErgoSyncInfoMessageSpec, cfg, ErgoSyncTracker(cfg.scorexSettings.network), tracker))
      if (checkClock) {
        val before = System.nanoTime() / 1000000L
        val observed = ref.underlyingActor.pendingAnnouncementsNow()
        val after = System.nanoTime() / 1000000L
        observed should be >= before
        observed should be <= after
      }
      ref ! ChangedHistory(history)
      ref ! ChangedMempool(pool)
      val first = announcement(101, history.fullBlockHeight + 2)
      val second = announcement(102, history.fullBlockHeight + 2)
      val target = if (evict || disconnect) first else second
      val typeId = org.ergoplatform.modifiers.InputBlockTypeId.value
      def inv(): Unit = {
        val data = InvData(typeId, Seq(target.id))
        ref ! org.ergoplatform.network.message.Message(
          org.ergoplatform.network.message.InvSpec,
          Left(org.ergoplatform.network.message.InvSpec.toBytes(data)), Some(peer))
      }
      inv()
      nc.fishForMessage(3.seconds) {
        case stn: SendToNetwork if stn.message.spec == RequestModifierSpec =>
          stn.message.data.get.asInstanceOf[InvData].ids.contains(target.id)
        case _ => false
      }
      val state = proxy(classOf[UtxoStateReader]) { (m, _) =>
        if (m.getName == "stateContext") emptyStateContext else throw new AssertionError(m.getName)
      }
      ref.underlyingActor.processInputBlock(first, history, pool, peer, Some(state))
      if (disconnect) {
        tracker.status(target.id, typeId, Seq.empty) shouldBe
          scorex.core.network.ModifiersStatus.Received
        ref ! org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.DisconnectedPeer(peer)
      } else {
        ref.underlyingActor.processInputBlock(second, history, pool,
          if (evict) other else peer, Some(state))
      }
      if (disconnect) {
        val probe = TestProbe()
        probe.send(stats, org.ergoplatform.local.ErgoStatsCollector.GetNodeInfo)
        val info = probe.expectMsgType[org.ergoplatform.local.ErgoStatsCollector.NodeInfo]
        val json = org.ergoplatform.local.ErgoStatsCollector.NodeInfo.jsonEncoder(info).hcursor
          .downField("pendingInputAnnouncements")
        json.get[Int]("size") shouldBe Right(0)
        json.downField("drops").get[Long]("disconnected") shouldBe Right(1L)
      }
      if (checkStats) {
        val probe = TestProbe()
        probe.send(stats, org.ergoplatform.local.ErgoStatsCollector.GetNodeInfo)
        val info = probe.expectMsgType[org.ergoplatform.local.ErgoStatsCollector.NodeInfo]
        val json = org.ergoplatform.local.ErgoStatsCollector.NodeInfo.jsonEncoder(info)
          .hcursor.downField("pendingInputAnnouncements")
        json.get[Int]("size") shouldBe Right(1)
        json.get[Long]("bytes").right.get should be > 0L
        json.get[Long]("evictions") shouldBe Right(if (evict) 1L else 0L)
        json.get[Long]("admitted") shouldBe Right(if (evict) 2L else 1L)
        json.get[Long]("replayed") shouldBe Right(0L)
        json.get[Long]("replayNotForwarded") shouldBe Right(0L)
        val drops = json.downField("drops")
        drops.get[Long]("hostLimit") shouldBe Right(if (evict) 0L else 1L)
        Seq("duplicate", "variantLimit", "oversize", "expired",
          "staleParent", "disconnected").foreach { reason =>
          drops.get[Long](reason) shouldBe Right(0L)
        }
      }
      tracker.status(target.id, typeId, Seq.empty) shouldBe
        scorex.core.network.ModifiersStatus.Unknown
      inv()
      nc.fishForMessage(3.seconds) {
        case stn: SendToNetwork if stn.message.spec == RequestModifierSpec =>
          stn.message.data.get.asInstanceOf[InvData].ids.contains(target.id)
        case _ => false
      }
      tracker.status(target.id, typeId, Seq.empty) shouldBe
        scorex.core.network.ModifiersStatus.Requested
    } finally {
      Await.result(system.terminate(), 10.seconds)
      history.closeStorage()
    }
  }

  property("node info exposes live pending size bytes evictions and drops") {
    disposalScenario(evict = false, checkStats = true)
    disposalScenario(evict = true, checkStats = true)
  }

  property("duplicates are counted without a warning; evictions warn at most once per second") {
    withPeers { (p, q) =>
      val logger = org.slf4j.LoggerFactory.getLogger(classOf[PendingInputAnnouncements])
        .asInstanceOf[ch.qos.logback.classic.Logger]
      val oldLevel = logger.getLevel
      val appender = new ch.qos.logback.core.read.ListAppender[ch.qos.logback.classic.spi.ILoggingEvent]
      appender.start()
      logger.addAppender(appender)
      logger.setLevel(ch.qos.logback.classic.Level.WARN)
      try {
        var now = 0L
        val s = new PendingInputAnnouncements(2, 100000, 10, 10000, () => now)
        val a = announcement(10)
        s.add(a, p) shouldBe true
        (1 to 10).foreach(_ => s.add(a, p) shouldBe false)
        appender.list.size() shouldBe 0
        s.drops("duplicate") shouldBe 10L
        // Capacity events share a warning budget; duplicates do not consume it.
        now = 1L
        s.add(announcement(11), q) shouldBe true
        s.add(announcement(12), p) shouldBe true
        appender.list.size() shouldBe 1
        s.evictions shouldBe 1L
        now = 1000L
        s.add(announcement(13), q) shouldBe true
        appender.list.size() shouldBe 1
        now = 1001L
        s.add(announcement(14), p) shouldBe true
        appender.list.size() shouldBe 2
        appender.list.get(1).getFormattedMessage should include ("eviction")
        appender.list.get(1).getLevel shouldBe ch.qos.logback.classic.Level.WARN
        s.evictions shouldBe 3L
      } finally {
        logger.detachAppender(appender)
        logger.setLevel(oldLevel)
        appender.stop()
      }
    }
  }

  property("a hostname-configured and an IP connection to the same address share one host quota") {
    withPeers { (p, q) =>
      val s = new PendingInputAnnouncements(3, 100000, 1, 1000, () => 0L)
      val configured = p.copy(connectionId = p.connectionId.copy(
        remoteAddress = new java.net.InetSocketAddress(
          java.net.InetAddress.getByAddress("configured-peer", Array[Byte](127, 0, 0, 1)), 9000)))
      val inbound = q.copy(connectionId = q.connectionId.copy(
        remoteAddress = new java.net.InetSocketAddress(
          java.net.InetAddress.getByAddress(Array[Byte](127, 0, 0, 1)), 9001)))
      s.add(announcement(30), configured) shouldBe true
      s.add(announcement(31), inbound) shouldBe false
      s.size shouldBe 1
    }
  }

  property("disconnect removes only the matching handler even when peer addresses are equal") {
    withPeers { (p, q) =>
      val s = new PendingInputAnnouncements(3, 100000, 3, 1000, () => 0L)
      val reconnected = p.copy(handlerRef = q.handlerRef)
      reconnected shouldBe p
      s.add(announcement(32), p) shouldBe true
      s.add(announcement(33), reconnected) shouldBe true
      s.removeConnection(p.handlerRef)
      s.take(blocks.head.header).map(_._1.id) shouldBe Seq(announcement(33).id)
    }
  }

  property("a disconnected sender's held announcements are dropped and their deliveries released") {
    disposalScenario(evict = false, disconnect = true)
  }

  property("synchronizer clock seam is backed by System.nanoTime") {
    disposalScenario(evict = false, checkClock = true)
  }

  property("full store drops delivery and later inventory requests it again") {
    disposalScenario(evict = false)
  }

  property("evicted accepted delivery becomes requestable again") {
    disposalScenario(evict = true)
  }

  private def replayScenario(epoch: Boolean, earlyBody: Boolean,
                             poisoned: Boolean = false, batchSize: Int = 1,
                             invalidOnly: Boolean = false, otherSupplier: Boolean = false, historyBeforeParent: Boolean = false,
                             knownBeforeApply: Boolean = false, reorg: Boolean = false): Unit = {
    implicit val system: ActorSystem = ActorSystem("pending-replay-test")
    implicit val ec = system.dispatcher
    val cfg = settings.copy(directory = java.nio.file.Files.createTempDirectory(
      new java.io.File("target").toPath, "pending-r1-").toFile.getAbsolutePath,
      matrix = settings.matrix.copy(pendingAnnouncements =
        settings.matrix.pendingAnnouncements.copy(
          perPeer = math.max(settings.matrix.pendingAnnouncements.perPeer, batchSize))))
    val realHistory = ErgoHistory.readOrGenerate(cfg)(null)
    try {
      val nc = TestProbe()
      val vh = TestProbe()
      val stats = TestActorRef(new org.ergoplatform.local.ErgoStatsCollector(
        TestProbe().ref, nc.ref, ErgoSyncTracker(cfg.scorexSettings.network), cfg))
      val peer = ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref, None)
      val pool = ErgoMemPool.empty(cfg)
      val height = if (epoch) cfg.chainSettings.voting.votingLength else 1
      var tip = blocks.head.header.copy(height = height - 1)
      val parent = blocks.head.header.copy(height = height,
        parentId = if (reorg) blocks.last.id else tip.id)
      var parentAvailable = false
      var difficultyReads = 0
      val expectedDiff = BigInt(100)
      val expectedBits = org.ergoplatform.mining.difficulty.DifficultySerializer
        .encodeCompactBits(expectedDiff)
      val nextParameters = new Parameters(height, parameters.parametersTable.updated(
        Parameters.SubblocksPerBlockIncrease, parameters.subBlocksPerBlock + 1),
        parameters.proposedUpdate)
      def state(h: Header, ps: Parameters): UtxoStateReader = {
        val ctx = new ErgoStateContext(Seq(h), None, emptyStateContext.genesisStateDigest,
          ps, emptyStateContext.validationSettings, emptyStateContext.votingData)(cfg.chainSettings)
        proxy(classOf[UtxoStateReader]) { (m, _) =>
          if (m.getName == "stateContext") ctx else throw new AssertionError(m.getName)
        }
      }
      val hr = proxy(classOf[ErgoHistory]) { (m, args) => m.getName match {
        case "isInBestChain" => Boolean.box(args.head == parent || args.head == parent.id)
        case "fullBlockHeight" => Int.box(tip.height)
        case "bestFullBlockIdOpt" => Some(tip.id)
        case "modifierById" | "typedModifierById" if args.head == parent.id =>
          if (parentAvailable) Some(parent) else None
        case "requiredDifficultyAfter" =>
          args.head shouldBe parent
          difficultyReads += 1
          expectedDiff
        case _ => m.invoke(realHistory, args: _*)
      }}
      var validations = Vector.empty[(Parameters, Option[Long])]
      var receiveTurns = Vector.empty[(String, Int)]
      val tracker = DeliveryTracker.empty(cfg)
      val ref = TestActorRef(new ErgoNodeViewSynchronizer(nc.ref, vh.ref,
        ErgoSyncInfoMessageSpec, cfg, ErgoSyncTracker(cfg.scorexSettings.network),
        tracker) {
        override def aroundReceive(receive: akka.actor.Actor.Receive, msg: Any): Unit = {
          val before = validations.size
          super.aroundReceive(receive, msg)
          receiveTurns :+= msg.getClass.getSimpleName -> (validations.size - before)
        }
      })
      ref ! ChangedHistory(hr)
      ref ! ChangedMempool(pool)
      ref ! ChangedState(state(tip, parameters))
      val a = new InputBlockAnnouncement(1,
        blocks(1).header.copy(height = height + 1, parentId = parent.id, nBits = expectedBits),
        InputBlockFields.empty, None) {
        override def valid(pow: AutolykosPowScheme, ps: Parameters, bits: Option[Long]): Boolean = {
          validations :+= ps -> bits
          !invalidOnly && (ps eq nextParameters) && bits.contains(expectedBits)
        }
      }
      val attacker = ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref, None)
      var garbageChecks = 0
      if (poisoned) {
        val fields = InputBlockFields.empty
        val garbageFields = new InputBlockFields(None,
          scorex.crypto.hash.Digest32 @@ Array.fill[Byte](32)(42),
          fields.prevTransactionsDigest, fields.inputBlockFieldsProof)
        val garbage = new InputBlockAnnouncement(1, a.header, garbageFields, None) {
          override def valid(pow: AutolykosPowScheme, ps: Parameters, bits: Option[Long]): Boolean = {
            garbageChecks += 1
            false
          }
        }
        ref.underlyingActor.processInputBlock(garbage, hr, pool, attacker,
          Some(state(tip, parameters)))
      }
      val typeId = org.ergoplatform.modifiers.InputBlockTypeId.value
      def requestInventory(): Unit = {
        val data = InvData(typeId, Seq(a.id))
        ref ! org.ergoplatform.network.message.Message(
          org.ergoplatform.network.message.InvSpec,
          Left(org.ergoplatform.network.message.InvSpec.toBytes(data)), Some(peer))
        nc.fishForMessage(3.seconds) {
          case stn: SendToNetwork if stn.message.spec == RequestModifierSpec =>
            stn.message.data.get.asInstanceOf[InvData].ids.contains(a.id)
          case _ => false
        }
      }
      if (invalidOnly) requestInventory()
      ref.underlyingActor.processInputBlock(a, hr, pool, peer, Some(state(tip, parameters)))
      if (invalidOnly) {
        tracker.status(a.id, typeId, Seq.empty) shouldBe
          scorex.core.network.ModifiersStatus.Received
        if (otherSupplier) {
          tracker.setUnknown(a.id, typeId)
          tracker.setReceivedDirectly(a.id, typeId, attacker)
        }
      }
      (1 until batchSize).foreach { n =>
        val extra = new InputBlockAnnouncement(1, a.header.copy(timestamp = n.toLong),
          InputBlockFields.empty, None) {
          override def valid(pow: AutolykosPowScheme, ps: Parameters, bits: Option[Long]): Boolean = {
            validations :+= ps -> bits
            !invalidOnly && (ps eq nextParameters) && bits.contains(expectedBits)
          }
        }
        ref.underlyingActor.processInputBlock(extra, hr, pool, peer, Some(state(tip, parameters)))
      }
      realHistory.getInputBlock(a.id) shouldBe None
      validations shouldBe empty
      nc.fishForMessage(3.seconds) {
        case stn: SendToNetwork if stn.message.spec == RequestModifierSpec =>
          stn.message.data.get.asInstanceOf[InvData].ids.contains(parent.id)
        case _ => false
      }
      if (earlyBody) {
        realHistory.applyInputBlockTransactions(a.id, Seq.empty, null) shouldBe
          (Seq.empty -> Seq.empty)
        realHistory.getInputBlockTransactions(a.id) shouldBe Some(Seq.empty)
      }
      if (historyBeforeParent) ref ! ChangedHistory(hr)
      if (knownBeforeApply) {
        parentAvailable = true
        ref ! ChangedHistory(hr)
        validations shouldBe empty
      }
      tip = parent
      parentAvailable = true
      ref ! RemoteBlockApplied(parent, Seq.empty)
      ref ! ChangedHistory(hr)
      validations shouldBe empty // history alone must not use pre-epoch parameters
      ref ! ChangedState(state(parent, nextParameters))
      val statsProbe = TestProbe()
      statsProbe.send(stats, org.ergoplatform.local.ErgoStatsCollector.GetNodeInfo)
      val info = statsProbe.expectMsgType[org.ergoplatform.local.ErgoStatsCollector.NodeInfo]
      val json = org.ergoplatform.local.ErgoStatsCollector.NodeInfo.jsonEncoder(info).hcursor
        .downField("pendingInputAnnouncements")
      json.get[Long]("replayed") shouldBe Right((batchSize + (if (poisoned) 1 else 0)).toLong)
      json.get[Long]("replayNotForwarded") shouldBe Right(if (invalidOnly || poisoned) 1L else 0L)
      if (invalidOnly) {
        validations shouldBe Vector(nextParameters -> Some(expectedBits))
        if (otherSupplier) {
          tracker.status(a.id, typeId, Seq.empty) shouldBe
            scorex.core.network.ModifiersStatus.Received
          tracker.getSource(a.id, typeId) shouldBe Some(attacker)
        } else {
          tracker.status(a.id, typeId, Seq.empty) shouldBe
            scorex.core.network.ModifiersStatus.Unknown
          requestInventory()
          tracker.status(a.id, typeId, Seq.empty) shouldBe
            scorex.core.network.ModifiersStatus.Requested
        }
        return
      }
      val processed = vh.fishForMessage(3.seconds) {
        case ProcessInputBlock(info, _) => info.id == a.id
        case _ => false
      }.asInstanceOf[ProcessInputBlock]
      validations shouldBe Vector.fill(batchSize)(nextParameters -> Some(expectedBits))
      if (batchSize > 1) {
        val work = receiveTurns.filter(_._2 > 0)
        work.head._2 should be <= 64
        all(work.map(_._2)) should be <= 64
        work.tail.map(_._1).distinct shouldBe
          (if (batchSize > 64) Vector("ReplayPendingInputAnnouncements$") else Vector.empty)
        work.map(_._2).sum shouldBe batchSize
      }
      difficultyReads shouldBe (batchSize + (if (poisoned) 1 else 0))
      if (poisoned) {
        garbageChecks shouldBe 1
        nc.fishForMessage(3.seconds) {
          case p: scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer =>
            p shouldBe scorex.core.network.NetworkController.ReceivableMessages.PenalizePeer(
              attacker.connectionId.remoteAddress,
              org.ergoplatform.network.peer.PenaltyType.MisbehaviorPenalty)
            true
          case _ => false
        }
      }
      if (earlyBody) {
        // Exercise the existing NodeViewHolder handler, which resumes cached bodies.
        var resumed = false
        val bodyHistory = proxy(classOf[ErgoHistory]) { (m, args) =>
          if (m.getName == "applyInputBlockTransactions") {
            realHistory.getInputBlock(a.id).isDefined shouldBe true
            args(1) shouldBe Seq.empty
            resumed = true
            Seq.empty[ModifierId] -> Seq.empty[ModifierId]
          } else m.invoke(realHistory, args: _*)
        }
        val holder = TestActorRef(new org.ergoplatform.nodeView.ErgoNodeViewHolder[
          org.ergoplatform.nodeView.state.UtxoState](cfg) {
          override protected def history(): ErgoHistory = bodyHistory
        })
        holder ! processed
        resumed shouldBe true
      }
      ref ! ChangedState(state(parent, nextParameters))
      validations.size shouldBe batchSize
    } finally {
      Await.result(system.terminate(), 10.seconds)
      realHistory.closeStorage()
    }
  }

  property("+2 root survives history changes before its parent header arrives") {
    replayScenario(epoch = false, earlyBody = false, historyBeforeParent = true)
  }

  property("a known parent header waits for its block to apply before replay") {
    replayScenario(epoch = false, earlyBody = false, knownBeforeApply = true)
  }

  property("a winning parent header survives history changes before the reorg applies") {
    replayScenario(epoch = false, earlyBody = false, knownBeforeApply = true, reorg = true)
  }

  property("replay counts replayed and not-forwarded announcements") {
    replayScenario(epoch = false, earlyBody = false, batchSize = 3)
    replayScenario(epoch = false, earlyBody = false, invalidOnly = true)
  }

  property("failed replay releases the requested announcement for a later inventory") {
    replayScenario(epoch = false, earlyBody = false, invalidOnly = true)
  }

  property("failed replay preserves a received copy from another supplier") {
    replayScenario(epoch = false, earlyBody = false, invalidOnly = true, otherSupplier = true)
  }

  property("256 same-parent announcements replay in bounded actor receives") {
    replayScenario(epoch = false, earlyBody = false, batchSize = 256)
  }

  property("garbage fields sharing a header cannot suppress honest replay or steal attribution") {
    replayScenario(epoch = false, earlyBody = false, poisoned = true)
  }

  property("a host's second variant of a header is refused and counted") {
    withPeers { (p, _) =>
      val s = new PendingInputAnnouncements(3, 100000, 2, 1000, () => 0L)
      val a = announcement(7)
      s.add(a, p) shouldBe true
      s.add(a.copy(weakTxIds = Some(Seq.empty)), p) shouldBe false
      s.size shouldBe 1
      s.drops("variantLimit") shouldBe 1L
    }
  }

  property("an identical same-host re-send is a duplicate even at the host limit") {
    withPeers { (p, _) =>
      val s = new PendingInputAnnouncements(3, 100000, 1, 1000, () => 0L)
      val a = announcement(7)
      s.add(a, p) shouldBe true
      s.add(a, p) shouldBe false
      s.size shouldBe 1
      s.drops("duplicate") shouldBe 1L
      s.drops("hostLimit") shouldBe 0L
      s.drops("variantLimit") shouldBe 0L
    }
  }

  property("another host's variant of the same header is held separately") {
    withPeers { (p, q) =>
      val s = new PendingInputAnnouncements(3, 100000, 2, 1000, () => 0L)
      val a = announcement(7)
      s.add(a, p) shouldBe true
      s.add(a.copy(weakTxIds = Some(Seq.empty)), q) shouldBe true
      s.size shouldBe 2
    }
  }

  property("+2 holds and downloads parent, then uses the normal +1 validation path") {
    replayScenario(epoch = false, earlyBody = false)
  }
  property("replay waits for epoch-boundary parameters and derives difficulty from parent") {
    replayScenario(epoch = true, earlyBody = false)
  }
  property("early body skipped for unknown announcement resumes after validated replay") {
    replayScenario(epoch = false, earlyBody = true)
  }
}
