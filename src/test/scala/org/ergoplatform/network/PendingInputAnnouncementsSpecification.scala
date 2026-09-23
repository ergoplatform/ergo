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

  private val blocks = genChain(3)
  private def announcement(n: Int, height: Int = 2): InputBlockAnnouncement =
    InputBlockAnnouncement(1, blocks(1).header.copy(height = height, timestamp = n.toLong),
      InputBlockFields.empty, None)

  // Reflection lets these behavioral tests run (and fail) against the unpatched base.
  private class Store(entries: Int, bytes: Long, perPeer: Int, clock: () => Long) {
    private val cls = Class.forName("org.ergoplatform.network.PendingInputAnnouncements")
    private val value = cls.getConstructors.head.newInstance(
      Int.box(entries), Long.box(bytes), Int.box(perPeer), Long.box(1000), clock)
    private def call(name: String, args: AnyRef*): Any =
      cls.getMethods.find(m => m.getName == name && m.getParameterCount == args.size).get
        .invoke(value, args: _*)
    def add(a: InputBlockAnnouncement, p: ConnectedPeer): Boolean =
      call("add", a, p).asInstanceOf[Boolean]
    def size: Int = call("size").asInstanceOf[Int]
    def byteSize: Long = call("byteSize").asInstanceOf[Long]
    def evictions: Long = call("evictions").asInstanceOf[Long]
    def drops: Long = call("drops").asInstanceOf[Long]
    def take(tip: Header): Seq[(InputBlockAnnouncement, ConnectedPeer)] =
      call("take", tip).asInstanceOf[Seq[(InputBlockAnnouncement, ConnectedPeer)]]
    def takeKnown(tip: Header, known: ModifierId => Boolean): Seq[(InputBlockAnnouncement, ConnectedPeer)] = {
      val method = cls.getMethods.find(m => m.getName == "take" && m.getParameterCount == 3)
      method.map(_.invoke(value, tip, Int.box(64), known)
        .asInstanceOf[Seq[(InputBlockAnnouncement, ConnectedPeer)]]).getOrElse(take(tip))
    }
  }

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

  property("(b) global entry and byte caps evict oldest and count evictions") {
    withPeers { (p, q) =>
      val a = announcement(1)
      val b = announcement(2)
      val c = announcement(3)
      val bytes = InputBlockAnnouncement.serializer.toBytes(a).length.toLong
      Seq(new Store(2, bytes * 10, 10, () => 0L),
        new Store(10, bytes * 2, 10, () => 0L)).foreach { s =>
        s.add(a, p) shouldBe true
        s.add(b, q) shouldBe true
        s.add(c, p) shouldBe true
        s.size shouldBe 2
        s.byteSize shouldBe bytes * 2
        s.evictions shouldBe 1
        s.take(blocks.head.header).map(_._1.id) shouldBe Seq(b.id, c.id)
        s.byteSize shouldBe 0
      }
      val tiny = new Store(2, bytes - 1, 2, () => 0L)
      tiny.add(a, p) shouldBe false
      tiny.size shouldBe 0
    }
  }

  property("(c) per-peer admission cannot evict another peer's entries") {
    withPeers { (p, q) =>
      val s = new Store(3, 100000, 1, () => 0L)
      s.add(announcement(1), p) shouldBe true
      s.add(announcement(2), p) shouldBe false
      s.add(announcement(3), q) shouldBe true
      s.size shouldBe 2
      s.evictions shouldBe 0
    }
  }

  property("(d) identical serialized announcements deduplicate without refreshing expiry") {
    withPeers { (p, q) =>
      var now = 0L
      val s = new Store(3, 100000, 2, () => now)
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

  property("(e) I5 stale and reorged-away +1/+2 parents drop; known best-chain +2 stays") {
    withPeers { (p, _) =>
      val s = new Store(10, 100000, 10, () => 0L)
      s.add(announcement(1, 1), p)
      s.add(announcement(2).copy(header = announcement(2).header.copy(
        parentId = blocks.last.id)), p)
      val current = announcement(3, 3).copy(header = announcement(3, 3).header.copy(
        parentId = blocks(1).id))
      val forked = announcement(4, 3).copy(header = announcement(4, 3).header.copy(
        parentId = blocks.last.id))
      val unknown = announcement(5, 3)
      s.add(current, p)
      s.add(forked, p)
      s.add(unknown, p)
      s.takeKnown(blocks.head.header, _ == blocks(1).id) shouldBe empty
      s.size shouldBe 1
      s.take(blocks(1).header).map(_._1.id) shouldBe Seq(current.id)
      s.size shouldBe 0
      s.byteSize shouldBe 0
    }
  }

  property("M8 TTL uses elapsed monotonic milliseconds, including a negative clock origin") {
    withPeers { (p, q) =>
      var elapsed = -5000L
      val s = new Store(3, 100000, 2, () => elapsed)
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

  property("I3 saturated hosts cannot evict the newly admitted honest host") {
    withPeers { (p, _) =>
      val caps = settings.matrix.pendingAnnouncements
      val s = new Store(caps.maxEntries, caps.maxBytes, caps.perPeer, () => 0L)
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
      val honest = announcement(100000)
      s.add(honest, host(attackerCount + 1)) shouldBe true
      // Keep replenishing attacker hosts: FIFO eventually ejects the honest singleton.
      (1 to 1024).foreach { _ =>
        attackers.foreach { peer =>
          serial += 1
          s.add(announcement(serial), peer)
        }
      }
      s.size shouldBe caps.maxEntries
      s.evictions should be > 1L
      s.take(blocks.head.header).map(_._1.id) should contain (honest.id)
    }
  }

  private def disposalScenario(evict: Boolean, checkClock: Boolean = false,
                               checkStats: Boolean = false): Unit = {
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
        val clock = ref.underlyingActor.getClass.getMethods.find(
          _.getName == "pendingAnnouncementsNow").get
        val observed = clock.invoke(ref.underlyingActor).asInstanceOf[Long]
        val after = System.nanoTime() / 1000000L
        observed should be >= before
        observed should be <= after
      }
      ref ! ChangedHistory(history)
      ref ! ChangedMempool(pool)
      val first = announcement(101, history.fullBlockHeight + 2)
      val second = announcement(102, history.fullBlockHeight + 2)
      val target = if (evict) first else second
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
      ref.underlyingActor.processInputBlock(second, history, pool,
        if (evict) other else peer, Some(state))
      if (checkStats) {
        val probe = TestProbe()
        probe.send(stats, org.ergoplatform.local.ErgoStatsCollector.GetNodeInfo)
        val info = probe.expectMsgType[org.ergoplatform.local.ErgoStatsCollector.NodeInfo]
        val json = org.ergoplatform.local.ErgoStatsCollector.NodeInfo.jsonEncoder(info)
          .hcursor.downField("pendingInputAnnouncements")
        json.get[Int]("size") shouldBe Right(1)
        json.get[Long]("bytes").right.get should be > 0L
        json.get[Long]("evictions") shouldBe Right(if (evict) 1L else 0L)
        json.get[Long]("drops") shouldBe Right(if (evict) 0L else 1L)
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

  property("M11 node info exposes live pending size bytes evictions and drops") {
    disposalScenario(evict = false, checkStats = true)
    disposalScenario(evict = true, checkStats = true)
  }

  property("M11 drop and eviction warnings are rate limited without losing counters") {
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
        val s = new Store(2, 100000, 10, () => now)
        val a = announcement(10)
        s.add(a, p) shouldBe true
        (1 to 10).foreach(_ => s.add(a, p) shouldBe false)
        appender.list.size() shouldBe 1
        s.drops shouldBe 10L
        appender.list.get(0).getFormattedMessage should include ("duplicate")
        // Remaining entries arrive at 1 ms, so they have not expired at 1000 ms.
        now = 1L
        s.add(announcement(11), q) shouldBe true
        s.add(announcement(12), p) shouldBe true
        appender.list.size() shouldBe 1
        s.evictions shouldBe 1L
        now = 1000L
        s.add(announcement(13), q) shouldBe true
        appender.list.size() shouldBe 2
        appender.list.get(1).getFormattedMessage should include ("eviction")
        appender.list.get(1).getLevel shouldBe ch.qos.logback.classic.Level.WARN
        s.evictions shouldBe 2L
      } finally {
        logger.detachAppender(appender)
        logger.setLevel(oldLevel)
        appender.stop()
      }
    }
  }

  property("M8 synchronizer clock seam is backed by System.nanoTime") {
    disposalScenario(evict = false, checkClock = true)
  }

  property("C1 full store drops delivery and later inventory requests it again") {
    disposalScenario(evict = false)
  }

  property("C1 evicted accepted delivery becomes requestable again") {
    disposalScenario(evict = true)
  }

  private def replayScenario(epoch: Boolean, earlyBody: Boolean,
                             poisoned: Boolean = false, batchSize: Int = 1): Unit = {
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
      val peer = ConnectedPeer(connectionIdGen.sample.get, TestProbe().ref, None)
      val pool = ErgoMemPool.empty(cfg)
      val height = if (epoch) cfg.chainSettings.voting.votingLength else 1
      val parent = blocks.head.header.copy(height = height)
      var tip = parent.copy(height = height - 1)
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
        case "fullBlockHeight" => Int.box(tip.height)
        case "bestFullBlockIdOpt" => Some(tip.id)
        case "modifierById" if args.head == parent.id =>
          if (parentAvailable) Some(parent) else None
        case "requiredDifficultyAfter" =>
          args.head shouldBe parent
          difficultyReads += 1
          expectedDiff
        case _ => m.invoke(realHistory, args: _*)
      }}
      var validations = Vector.empty[(Parameters, Option[Long])]
      var receiveTurns = Vector.empty[(String, Int)]
      val ref = TestActorRef(new ErgoNodeViewSynchronizer(nc.ref, vh.ref,
        ErgoSyncInfoMessageSpec, cfg, ErgoSyncTracker(cfg.scorexSettings.network),
        DeliveryTracker.empty(cfg)) {
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
          (ps eq nextParameters) && bits.contains(expectedBits)
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
        system.stop(attacker.handlerRef)
        ref ! org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.DisconnectedPeer(attacker)
      }
      ref.underlyingActor.processInputBlock(a, hr, pool, peer, Some(state(tip, parameters)))
      (1 until batchSize).foreach { n =>
        val extra = new InputBlockAnnouncement(1, a.header.copy(timestamp = n.toLong),
          InputBlockFields.empty, None) {
          override def valid(pow: AutolykosPowScheme, ps: Parameters, bits: Option[Long]): Boolean = {
            validations :+= ps -> bits
            (ps eq nextParameters) && bits.contains(expectedBits)
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
      tip = parent
      parentAvailable = true
      ref ! RemoteBlockApplied(parent, Seq.empty)
      ref ! ChangedHistory(hr)
      validations shouldBe empty // history alone must not use pre-epoch parameters
      ref ! ChangedState(state(parent, nextParameters))
      val processed = vh.fishForMessage(3.seconds) {
        case ProcessInputBlock(info, _) => info.id == a.id
        case _ => false
      }.asInstanceOf[ProcessInputBlock]
      validations shouldBe Vector.fill(batchSize)(nextParameters -> Some(expectedBits))
      if (batchSize > 1) {
        val work = receiveTurns.filter(_._2 > 0)
        work.head._2 should be <= 64
        all(work.map(_._2)) should be <= 64
        work.tail.map(_._1).distinct shouldBe Vector("ReplayPendingInputAnnouncements$")
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

  property("I4 256 same-parent announcements replay in bounded actor receives") {
    replayScenario(epoch = false, earlyBody = false, batchSize = 256)
  }

  property("I2 garbage fields sharing a header cannot suppress honest replay or steal attribution") {
    replayScenario(epoch = false, earlyBody = false, poisoned = true)
  }

  property("I2 weak transaction ids are included in serialized deduplication") {
    withPeers { (p, q) =>
      val s = new Store(3, 100000, 2, () => 0L)
      val a = announcement(7)
      s.add(a, p) shouldBe true
      s.add(a.copy(weakTxIds = Some(Seq.empty)), q) shouldBe true
      s.size shouldBe 2
    }
  }

  property("(a) +2 holds and downloads parent, then uses the normal +1 validation path") {
    replayScenario(epoch = false, earlyBody = false)
  }
  property("(a) replay waits for epoch-boundary parameters and derives difficulty from parent") {
    replayScenario(epoch = true, earlyBody = false)
  }
  property("(f) early body skipped for unknown announcement resumes after validated replay") {
    replayScenario(epoch = false, earlyBody = true)
  }
}
