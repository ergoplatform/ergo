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
  import org.ergoplatform.wallet.utils.FileUtils

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
    def take(tip: Header): Seq[(InputBlockAnnouncement, ConnectedPeer)] =
      call("take", tip).asInstanceOf[Seq[(InputBlockAnnouncement, ConnectedPeer)]]
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

  property("(d) announcement id deduplicates across peers without refreshing expiry") {
    withPeers { (p, q) =>
      var now = 0L
      val s = new Store(3, 100000, 2, () => now)
      val a = announcement(1)
      s.add(a, p) shouldBe true
      now = 900
      s.add(a.copy(weakTxIds = Some(Seq.empty)), q) shouldBe false
      s.size shouldBe 1
      now = 1001
      s.take(blocks.head.header) shouldBe empty
      s.size shouldBe 0
      s.byteSize shouldBe 0
    }
  }

  property("(e) behind-tip and reorged-away parents are removed, future parents stay") {
    withPeers { (p, _) =>
      val s = new Store(10, 100000, 10, () => 0L)
      s.add(announcement(1, 1), p)
      s.add(announcement(2).copy(header = announcement(2).header.copy(
        parentId = blocks.last.id)), p)
      s.add(announcement(3, 3), p)
      s.take(blocks.head.header) shouldBe empty
      s.size shouldBe 1
      s.take(blocks.last.header) shouldBe empty
      s.size shouldBe 0
      s.byteSize shouldBe 0
    }
  }

  private def replayScenario(epoch: Boolean, earlyBody: Boolean): Unit = {
    implicit val system: ActorSystem = ActorSystem("pending-replay-test")
    implicit val ec = system.dispatcher
    val files = new FileUtils {}
    val cfg = settings.copy(directory = files.createTempDir.getAbsolutePath)
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
      val ref = TestActorRef(new ErgoNodeViewSynchronizer(nc.ref, vh.ref,
        ErgoSyncInfoMessageSpec, cfg, ErgoSyncTracker(cfg.scorexSettings.network),
        DeliveryTracker.empty(cfg)))
      ref ! ChangedHistory(hr)
      ref ! ChangedMempool(pool)
      ref ! ChangedState(state(tip, parameters))
      var validations = Vector.empty[(Parameters, Option[Long])]
      val a = new InputBlockAnnouncement(1,
        blocks(1).header.copy(height = height + 1, parentId = parent.id, nBits = expectedBits),
        InputBlockFields.empty, None) {
        override def valid(pow: AutolykosPowScheme, ps: Parameters, bits: Option[Long]): Boolean = {
          validations :+= ps -> bits
          (ps eq nextParameters) && bits.contains(expectedBits)
        }
      }
      ref.underlyingActor.processInputBlock(a, hr, pool, peer, Some(state(tip, parameters)))
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
      validations shouldBe Vector(nextParameters -> Some(expectedBits))
      difficultyReads shouldBe 1
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
      validations.size shouldBe 1
    } finally {
      Await.result(system.terminate(), 10.seconds)
      realHistory.closeStorage()
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
