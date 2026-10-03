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
import scorex.core.network.{ConnectedPeer, DeliveryTracker, ModifiersStatus}
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

class PendingInputAnnouncementsOffSwitchSpecification extends ErgoCorePropertyTest {
  import org.ergoplatform.utils.ErgoCoreTestConstants.{emptyStateContext, parameters}
  import org.ergoplatform.utils.ErgoNodeTestConstants.settings
  import org.ergoplatform.utils.generators.ChainGenerator.genChain
  import org.ergoplatform.utils.generators.ConnectedPeerGenerators.connectionIdGen

  private val blocks = genChain(3)

  private def proxy[T](cls: Class[T])(f: (Method, Array[AnyRef]) => Any): T =
    cls.cast(Proxy.newProxyInstance(cls.getClassLoader, Array(cls), new InvocationHandler {
      override def invoke(p: Any, m: Method, a: Array[AnyRef]): AnyRef =
        f(m, Option(a).getOrElse(Array.empty[AnyRef])).asInstanceOf[AnyRef]
    }))

  private class SynchronizerFixture(enabled: Boolean)(implicit val system: ActorSystem) {
    implicit val ec = system.dispatcher
    val cfg = settings.copy(directory = java.nio.file.Files.createTempDirectory(
      new java.io.File("target").toPath, "pending-off-switch-").toFile.getAbsolutePath,
      matrix = settings.matrix.copy(pendingAnnouncements =
        settings.matrix.pendingAnnouncements.copy(enabled = enabled)))
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
    val expectedDiff = BigInt(100)
    val expectedBits = org.ergoplatform.mining.difficulty.DifficultySerializer
      .encodeCompactBits(expectedDiff)
    val announcement = new InputBlockAnnouncement(1,
      blocks(2).header.copy(nBits = expectedBits), InputBlockFields.empty, None) {
      override def valid(pow: AutolykosPowScheme, ps: Parameters,
                         bits: Option[Long]): Boolean = {
        validations += 1
        bits shouldBe Some(expectedBits)
        true
      }
    }
    def state(h: Header): UtxoStateReader = {
      val ctx = new ErgoStateContext(Seq(h), None, emptyStateContext.genesisStateDigest,
        parameters, emptyStateContext.validationSettings, emptyStateContext.votingData)(
        cfg.chainSettings)
      proxy(classOf[UtxoStateReader]) { (m, _) =>
        if (m.getName == "stateContext") ctx else throw new AssertionError(m.getName)
      }
    }
    val hr = proxy(classOf[ErgoHistory]) { (m, args) => m.getName match {
      case "fullBlockHeight" => Int.box(tip.height)
      case "bestFullBlockIdOpt" => Some(tip.id)
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

  private def withSynchronizer(enabled: Boolean)(f: SynchronizerFixture => Unit): Unit = {
    implicit val system: ActorSystem = ActorSystem("pending-off-switch-test")
    val fixture = new SynchronizerFixture(enabled)
    try f(fixture)
    finally {
      Await.result(system.terminate(), 10.seconds)
      fixture.realHistory.closeStorage()
    }
  }

  property("disabled +2 announcements drop and request the ordering header as stock does") {
    withSynchronizer(enabled = false) { f =>
      f.info.get[Boolean]("enabled") shouldBe Right(false)
      f.announce()
      f.tracker.status(f.announcement.id, InputBlockTypeId.value, Seq.empty) shouldBe
        ModifiersStatus.Requested
      f.applyParent()
      f.validations shouldBe 0
      f.vh.expectNoMessage(100.millis)
      val json = f.info
      json.get[Boolean]("enabled") shouldBe Right(false)
      Seq("size", "bytes", "admitted", "replayed", "replayNotForwarded", "evictions")
        .foreach(key => json.get[Long](key) shouldBe Right(0L))
      json.downField("drops").as[Map[String, Long]].right.get.values.toSeq
        .foreach(_ shouldBe 0L)
    }
  }

  property("enabled +2 announcements keep the existing admission and replay behavior") {
    withSynchronizer(enabled = true) { f =>
      f.announce()
      f.info.get[Boolean]("enabled") shouldBe Right(true)
      f.info.get[Int]("size") shouldBe Right(1)
      f.tracker.status(f.announcement.id, InputBlockTypeId.value, Seq.empty) shouldBe
        ModifiersStatus.Received
      f.applyParent()
      f.validations shouldBe 1
      f.vh.expectMsg(ProcessInputBlock(f.announcement, f.peer))
      f.info.get[Int]("size") shouldBe Right(0)
      f.info.get[Long]("admitted") shouldBe Right(1L)
      f.info.get[Long]("replayed") shouldBe Right(1L)
    }
  }
}
