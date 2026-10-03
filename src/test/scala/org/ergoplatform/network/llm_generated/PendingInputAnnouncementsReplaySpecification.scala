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

class PendingInputAnnouncementsReplaySpecification extends ErgoCorePropertyTest {
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
}
