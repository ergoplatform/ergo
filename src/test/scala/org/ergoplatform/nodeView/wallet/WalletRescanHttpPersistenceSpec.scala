package org.ergoplatform.nodeView.wallet

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.testkit.TestProbe
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.http.api.WalletApiRoute
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.ChangedState
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistoryReader._
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages.{CloseWallet, ScanOnChain, WalletRescanState}
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId

import java.io.File
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.concurrent.duration._
import scala.util.Try

class WalletRescanHttpPersistenceSpec
  extends ErgoCorePropertyTest with WalletTestOps with ScalatestRouteTest
    with FailFastCirceSupport with Eventually {

  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("HTTP wallet rescan completes through the actor and persists its checkpoint") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val view = getCurrentView
      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "http-rescan-persistence").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val allowBodyProbe = new AtomicBoolean(true)
      val observeReplay = new AtomicBoolean(false)
      val replayScans = new AtomicInteger(0)
      val service = new ErgoWalletServiceImpl(actorSettings) {
        override def scanBlockUpdate(state: ErgoWalletState,
                                     block: ErgoFullBlock,
                                     dustLimit: Option[Long]): Try[ErgoWalletState] = {
          val updated = super.scanBlockUpdate(state, block, dustLimit)
          if (observeReplay.get() && block.id == first.id && updated.isSuccess) replayScans.incrementAndGet()
          updated
        }
      }
      val actor = system.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, service, selector, view.history
      ) {
        override protected def probeSelectedFullChainBodies(
          targetId: ModifierId,
          targetHeight: Int,
          cursor: Option[FullChainCursor]
        ): FullChainProbe =
          if (allowBodyProbe.get()) super.probeSelectedFullChainBodies(targetId, targetHeight, cursor)
          else FullChainUnknown
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val readers = Readers(view.history, view.state, view.pool, reader)
      val readersHolder = system.actorOf(Props(new Actor {
        override def receive: Receive = {
          case GetReaders => sender() ! readers
        }
      }))
      val route = Route.seal(WalletApiRoute(readersHolder, w.nodeViewHolderRef, actorSettings).route)
      val probe = TestProbe()(system)
      probe.watch(actor)

      try {
        probe.send(actor, ChangedState(view.state))
        probe.send(actor, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val walletStatus = await(reader.getWalletStatus)
          walletStatus.height shouldBe first.height
          walletStatus.rescanState shouldBe WalletRescanState.Inactive
          walletStatus.error shouldBe None
        }

        allowBodyProbe.set(false)
        Post("/wallet/rescan", Json.obj("fromHeight" -> Json.fromInt(0))) ~> route ~> check {
          status shouldBe StatusCodes.Accepted
        }
        Get("/wallet/status") ~> route ~> check {
          status shouldBe StatusCodes.OK
          val body = responseAs[Json]
          body.hcursor.downField("walletHeight").as[Int] shouldBe Right(first.height)
          body.hcursor.downField("rescanState").as[String] shouldBe Right("in_progress")
        }
        observeReplay.set(true)
        allowBodyProbe.set(true)
        eventually(timeout(10.seconds), interval(100.millis)) {
          Get("/wallet/status") ~> route ~> check {
            status shouldBe StatusCodes.OK
            val body = responseAs[Json]
            body.hcursor.downField("walletHeight").as[Int] shouldBe Right(first.height)
            body.hcursor.downField("rescanState").as[String] shouldBe Right("inactive")
            body.hcursor.downField("error").as[String] shouldBe Right("")
          }
        }
        replayScans.get() shouldBe 1
      } finally {
        allowBodyProbe.set(true)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
        system.stop(readersHolder)
      }

      val reopened = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        reopened.registry.committedVersionAndDigest.get._1 shouldBe first.id
        reopened.registry.fetchDigest().height shouldBe first.height
        reopened.storage.rescanRecoveryIntent.get shouldBe false
        reopened.storage.deepForkQuarantine.get shouldBe false
      } finally {
        reopened.registry.close()
        reopened.storage.close()
      }
    }
  }
}
