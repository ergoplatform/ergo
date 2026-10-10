package org.ergoplatform.nodeView.wallet

import akka.actor.Props
import akka.testkit.TestProbe
import org.ergoplatform.core.idToVersion
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{ChangedHistory, ChangedState}
import org.ergoplatform.nodeView.history.ErgoHistoryReader._
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages._
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.scalatest.concurrent.Eventually
import scorex.util.ModifierId

import java.io.File
import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.duration._
import scala.util.Success

class WalletRescanRollbackCompositionSpec
  extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters

  property("a rollback during an explicit rescan proof releases the pending rescan") {
    withFixture { implicit w =>
      val first = makeGenesisBlock(getPublicKeys.head.pubkey)
      applyBlock(first) shouldBe 'success
      val history = getHistory
      history.bestFullBlockAt(first.height).map(_.id) shouldBe Some(first.id)

      val actorSettings = w.settings.copy(
        directory = new File(w.nodeViewDir, "rollback-during-rescan-proof").getAbsolutePath,
        nodeSettings = w.settings.nodeSettings.copy(keepVersions = 10, blocksToKeep = -1)
      )
      val ws = actorSettings.walletSettings
      val selector = new ReplaceCompactCollectBoxSelector(ws.maxInputs, ws.optimalInputs, None)
      val allowBodyProof = new AtomicBoolean(true)
      val bodyProbeEntered = TestProbe()(w.actorSystem)
      val actor = w.actorSystem.actorOf(Props(new ErgoWalletActor(
        actorSettings, parameters, new ErgoWalletServiceImpl(actorSettings), selector, history
      ) {
        override protected def probeSelectedFullChainBodies(targetId: ModifierId,
                                                            targetHeight: Int,
                                                            cursor: Option[FullChainCursor]): FullChainProbe = {
          if (!allowBodyProof.get()) {
            bodyProbeEntered.ref ! targetId
            FullChainUnknown
          } else super.probeSelectedFullChainBodies(targetId, targetHeight, cursor)
        }
      }))
      val reader = new ErgoWalletReader { override val walletActor = actor }
      val probe = TestProbe()(w.actorSystem)
      probe.watch(actor)

      try {
        probe.send(actor, ChangedState(getCurrentState))
        probe.send(actor, ScanOnChain(first))
        eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          status.height shouldBe first.height
          status.error shouldBe None
          status.rescanState shouldBe WalletRescanState.Inactive
        }

        allowBodyProof.set(false)
        probe.send(actor, RescanWallet(1))
        probe.expectMsg(5.seconds, Success(()))
        bodyProbeEntered.expectMsg(5.seconds, org.ergoplatform.modifiers.history.header.PreGenesisHeader.id)
        val inProgress = await(reader.getWalletStatus)
        inProgress.rescanState shouldBe WalletRescanState.InProgress

        // The first body probe returned Unknown, so this rollback is processed
        // by provingFullChain after the durable rescan intent was written.
        val version = idToVersion(first.id)
        probe.send(actor, Rollback(version))
        // Same-sender ordering makes this response a receipt for the Rollback
        // while the body proof remains blocked.
        probe.send(actor, GetWalletStatus)
        probe.expectMsgType[WalletStatus](5.seconds)
        allowBodyProof.set(true)
        probe.send(actor, ChangedHistory(history))

        val terminal = eventually(timeout(10.seconds), interval(100.millis)) {
          val status = await(reader.getWalletStatus)
          assert(status.rescanState != WalletRescanState.InProgress)
          status
        }
        if (terminal.rescanState == WalletRescanState.NeedsRecovery) {
          probe.send(actor, RescanWallet(1))
          probe.expectMsg(5.seconds, Success(()))
          eventually(timeout(10.seconds), interval(100.millis)) {
            val status = await(reader.getWalletStatus)
            status.rescanState shouldBe WalletRescanState.Inactive
            status.error shouldBe None
          }
        } else {
          terminal.rescanState shouldBe WalletRescanState.Inactive
          terminal.error shouldBe None
        }
      } finally {
        allowBodyProof.set(true)
        probe.send(actor, CloseWallet)
        probe.expectTerminated(actor, 5.seconds)
      }

      val reopened = ErgoWalletState.initial(actorSettings, parameters).get
      try {
        reopened.registry.committedVersionAndDigest.get._1 shouldBe first.id
        reopened.storage.rescanRecoveryIntent.get shouldBe false
      } finally {
        reopened.registry.close()
        reopened.storage.close()
      }
    }
  }
}
