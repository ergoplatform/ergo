package org.ergoplatform.nodeView.wallet

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.ErgoBox
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages.{GetWalletStatus, ScanBoxesFromUtxoSnapshot, WalletStatus}
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.boxes.ReplaceCompactCollectBoxSelector
import org.ergoplatform.wallet.utils.FileUtils
import scorex.util.{ModifierId, bytesToId}

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Try}

class UtxoSnapshotChunkFailureSpec extends ErgoCorePropertyTest with FileUtils {
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  private implicit val timeout: Timeout = Timeout(10.seconds)

  /** Wallet service that fails `scanSnapshotChunk` for one subtree id and records that it was asked to. */
  private class FailingChunkService(s: ErgoSettings, failId: Option[ModifierId]) extends ErgoWalletServiceImpl(s) {
    val failedCalled = new AtomicBoolean(false)

    override def scanSnapshotChunk(state: ErgoWalletState,
                                   boxes: Array[ErgoBox],
                                   subtreeId: ModifierId,
                                   dustLimit: Option[Long],
                                   fullHeight: Int): Try[ErgoWalletState] =
      if (failId.contains(subtreeId)) {
        failedCalled.set(true)
        Failure(new Exception("injected chunk failure"))
      } else {
        super.scanSnapshotChunk(state, boxes, subtreeId, dustLimit, fullHeight)
      }
  }

  private def walletActor(service: ErgoWalletService)(implicit system: ActorSystem): ActorRef = {
    val s = settings.copy(directory = createTempDir.getAbsolutePath)
    val selector = new ReplaceCompactCollectBoxSelector(s.walletSettings.maxInputs, s.walletSettings.optimalInputs, None)
    system.actorOf(Props(new ErgoWalletActor(s, parameters, service, selector, null)))
  }

  private def subtreeId(i: Int): ModifierId = bytesToId(Array.fill(32)(i.toByte))

  property("a batch whose final UTXO snapshot chunk fails is not acknowledged like a successful batch") {
    implicit val system: ActorSystem = ActorSystem("utxo-snapshot-chunk-failure")
    try {
      val snapshotHeight = 7
      val batch = (1 to 3).map(i => subtreeId(i) -> Array.empty[ErgoBox])
      val msg = ScanBoxesFromUtxoSnapshot(batch, current = 3, total = 3, fullHeight = snapshotHeight)

      val okService = new FailingChunkService(settings, None)
      val failService = new FailingChunkService(settings, Some(subtreeId(3)))
      val okWallet = walletActor(okService)
      val failWallet = walletActor(failService)

      val okReply = Await.result(okWallet ? msg, 10.seconds)
      val failReply = Await.result(failWallet ? msg, 10.seconds)

      val okStatus = Await.result((okWallet ? GetWalletStatus).mapTo[WalletStatus], 10.seconds)
      val failStatus = Await.result((failWallet ? GetWalletStatus).mapTo[WalletStatus], 10.seconds)
      info(s"okReply=$okReply okHeight=${okStatus.height}; failReply=$failReply failHeight=${failStatus.height} " +
        s"failError=${failStatus.error}")

      // reach control: the injected failure ran on the failing wallet's final chunk
      failService.failedCalled.get() shouldBe true
      okService.failedCalled.get() shouldBe false

      // the successful batch records the snapshot height; the failed final chunk leaves the wallet at height 0
      okStatus.height shouldBe snapshotHeight
      failStatus.height shouldBe 0

      // the caller (UtxoSetScanner) must be able to tell the two batches apart
      failReply should not equal okReply
      failReply shouldBe 2
      okReply shouldBe 3
    } finally {
      Await.result(system.terminate(), 30.seconds)
    }
  }
}
