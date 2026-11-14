package org.ergoplatform.nodeView.history

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.ErgoBox
import org.ergoplatform.mining.groupElemToBytes
import org.ergoplatform.nodeView.ErgoNodeViewHolder.CurrentView
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import org.ergoplatform.nodeView.history.UtxoSetScanner.StartUtxoSetScan
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages.ScanBoxesFromUtxoSnapshot
import org.ergoplatform.settings.{ErgoSettings, Parameters}
import org.ergoplatform.utils.fixtures.WalletFixture
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.wallet.utils.FileUtils
import org.scalatest.concurrent.Eventually

import scala.concurrent.duration._

class UtxoSetScannerFailureSpec extends ErgoCorePropertyTest with WalletTestOps with Eventually with FileUtils {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._

  /** Wallet whose snapshot scan fails once, at the first subtree holding a box that `failOn` matches. */
  private class FailOnceWallet(h: ErgoHistoryReader, s: ErgoSettings, p: Parameters, failOn: ErgoBox => Boolean)
                              (implicit system: ActorSystem) extends ErgoWallet(h, s, p) {
    @volatile var failed = false

    override def scanUtxoSnapshot(msg: ScanBoxesFromUtxoSnapshot): Int = {
      val k = msg.subtrees.indexWhere(_._2.exists(failOn))
      if (!failed && k >= 0) {
        failed = true
        val first = msg.current - msg.subtrees.size + 1
        // the chunks before the failing one are scanned; the reply is the last chunk scanned
        if (k == 0) first - 1 else super.scanUtxoSnapshot(msg.copy(subtrees = msg.subtrees.take(k), current = first + k - 1))
      } else {
        super.scanUtxoSnapshot(msg)
      }
    }
  }

  /** Node view proxy: hands the scanner `wallet` instead of the node's own wallet. */
  private class SwapWalletProxy(nodeView: ActorRef, wallet: ErgoWallet) extends Actor {
    override def receive: Receive = {
      case GetDataFromCurrentView(f) =>
        val g = f.asInstanceOf[CurrentView[Any] => Any]
        nodeView.forward(GetDataFromCurrentView[Any, Any](cv => g(cv.copy(vault = wallet))))
      case m => nodeView.forward(m)
    }
  }

  private def withUtxoBootstrapFixture[T](test: WalletFixture => T): T = {
    val utxoBootstrapSettings = settings.copy(
      nodeSettings = settings.nodeSettings.copy(
        utxoSettings = settings.nodeSettings.utxoSettings.copy(utxoBootstrap = true)
      )
    )
    new WalletFixture(utxoBootstrapSettings, parameters, getCurrentView(_).vault).apply(test)
  }

  property("a failed UTXO snapshot chunk is scanned again when the scan resumes") {
    withUtxoBootstrapFixture { implicit w =>
      implicit val system: ActorSystem = w.actorSystem
      val address = getPublicKeys.head
      val pkBytes = groupElemToBytes(address.pubkey.value)
      val testWallet = new FailOnceWallet(getHistory, w.settings.copy(directory = createTempDir.getAbsolutePath),
        parameters, _.propositionBytes.containsSlice(pkBytes))
      val proxy = system.actorOf(Props(new SwapWalletProxy(w.nodeViewHolderRef, testWallet)))
      val scannerRef = UtxoSetScanner(proxy, depth = 3)
      try {
        val genesisBlock = makeGenesisBlock(address.pubkey, randomNewAsset)
        val expectedGenesisBalance = balanceAmount(boxesAvailable(genesisBlock, address.pubkey))
        expectedGenesisBalance should be > 0L

        applyBlock(genesisBlock) shouldBe 'success

        implicit val patienceConfig: PatienceConfig = PatienceConfig(10.seconds, 100.millis)
        eventually {
          testWallet.failed shouldBe true
        }
        val afterFailure = await(testWallet.confirmedBalances)
        info(s"after failure: height=${afterFailure.height} balance=${afterFailure.walletBalance}")
        afterFailure.height shouldBe 0
        afterFailure.walletBalance shouldBe 0L

        system.eventStream.publish(StartUtxoSetScan(rescan = false))

        eventually {
          val resumed = await(testWallet.confirmedBalances)
          resumed.height shouldBe genesisBlock.height
          resumed.walletBalance shouldBe expectedGenesisBalance
        }
      } finally {
        system.stop(scannerRef)
        system.stop(proxy)
        system.stop(testWallet.walletActor)
      }
    }
  }
}
