package org.ergoplatform.nodeView.wallet

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import org.ergoplatform.{ErgoAddressEncoder, Pay2SAddress}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoNodeViewHolder.{CurrentView, ReceivableMessages}
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.fixtures.WalletFixture
import org.scalatest.concurrent.Eventually
import scorex.db.{LDBFactory, StoreRegistry}

import scala.concurrent.Await
import scala.concurrent.duration._

class WalletRestartRecoverySpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(15.seconds, 100.millis)
  private implicit val addressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(ErgoAddressEncoder.TestnetNetworkPrefix)

  property("restore a persisted pending payment into the real mempool after closing and reopening stores") {
    val fixture = new WalletFixture(settings, parameters, getCurrentView(_).vault)
    def closeStores(): Unit = {
      val registry = LDBFactory.factory.asInstanceOf[StoreRegistry]
      registry.lock.writeLock().lock()
      try registry.map.toVector.collect {
        case (path, store) if path.toPath.toAbsolutePath.normalize()
          .startsWith(fixture.nodeViewDir.toPath.toAbsolutePath.normalize()) => store
      }.foreach(_.close())
      finally registry.lock.writeLock().unlock()
    }
    val pending: ErgoTransaction = try fixture.apply { implicit active =>
      val address = getPublicKeys.head
      applyBlock(makeGenesisBlock(address.pubkey)) shouldBe 'success
      val tx = eventually {
        val amount = getConfirmedBalances.walletBalance / 2
        val fee = math.max(settings.nodeSettings.minimalFeeAmount, 1000000L)
        val requests = Seq(
          PaymentRequest(address, amount, Array.empty, Map.empty),
          PaymentRequest(Pay2SAddress(settings.chainSettings.monetary.feeProposition),
            fee, Array.empty, Map.empty)
        )
        await(wallet.generateTransaction(requests)).get
      }
      wallet.scanOffchain(tx)
      eventually {
        await(wallet.unconfirmedTransactionsToRestore).map(_.id) shouldBe Seq(tx.id)
      }
      getCurrentView.pool.contains(tx.id) shouldBe false
      tx
    } finally {
      Await.result(fixture.actorSystem.whenTerminated, 30.seconds)
      // A wrapper around the same live DB would not establish persistence.
      closeStores()
    }

    implicit val timeout: Timeout = Timeout(20.seconds)
    val restartedSystem = ActorSystem()
    val restarted = ErgoNodeViewRef(fixture.settings)(restartedSystem)
    type View = CurrentView[ErgoState[_]]
    try {
      eventually {
        val request = ReceivableMessages.GetDataFromCurrentView[ErgoState[_], View](v => v)
        val view = Await.result((restarted ? request).mapTo[View], 20.seconds)
        view.pool.getAll.map(_.id) shouldBe Seq(pending.id)
        Await.result(view.vault.unconfirmedTransactionsToRestore, 20.seconds)
          .map(_.id) shouldBe Seq(pending.id)
      }
    } finally {
      Await.result(restartedSystem.terminate(), 30.seconds)
      closeStores()
    }
  }
}
