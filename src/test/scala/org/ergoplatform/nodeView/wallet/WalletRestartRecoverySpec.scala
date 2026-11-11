package org.ergoplatform.nodeView.wallet

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.testkit.TestProbe
import akka.util.Timeout
import org.ergoplatform.{ErgoAddressEncoder, Pay2SAddress}
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.{DeclinedTransaction, FailedTransaction}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.{CurrentView, ReceivableMessages}
import org.ergoplatform.nodeView.ErgoNodeViewRef
import org.ergoplatform.nodeView.state.ErgoState
import org.ergoplatform.nodeView.wallet.requests.PaymentRequest
import org.ergoplatform.utils.{ErgoCorePropertyTest, WalletTestOps}
import org.ergoplatform.utils.fixtures.WalletFixture
import org.scalatest.concurrent.Eventually
import scorex.db.{LDBFactory, StoreRegistry}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class WalletRestartRecoverySpec extends ErgoCorePropertyTest with WalletTestOps with Eventually {
  import org.ergoplatform.utils.ErgoCoreTestConstants._
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  override implicit val patienceConfig: PatienceConfig = PatienceConfig(15.seconds, 100.millis)
  private implicit val addressEncoder: ErgoAddressEncoder =
    ErgoAddressEncoder(ErgoAddressEncoder.TestnetNetworkPrefix)

  property("retain an already pooled restored payment across a second store reopen") {
    checkRecovery(blacklistOnRestore = false)
  }

  property("forget an invalidated restored payment and keep it absent after another store reopen") {
    checkRecovery(blacklistOnRestore = true)
  }

  private def checkRecovery(blacklistOnRestore: Boolean): Unit = {
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
    val probe = TestProbe()(restartedSystem)
    restartedSystem.eventStream.subscribe(probe.ref, classOf[FailedTransaction])
    val restartSettings = if (blacklistOnRestore) {
      fixture.settings.copy(nodeSettings = fixture.settings.nodeSettings.copy(blacklistedTransactions = Seq(pending.id)))
    } else fixture.settings
    val restarted = ErgoNodeViewRef(restartSettings)(restartedSystem)
    type View = CurrentView[ErgoState[_]]
    val expectedIds = if (blacklistOnRestore) Seq.empty else Seq(pending.id)
    try {
      if (blacklistOnRestore) {
        val failed = probe.expectMsgType[FailedTransaction](20.seconds)
        failed.transaction.id shouldBe pending.id
        failed.error.getMessage shouldBe "blacklisted tx"
      }
      eventually {
        val request = ReceivableMessages.GetDataFromCurrentView[ErgoState[_], View](v => v)
        val view = Await.result((restarted ? request).mapTo[View], 20.seconds)
        view.pool.getAll.map(_.id) shouldBe expectedIds
        Await.result(view.vault.unconfirmedTransactionsToRestore, 20.seconds)
          .map(_.id) shouldBe expectedIds
      }

      if (!blacklistOnRestore) {
      // The first reopen above admits the payment. Exercise the real duplicate guard,
      // rather than replacing mempool admission with a stubbed Declined outcome.
      restartedSystem.eventStream.subscribe(probe.ref, classOf[DeclinedTransaction])
      probe.send(restarted, ReceivableMessages.RestoredTransaction(UnconfirmedTransaction(pending, None)))
      probe.expectMsgType[DeclinedTransaction](20.seconds).transaction.id shouldBe pending.id

      // Send the read from the same probe after the restore. The holder then asks its
      // wallet after any forget request, making the persistence assertion a mailbox barrier.
      probe.send(restarted, ReceivableMessages.GetDataFromCurrentView[ErgoState[_], Future[Seq[ErgoTransaction]]](
        v => {
          v.pool.getAll.map(_.id) shouldBe Seq(pending.id)
          v.vault.unconfirmedTransactionsToRestore
        }
      ))
      Await.result(probe.expectMsgType[Future[Seq[ErgoTransaction]]](20.seconds), 20.seconds)
        .map(_.id) shouldBe Seq(pending.id)
      }
    } finally {
      Await.result(restartedSystem.terminate(), 30.seconds)
      closeStores()
    }

    val secondSystem = ActorSystem()
    val secondRestart = ErgoNodeViewRef(fixture.settings)(secondSystem)
    try {
      eventually {
        val request = ReceivableMessages.GetDataFromCurrentView[ErgoState[_], View](v => v)
        val view = Await.result((secondRestart ? request).mapTo[View], 20.seconds)
        view.pool.getAll.map(_.id) shouldBe expectedIds
        Await.result(view.vault.unconfirmedTransactionsToRestore, 20.seconds)
          .map(_.id) shouldBe expectedIds
      }
    } finally {
      Await.result(secondSystem.terminate(), 30.seconds)
      closeStores()
    }
  }
}
