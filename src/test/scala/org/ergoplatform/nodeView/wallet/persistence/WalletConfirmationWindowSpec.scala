package org.ergoplatform.nodeView.wallet.persistence

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.db.DBSpec
import org.ergoplatform.nodeView.wallet.ErgoWalletActorMessages.GetFilteredScanTxs
import org.ergoplatform.nodeView.wallet.{AugWalletTransaction, ErgoWalletActor, ErgoWalletServiceImpl, ErgoWalletState, WalletTransaction, WalletVars}
import org.ergoplatform.utils.ErgoCorePropertyTest
import org.ergoplatform.wallet.Constants.{PaymentsScanId, ScanId}

import scala.concurrent.Await
import scala.concurrent.duration._

class WalletConfirmationWindowSpec extends ErgoCorePropertyTest with DBSpec {
  import org.ergoplatform.utils.ErgoCoreTestConstants.parameters
  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators.walletTransactionGen

  private val otherScan = ScanId @@ (PaymentsScanId + 1).toShort

  private def withWallet(height: Int = 100)(test: (GetFilteredScanTxs => Seq[AugWalletTransaction],
    Seq[WalletTransaction], AugWalletTransaction) => Unit): Unit = {
    withVersionedStore(10) { store =>
      implicit val system: ActorSystem = ActorSystem("wallet-confirmation-window")
      try {
        val records = Seq(0, 89, 90, 95, 96, 100).map { h =>
          walletTransactionGen.sample.get.copy(inclusionHeight = h, scanIds = Seq(PaymentsScanId))
        } :+ walletTransactionGen.sample.get.copy(inclusionHeight = 95, scanIds = Seq(otherScan))
        records.foreach(tx => WalletRegistry.putTx(KeyValuePairsBag.empty, tx).transact(store).get)
        val registry = new WalletRegistry(store)(settings.walletSettings)
        // This read-only actor path needs the registry and chain height, not wallet initialization.
        val state = new ErgoWalletState(null, None, registry, OffChainRegistry.empty, None,
          WalletVars(None, Seq.empty, None), None, None, None, parameters, 1000, None, false) {
          override def fullHeight: Int = height
        }
        val unconfirmed = AugWalletTransaction(
          walletTransactionGen.sample.get.copy(inclusionHeight = 0, scanIds = Seq(PaymentsScanId)), 0)
        val service = new ErgoWalletServiceImpl(settings) {
          override def getUnconfirmedTransactions(s: ErgoWalletState, scan: ScanId): Seq[AugWalletTransaction] =
            if (scan == PaymentsScanId) Seq(unconfirmed) else Seq.empty
        }
        val actor = TestActorRef(new ErgoWalletActor(settings, parameters, service, null, null) {
          override def preStart(): Unit = ()
          override def receive: Receive = {
            case GetFilteredScanTxs(scans, from, to, min, max, unconfirmed) =>
              readFiltered(state, scans, from, to, min, max, unconfirmed)
          }
        })
        val client = TestProbe()
        def query(request: GetFilteredScanTxs): Seq[AugWalletTransaction] = {
          client.send(actor, request)
          client.expectMsgType[Seq[AugWalletTransaction]]
        }
        test(query, records, unconfirmed)
      } finally Await.result(system.terminate(), 10.seconds)
    }
  }

  private def request(min: Int = 0, max: Int = Int.MaxValue, from: Int = 0,
    to: Int = Int.MaxValue, unconfirmed: Boolean = false,
    scans: List[ScanId] = List(PaymentsScanId)): GetFilteredScanTxs =
    GetFilteredScanTxs(scans, from, to, min, max, unconfirmed)

  property("confirmation limits select exact inclusive transaction identities in descending height order") {
    withWallet() { (query, records, _) =>
      val expected = records.filter(tx => tx.scanIds.contains(PaymentsScanId) &&
        Set(90, 95).contains(tx.inclusionHeight)).sortBy(-_.inclusionHeight)
      val result = query(request(min = 5, max = 10))
      result.map(_.wtx.id) shouldBe expected.map(_.id)
      result.map(_.numConfirmations) shouldBe Seq(5, 10)
      query(request(min = 5, max = 5)).map(_.wtx.id) shouldBe expected.take(1).map(_.id)
    }
  }

  property("empty and inverted confirmation or height windows return no transactions") {
    withWallet() { (query, _, _) =>
      Seq(request(min = 101), request(min = Int.MaxValue), request(min = 10, max = 5),
        request(from = 96, to = 90), request(from = 101), request(to = -1),
        request(min = 5, from = 96), request(max = -1)).foreach { r =>
        query(r) shouldBe empty
      }
    }
  }

  property("unfiltered and height-filtered reads preserve scan selection and ordering") {
    withWallet() { (query, records, _) =>
      val payments = records.filter(_.scanIds.contains(PaymentsScanId)).sortBy(-_.inclusionHeight)
      query(request()).map(_.wtx.id) shouldBe payments.map(_.id)
      query(request(from = 90, to = 95)).map(_.wtx.id) shouldBe
        payments.filter(tx => tx.inclusionHeight >= 90 && tx.inclusionHeight <= 95).map(_.id)
      query(request(min = 5, max = 10, scans = List(otherScan))).map(_.wtx.id) shouldBe Seq(records.last.id)
      query(request(scans = Nil)) shouldBe empty
    }
  }

  property("unconfirmed inclusion keeps the existing upper-height gate and excludes impossible windows") {
    withWallet() { (query, _, unconfirmed) =>
      query(request(unconfirmed = true)).last shouldBe unconfirmed
      query(request(to = 100, unconfirmed = true)).exists(_.wtx.id == unconfirmed.wtx.id) shouldBe false
      query(request(min = 5, unconfirmed = true)).exists(_.wtx.id == unconfirmed.wtx.id) shouldBe false
      query(request(min = 10, max = 5, unconfirmed = true)) shouldBe empty
    }
  }

  property("confirmation arithmetic does not wrap at signed integer bounds") {
    withWallet(Int.MaxValue) { (query, records, _) =>
      query(request(min = Int.MaxValue)).map(_.wtx.id) shouldBe Seq(records.head.id)
      query(request(min = Int.MinValue)).map(_.wtx.id).toSet shouldBe
        records.filter(_.scanIds.contains(PaymentsScanId)).map(_.id).toSet
      query(request(max = Int.MinValue)) shouldBe empty
    }
  }
}
