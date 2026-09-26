package org.ergoplatform.local

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import org.ergoplatform.Input
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.settings.Constants.TrueTree
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoCorePropertyTest, StorageRentTestHelpers}
import org.scalatest.BeforeAndAfterAll
import scorex.util.ModifierId
import sigma.interpreter.ProverResult

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Success

/**
  * Mempool recheck under the EIP draft "Storage Rent Claims Restricted to the First Transaction of a Block":
  * `CleanupWorker.validatePool` invalidates a pooled transaction which has become a storage rent claim at the
  * height of the next block (with `declineStorageRentClaims`, default true), the same policy as at admission.
  */
class CleanupWorkerStorageRentSpec extends ErgoCorePropertyTest with StorageRentTestHelpers with BeforeAndAfterAll {

  import org.ergoplatform.utils.ErgoCoreTestConstants.validationSettingsNoIl

  private implicit val system: ActorSystem = ActorSystem("CleanupWorkerStorageRentSpec")

  override def afterAll(): Unit = {
    Await.result(system.terminate(), 10.seconds)
    super.afterAll()
  }

  /** Height of the best block of the state; the recheck evaluates transactions for the next block, at tip + 1 */
  private val tip: Int = StoragePeriod + 4001

  /** A pooled transaction whose last check is long enough ago to be rechecked */
  private def pooled(tx: ErgoTransaction): UnconfirmedTransaction =
    new UnconfirmedTransaction(tx, None, createdTime = 0L, lastCheckedTime = 0L, Some(tx.bytes), None)

  private def recheck(s: ErgoSettings,
                      us: UtxoState,
                      txs: Seq[UnconfirmedTransaction]): (Seq[UnconfirmedTransaction], Seq[ModifierId]) = {
    val worker = TestActorRef(new CleanupWorker(TestProbe().ref, s.nodeSettings))
    val pool = ErgoMemPool.empty(s).put(txs)
    pool.size shouldBe txs.size
    Await.result(worker.underlyingActor.validatePool(us, pool), 60.seconds)
  }

  property("cleanupRecheckInvalidatesClaim: a pooled transaction which becomes a rent claim at the recheck height " +
    "is invalidated with declineStorageRentClaims = true, and kept with the setting false") {
    // StoragePeriod - 1 blocks old for a block at tip (so admissible then), exactly StoragePeriod old at tip + 1
    val crossing = boxAt(TrueTree, tip + 1 - StoragePeriod, seed = 51)
    val plain = boxAt(TrueTree, tip - 5, seed = 52)

    val ctx = stateContext(tip, Header.Interpreter60Version, rentSettings, validationSettingsNoIl)
    val us = utxoStateAt(Seq(crossing, plain), None, ctx, rentSettings)
    val upcoming = us.stateContext.simplifiedUpcoming()
    upcoming.currentHeight shouldBe tip + 1

    val crossingTx = rentShapedTx(crossing, tip + 1)
    ErgoTransaction.hasStorageRentClaim(crossingTx, IndexedSeq(crossing), tip) shouldBe false
    ErgoTransaction.hasStorageRentClaim(crossingTx, IndexedSeq(crossing), tip + 1) shouldBe true
    val plainTx = ErgoTransaction(IndexedSeq(Input(plain.id, ProverResult.empty)), IndexedSeq(recreated(plain, tip + 1)))

    // `true` script, recreated unchanged: valid at the recheck height on its own, so an invalidation below is the policy
    us.validateWithCost(crossingTx, upcoming, rentSettings.nodeSettings.maxTransactionCost, None) shouldBe a[Success[_]]

    rentSettings.nodeSettings.declineStorageRentClaims shouldBe true
    val (validated, invalidated) = recheck(rentSettings, us, Seq(pooled(crossingTx), pooled(plainTx)))
    invalidated shouldBe Seq(crossingTx.id)
    validated.map(_.id) shouldBe Seq(plainTx.id)

    // setting false: the same transaction is re-validated and kept
    val off = rentSettings.copy(nodeSettings = rentSettings.nodeSettings.copy(declineStorageRentClaims = false))
    val (validatedOff, invalidatedOff) = recheck(off, us, Seq(pooled(crossingTx), pooled(plainTx)))
    invalidatedOff shouldBe empty
    validatedOff.map(_.id) should contain theSameElementsAs Seq(crossingTx.id, plainTx.id)
  }

}
