package org.ergoplatform.local

import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.{ErgoTestHelpers, MempoolTestHelpers, NodeViewTestOps, RandomWrapper}
import org.scalatest.flatspec.AnyFlatSpec

/**
  * Tests for the memory pool re-validation logic extracted from the `CleanupWorker` actor.
  */
class CleanupWorkerSpec extends AnyFlatSpec with NodeViewTestOps with ErgoTestHelpers with MempoolTestHelpers {

  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  import org.ergoplatform.utils.generators.ValidBlocksGenerators._

  private val settingsToTest: ErgoSettings = settings
  private val maxTransactionCost: Int = settingsToTest.nodeSettings.maxTransactionCost

  private val TimeLimit = 1000L
  private val Now = 1000000L
  // the production limit would truncate the pools used here, which is exercised by its own test below
  private val NoCostLimit = Long.MaxValue

  /** Transaction last checked `age` milliseconds ago */
  private def unconfirmed(tx: ErgoTransaction, age: Long): UnconfirmedTransaction =
    new UnconfirmedTransaction(tx, lastCost = None, createdTime = Now - age,
      lastCheckedTime = Now - age, transactionBytes = Some(tx.bytes), source = None)

  /** A state with one block applied, and transactions which are valid against it */
  private def stateWithValidTxs: (UtxoState, Seq[ErgoTransaction]) = {
    val (us0, bh0) = createUtxoState(settingsToTest)
    val (genesisTxs, bh1) = validTransactionsFromBoxHolder(bh0)
    val block = validFullBlock(None, us0, genesisTxs)
    val us = us0.applyModifier(block, None)(_ => ()).get

    val boxes = bh1.boxes.values.toList.filter(_.proposition != genesisEmissionBox.proposition)
    (us, validTransactionsFromBoxes(200000, boxes, new RandomWrapper)._1)
  }

  /** Transactions spending inputs which do not exist in any state */
  private def invalidTxs(n: Int): Seq[ErgoTransaction] =
    (1 to n).map(_ => invalidErgoTransactionGen.sample.get)

  it should "not re-check transactions checked recently enough" in {
    val (us, validTxs) = stateWithValidTxs
    val fresh = validTxs.map(tx => unconfirmed(tx, age = TimeLimit / 2))

    CleanupWorker.transactionsToValidate(new FakeMempool(fresh), TimeLimit, Now) shouldBe empty

    val result = CleanupWorker.validatePool(us, new FakeMempool(fresh), maxTransactionCost, TimeLimit, Now)
    result.validated shouldBe empty
    result.invalidated shouldBe empty
  }

  it should "re-check transactions which are stale enough" in {
    val (_, validTxs) = stateWithValidTxs
    val stale = validTxs.map(tx => unconfirmed(tx, age = TimeLimit * 2))
    val fresh = validTxs.map(tx => unconfirmed(tx, age = 0))

    CleanupWorker.transactionsToValidate(new FakeMempool(stale), TimeLimit, Now).map(_.id) shouldBe stale.map(_.id)
    CleanupWorker.transactionsToValidate(new FakeMempool(fresh), TimeLimit, Now) shouldBe empty
    // the boundary is exclusive: exactly `TimeLimit` old is not stale yet
    CleanupWorker.transactionsToValidate(
      new FakeMempool(validTxs.map(tx => unconfirmed(tx, age = TimeLimit))), TimeLimit, Now) shouldBe empty
  }

  it should "keep still-valid transactions and update their cost" in {
    val (us, validTxs) = stateWithValidTxs
    validTxs.nonEmpty shouldBe true
    val pool = validTxs.map(tx => unconfirmed(tx, age = TimeLimit * 2))

    val result = CleanupWorker.validatePool(us, new FakeMempool(pool), maxTransactionCost, TimeLimit, Now, NoCostLimit)

    result.invalidated shouldBe empty
    result.validated.map(_.id) should contain theSameElementsAs pool.map(_.id)
    // cost is unknown before the check and is filled in by it
    pool.forall(_.lastCost.isEmpty) shouldBe true
    result.validated.forall(_.lastCost.exists(_ > 0)) shouldBe true
  }

  it should "invalidate transactions which are not valid anymore" in {
    val (us, _) = stateWithValidTxs
    val broken = invalidTxs(4)
    val pool = broken.map(tx => unconfirmed(tx, age = TimeLimit * 2))

    val result = CleanupWorker.validatePool(us, new FakeMempool(pool), maxTransactionCost, TimeLimit, Now, NoCostLimit)

    result.validated shouldBe empty
    result.invalidated should contain theSameElementsAs pool.map(_.id)
  }

  it should "report both validated and invalidated transactions of a mixed pool" in {
    val (us, validTxs) = stateWithValidTxs
    val broken = invalidTxs(3)
    val pool = (validTxs ++ broken).map(tx => unconfirmed(tx, age = TimeLimit * 2))

    val result = CleanupWorker.validatePool(us, new FakeMempool(pool), maxTransactionCost, TimeLimit, Now, NoCostLimit)

    result.validated.map(_.id) should contain theSameElementsAs validTxs.map(_.id)
    result.invalidated should contain theSameElementsAs broken.map(_.id)
  }

  it should "stop validating once the cost limit is reached" in {
    val (us, validTxs) = stateWithValidTxs
    // more than one transaction is needed for the limit to be observable
    validTxs.size > 1 shouldBe true
    val pool = validTxs.map(tx => unconfirmed(tx, age = TimeLimit * 2))

    // costAcc starts at 0, so the first transaction is always checked and then the limit stops the loop
    val limited = CleanupWorker.validatePool(us, new FakeMempool(pool), maxTransactionCost, TimeLimit, Now, costLimit = 1L)
    limited.validated.size + limited.invalidated.size shouldBe 1

    val unlimited = CleanupWorker.validatePool(us, new FakeMempool(pool), maxTransactionCost, TimeLimit, Now, NoCostLimit)
    unlimited.validated.size + unlimited.invalidated.size shouldBe pool.size
  }

}
