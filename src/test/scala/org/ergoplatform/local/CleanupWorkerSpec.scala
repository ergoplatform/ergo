package org.ergoplatform.local

import org.ergoplatform.local.CleanupWorker.CleanupState
import org.ergoplatform.utils.{ErgoTestHelpers, NodeViewTestOps}
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.duration._
import scala.collection.immutable.TreeSet
import scala.collection.mutable

class CleanupWorkerSpec extends AnyFlatSpec with NodeViewTestOps with ErgoTestHelpers {

  it should "validate transactions in mempory pool" in {
    val (us0, bh0) = createUtxoState(None)
    val (txs0, _) = validTransactionsFromBoxHolder(bh0)
    val b1 = validFullBlock(None, us0, txs0)
    val us = us0.applyModifier(b1).get

    val initialState = CleanupState(validatedIndex = TreeSet.empty, epochNr = 0)
    val (newState0, invalidTxIds0) = CleanupWorker.validatePool(initialState, us, txs0, 0.second)

    // 0 mempoolCleanupDuration should disable pool validation entirely
    newState0.validatedIndex shouldBe empty
    invalidTxIds0 shouldBe empty

    val (newState1, invalidTxIds1) = CleanupWorker.validatePool(initialState, us, txs0, 1.second)
    val validTxIds = newState1.validatedIndex

    // it should always partition incoming txs into valid and invalid
    txs0.size shouldBe (validTxIds.size + invalidTxIds1.size)

    // epoch counter should be increased
    newState1.epochNr shouldBe 1

    val (newState2, invalidTxIds2) = CleanupWorker.validatePool(newState1, us, txs0, 1.second)

    // second run with the same txs should end with the same validatedIndex and return the same invalidTxIds
    newState2.validatedIndex shouldBe validTxIds
    invalidTxIds2 shouldBe invalidTxIds1

    // epoch counter should be increased again
    newState2.epochNr shouldBe 2

    val newState3 =
      (1 to 2).foldLeft(newState2) { case (stateAcc, _) =>
        CleanupWorker.validatePool(stateAcc, us, mutable.WrappedArray.empty, 1.second)._1
      }

    // RevisionInterval should kick in at 4th iteration and cleanup validatedIndex
    newState3.validatedIndex shouldBe empty
  }

}
