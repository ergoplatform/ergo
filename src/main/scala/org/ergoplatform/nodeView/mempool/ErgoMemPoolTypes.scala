package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import scorex.util.{ModifierId, ScorexLogging}
import scala.util.Random

object ErgoMemPoolTypes extends ScorexLogging {

  /**
   * Hierarchy of sorting strategies for mempool transactions
   */
  sealed trait SortingOption

  object SortingOption {
    /**
     * Sort transactions by fee paid for transaction size, so fee/byte
     */
    case object FeePerByte extends SortingOption

    /**
     * Sort transactions by fee paid for transaction contracts validation cost, so fee/execution unit
     */
    case object FeePerCycle extends SortingOption

    /**
     * @return randomly chosen mempool sorting strategy
     */
    def random(): SortingOption = {
      if (Random.nextBoolean()) {
        FeePerByte
      } else {
        FeePerCycle
      }
    }
  }

  /**
   * Root of possible mempool transaction validation result family
   */
  sealed trait ProcessingOutcome {
    /**
     * Time when transaction validation was started
     */
    protected val validationStartTime: Long

    /**
     * We assume that validation ends when this processing result class is constructed
     */
    private val validationEndTime: Long = System.currentTimeMillis()

    /**
     * 5.0 JIT costing was designed in a way that 1000 cost units are roughly corresponding to 1 ms of 1 CPU core
     * on commodity hardware (of 2021). So if we do not know the exact cost of transaction, we can estimate it by
     * tracking validation time and then getting estimated validation cost by multiplying the time (in ms) by 1000
     */
    val costPerMs = 1000

    /**
     * Estimated validation cost, see comment for `costPerMs`
     */
    def cost: Int = {
      val timeDiff = validationEndTime - validationStartTime
      if (timeDiff == 0) {
        costPerMs
      } else if (timeDiff > 1000000) {
        Int.MaxValue // shouldn't be here, so this branch is mostly to have safe .toInt below
      } else {
        (timeDiff * costPerMs).toInt
      }
    }
  }

  object ProcessingOutcome {

    /**
     * Object signalling that a transaction is accepted to the memory pool
     */
    class Accepted(val tx: UnconfirmedTransaction,
                   override protected val validationStartTime: Long) extends ProcessingOutcome {
      override val cost: Int = tx.lastCost.getOrElse(super.cost)
    }

    /**
     * Class signalling that a valid transaction was rejected as it is double-spending inputs of mempool transactions
     * and has no bigger weight (fee/byte) than them on average.
     *
     * @param winnerTxIds - identifiers of transactions won in replace-by-fee auction
     */
    class DoubleSpendingLoser(val winnerTxIds: Set[ModifierId],
                              override protected val validationStartTime: Long) extends ProcessingOutcome

    /**
     * Class signalling that a transaction declined from being accepted into the memory pool
     */
    class Declined(val e: Throwable,
                   override protected val validationStartTime: Long) extends ProcessingOutcome


    /**
     * Class signalling that a transaction turned out to be invalid when checked in the mempool
     */
    class Invalidated(val e: Throwable,
                      override protected val validationStartTime: Long) extends ProcessingOutcome

  }

}
