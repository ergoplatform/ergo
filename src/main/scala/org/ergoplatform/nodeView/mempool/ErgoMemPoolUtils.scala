package org.ergoplatform.nodeView.mempool

import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import scorex.util.{ModifierId, ScorexLogging}
import scala.util.Random

/**
  * Additional types and functions used in ErgoMemPool
  */
object ErgoMemPoolUtils extends ScorexLogging {

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

    /**
      * Whether a persisted unconfirmed wallet transaction receiving this outcome during restart
      * recovery (`RestoredTransaction`) should keep its stored wallet record.
      *
      * Transient mempool or chain-state outcomes (`Accepted`, `DoubleSpendingLoser`, and `Declined`
      * for `AlreadyInPool`, `PoolCapacity`, or `MissingInputs`) retain the record until on-chain
      * confirmation, on-chain input conflict, or `WalletStorage.UnconfirmedTxLifetimeInBlocks`
      * expiry. Consensus invalidation (`Invalidated`) and deterministic local policy refusals
      * (`MinFeeNotMet`, `MempoolPolicy`) forget the record so unrelayable transactions do not lock
      * wallet inputs.
      */
    def retainPersistedWalletTx: Boolean
  }

  object ProcessingOutcome {

    /**
      * Structured classification of why a transaction was declined without being marked
      * consensus-invalid (`Invalidated`).
      */
    sealed trait DeclineReason {
      def retainPersistedWalletTx: Boolean
    }

    object DeclineReason {
      /** Transaction is already in the mempool (e.g. received via gossip before restore). */
      case object AlreadyInPool extends DeclineReason {
        override val retainPersistedWalletTx: Boolean = true
      }

      /** Mempool cannot currently admit or retain the transaction under pool capacity/eviction state. */
      case object PoolCapacity extends DeclineReason {
        override val retainPersistedWalletTx: Boolean = true
      }

      /** At least one input is not currently available in `UtxoState` + mempool. */
      case object MissingInputs extends DeclineReason {
        override val retainPersistedWalletTx: Boolean = true
      }

      /** Transaction fee is below `nodeSettings.minimalFeeAmount`. */
      case object MinFeeNotMet extends DeclineReason {
        override val retainPersistedWalletTx: Boolean = false
      }

      /** Transaction is rejected by local mempool policy prefilters (e.g. re-emission or storage rent). */
      case object MempoolPolicy extends DeclineReason {
        override val retainPersistedWalletTx: Boolean = false
      }
    }

    /**
     * Object signalling that a transaction is accepted to the memory pool
     */
    class Accepted(val tx: UnconfirmedTransaction,
                   override protected val validationStartTime: Long) extends ProcessingOutcome {
      override val cost: Int = tx.lastCost.getOrElse(super.cost)
      override val retainPersistedWalletTx: Boolean = true
    }

    /**
     * Class signalling that a valid transaction was rejected as it is double-spending inputs of mempool transactions
     * and has no bigger weight (fee/byte) than them on average.
     *
     * @param winnerTxIds - identifiers of transactions won in replace-by-fee auction
     */
    class DoubleSpendingLoser(val winnerTxIds: Set[ModifierId],
                              override protected val validationStartTime: Long) extends ProcessingOutcome {
      override val retainPersistedWalletTx: Boolean = true
      override def toString: String = s"DoubleSpendingLoser(${winnerTxIds.mkString(",")})"
    }

    /**
     * Class signalling that a transaction declined from being accepted into the memory pool
     */
    class Declined(val e: Throwable,
                   override protected val validationStartTime: Long,
                   val reason: DeclineReason = DeclineReason.PoolCapacity) extends ProcessingOutcome {
      override def retainPersistedWalletTx: Boolean = reason.retainPersistedWalletTx
      override def toString: String = s"Declined($reason: ${e.getMessage})"
    }


    /**
     * Class signalling that a transaction turned out to be invalid when checked in the mempool
     */
    class Invalidated(val e: Throwable,
                      override protected val validationStartTime: Long) extends ProcessingOutcome {
      override val retainPersistedWalletTx: Boolean = false
      override def toString: String = s"Invalidated(${e.getMessage})"
    }

  }

}
