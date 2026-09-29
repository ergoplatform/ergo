package org.ergoplatform.local

import akka.actor.{Actor, ActorRef}
import org.ergoplatform.local.CleanupWorker.RunCleanup
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.UtxoStateReader
import org.ergoplatform.settings.NodeConfigurationSettings
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{EliminateTransactions, RecheckedTransactions}
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Performs mempool transactions re-validation. Called on a new block coming.
  * Validation results sent directly to `NodeViewHolder`.
  *
  * The actual re-validation logic lives in [[CleanupWorker.validatePool]], so that it can be
  * exercised without an actor system; this actor only wires it to the node view holder.
  */
class CleanupWorker(nodeViewHolderRef: ActorRef,
                    nodeSettings: NodeConfigurationSettings) extends Actor with ScorexLogging {

  // Transaction can be re-checked only after this delay
  private val TimeLimit = nodeSettings.mempoolCleanupDuration.toMillis

  override def preStart(): Unit = {
    log.info("Cleanup worker started")
  }

  override def receive: Receive = {
    case RunCleanup(validator, mempool) =>
      val s = sender()
      Future {
        CleanupWorker.validatePool(
          validator = validator,
          mempool = mempool,
          maxTransactionCost = nodeSettings.maxTransactionCost,
          timeLimit = TimeLimit,
          now = System.currentTimeMillis()
        )
      }.map { case CleanupWorker.CleanupResult(validated, toEliminate) =>
          log.debug(s"${validated.size} re-checked mempool transactions were ok, " +
            s"${toEliminate.size} transactions were invalidated")

          if (validated.nonEmpty) {
            nodeViewHolderRef ! RecheckedTransactions(validated)
          }
          if (toEliminate.nonEmpty) {
            nodeViewHolderRef ! EliminateTransactions(toEliminate)
          }
          s ! CleanupDone
        }.andThen { case Failure(ex) =>
          logger.error("Mempool validation failed", ex)
        }

    //Should not be here, if non-expected signal comes, check logic
    case a: Any => log.warn(s"Strange input: $a")
  }

}

object CleanupWorker extends ScorexLogging {

  /**
    *
    * A command to run (partial) memory pool cleanup
    *
    * @param validator - a state implementation which provides transaction validation
    * @param mempool - mempool reader instance
    */
  case class RunCleanup(validator: UtxoStateReader, mempool: ErgoMemPoolReader)

  /**
    * Outcome of a memory pool re-validation pass.
    *
    * @param validated   - transactions which are still valid, with their costs updated
    * @param invalidated - ids of transactions which are not valid anymore
    */
  case class CleanupResult(validated: Seq[UnconfirmedTransaction], invalidated: Seq[ModifierId])

  // Limit for total cost of transactions to be re-checked. Hard-coded for now.
  val CostLimit: Long = 7000000

  /**
    * Selects mempool transactions which were not re-checked recently enough.
    *
    * @param mempool   - mempool reader instance
    * @param timeLimit - a transaction can be re-checked only after this delay, in milliseconds
    * @param now       - current time, in milliseconds
    * @return transactions to be re-validated, sorted by priority (a parent comes before its children)
    */
  def transactionsToValidate(mempool: ErgoMemPoolReader,
                             timeLimit: Long,
                             now: Long): Seq[UnconfirmedTransaction] =
    mempool.getAllPrioritized.filter { utx =>
      (now - utx.lastCheckedTime) > timeLimit
    }

  /**
    * Validates transactions from the memory pool, until `costLimit` of accumulated cost is reached.
    *
    * This is a pure function of its arguments: it does not read the clock and does not touch the
    * node view, so it can be called directly from tests.
    *
    * @param validator          - a state implementation which provides transaction validation
    * @param mempool            - mempool reader instance
    * @param maxTransactionCost - maximum cost of a single transaction
    * @param timeLimit          - a transaction can be re-checked only after this delay, in milliseconds
    * @param now                - current time, in milliseconds
    * @param costLimit          - limit for total cost of transactions to be re-checked
    * @return - updated valid transactions and invalidated transaction ids
    */
  def validatePool(validator: UtxoStateReader,
                   mempool: ErgoMemPoolReader,
                   maxTransactionCost: Int,
                   timeLimit: Long,
                   now: Long,
                   costLimit: Long = CostLimit): CleanupResult = {

    // Check transactions sorted by priority. Parent transaction comes before its children.
    val allPoolTxs = mempool.getAllPrioritized
    val txsToValidate = transactionsToValidate(mempool, timeLimit, now).toList

    // Take into account other transactions from the pool.
    // This provides possibility to validate transactions which are spending off-chain outputs.
    val state = validator.withUnconfirmedTransactions(allPoolTxs)

    //internal loop function validating transactions, returns validated and invalidated transaction ids
    @tailrec
    def validationLoop(txs: Seq[UnconfirmedTransaction],
                       validated: mutable.ArrayBuilder[UnconfirmedTransaction],
                       invalidated: mutable.ArrayBuilder[ModifierId],
                       costAcc: Long
                      ): (mutable.ArrayBuilder[UnconfirmedTransaction], mutable.ArrayBuilder[ModifierId]) = {
      txs match {
        case head :: tail if costAcc < costLimit =>
          val validationContext = state.stateContext.simplifiedUpcoming()
          state.validateWithCost(head.transaction, validationContext, maxTransactionCost, None) match {
            case Success(txCost) =>
              val updTx = head.withCost(txCost)
              validationLoop(tail, validated += updTx, invalidated, txCost + costAcc)
            case Failure(e) =>
              val txId = head.id
              log.info(s"Transaction $txId invalidated: ${e.getMessage}")
              validationLoop(tail, validated, invalidated += txId, head.lastCost.getOrElse(0) + costAcc) //add old cost
          }
        case _ =>
          validated -> invalidated
      }
    }

    val res = validationLoop(txsToValidate, mutable.ArrayBuilder.make(), mutable.ArrayBuilder.make(), 0L)
    CleanupResult(wrapRefArray(res._1.result()), wrapRefArray(res._2.result()))
  }

}
