package org.ergoplatform.local

import akka.actor.Actor
import org.ergoplatform.local.CleanupWorker.RunCleanup
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.RecheckMempool
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.UtxoStateReader
import org.ergoplatform.settings.NodeConfigurationSettings
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.RecheckedTransactions
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.UUID

/**
  * Performs mempool transactions re-validation. Called on a new block coming.
  * Reports a terminal result to the auditor for every completed Future.
  *
  * The actual re-validation logic lives in [[CleanupWorker.validatePool]], so that it can be
  * exercised without an actor system; this actor only wires it to the auditor and node view holder.
  */
class CleanupWorker(nodeSettings: NodeConfigurationSettings) extends Actor with ScorexLogging {

  // Transaction can be re-checked only after this delay
  private val TimeLimit = nodeSettings.mempoolCleanupDuration.toMillis

  override def preStart(): Unit = {
    log.info("Cleanup worker started")
  }

  override def receive: Receive = {
    case RunCleanup(jobId, request) =>
      val s = sender()
      validatePool(request.state, request.mempool, request.stateRevision).onComplete { result =>
        s ! CleanupDone(jobId, result)
        result.failed.foreach { ex =>
          log.error("Mempool validation failed", ex)
        }
      }

    //Should not be here, if non-expected signal comes, check logic
    case a: Any => log.warn(s"Strange input: $a")
  }

  private def validatePool(validator: UtxoStateReader,
                           mempool: ErgoMemPoolReader,
                           stateRevision: UUID): Future[RecheckedTransactions] = Future {
    val allPoolTxs = mempool.getAllPrioritized
    val res = CleanupWorker.validatePool(
      validator = validator,
      poolTxs = allPoolTxs,
      maxTransactionCost = nodeSettings.maxTransactionCost,
      timeLimit = TimeLimit,
      now = System.currentTimeMillis()
    )
    RecheckedTransactions(stateRevision, allPoolTxs, res.validated, res.invalidated)
  }

}

object CleanupWorker extends ScorexLogging {

  /**
    *
    * A command to run (partial) memory pool cleanup
    *
    * @param jobId - identifies this run independently of the state revision
    * @param request - state, pool and owner-issued state revision to validate
    */
  case class RunCleanup(jobId: UUID, request: RecheckMempool)

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
    * @param poolTxs   - pool transactions, sorted by priority
    * @param timeLimit - a transaction can be re-checked only after this delay, in milliseconds
    * @param now       - current time, in milliseconds
    * @return transactions to be re-validated, sorted by priority (a parent comes before its children)
    */
  def transactionsToValidate(poolTxs: Seq[UnconfirmedTransaction],
                             timeLimit: Long,
                             now: Long): Seq[UnconfirmedTransaction] =
    poolTxs.filter { utx =>
      (now - utx.lastCheckedTime) > timeLimit
    }

  /**
    * Validates transactions from the memory pool, until `costLimit` of accumulated cost is reached.
    *
    * This is a pure function of its arguments: it does not read the clock and does not touch the
    * node view, so it can be called directly from tests.
    *
    * @param validator          - a state implementation which provides transaction validation
    * @param poolTxs            - pool transactions, sorted by priority (a parent comes before its children)
    * @param maxTransactionCost - maximum cost of a single transaction
    * @param timeLimit          - a transaction can be re-checked only after this delay, in milliseconds
    * @param now                - current time, in milliseconds
    * @param costLimit          - limit for total cost of transactions to be re-checked
    * @return - updated valid transactions and invalidated transaction ids
    */
  def validatePool(validator: UtxoStateReader,
                   poolTxs: Seq[UnconfirmedTransaction],
                   maxTransactionCost: Int,
                   timeLimit: Long,
                   now: Long,
                   costLimit: Long = CostLimit): CleanupResult = {

    // Check transactions sorted by priority. Parent transaction comes before its children.
    val txsToValidate = transactionsToValidate(poolTxs, timeLimit, now).toList

    // Take into account other transactions from the pool.
    // This provides possibility to validate transactions which are spending off-chain outputs.
    val state = validator.withUnconfirmedTransactions(poolTxs)

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
              log.debug(s"Transaction $txId failed cleanup validation: ${e.getMessage}")
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
