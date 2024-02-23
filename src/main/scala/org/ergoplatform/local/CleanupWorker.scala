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
  */
class CleanupWorker(nodeViewHolderRef: ActorRef,
                    nodeSettings: NodeConfigurationSettings) extends Actor with ScorexLogging {

  // Limit for total cost of transactions to be re-checked. Hard-coded for now.
  private val CostLimit = 7000000

  // Transaction can be re-checked only after this delay
  private val TimeLimit = nodeSettings.mempoolCleanupDuration.toMillis

  override def preStart(): Unit = {
    log.info("Cleanup worker started")
  }

  override def receive: Receive = {
    case RunCleanup(validator, mempool) =>
      val s = sender()
      validatePool(validator, mempool)
        .map { case (validated, toEliminate) =>
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

  /**
    * Validates transactions from mempool for some specified amount of time.
    *
    * @return - updated valid transactions and invalidated transaction ids
    */
  private def validatePool(validator: UtxoStateReader,
                           mempool: ErgoMemPoolReader): Future[(Seq[UnconfirmedTransaction], Seq[ModifierId])] = Future {

    val now = System.currentTimeMillis()

    val allPoolTxs = mempool.getAllPrioritized
    // Check transactions sorted by priority. Parent transaction comes before its children.
    val txsToValidate = allPoolTxs.filter { utx =>
      (now - utx.lastCheckedTime) > TimeLimit
    }.toList


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
        case head :: tail if costAcc < CostLimit =>
          val validationContext = state.stateContext.simplifiedUpcoming()
          state.validateWithCost(head.transaction, validationContext, nodeSettings.maxTransactionCost, None) match {
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
    wrapRefArray(res._1.result()) -> wrapRefArray(res._2.result())
  }

}

object CleanupWorker {

  /**
    *
    * A command to run (partial) memory pool cleanup
    *
    * @param validator - a state implementation which provides transaction validation
    * @param mempool - mempool reader instance
    */
  case class RunCleanup(validator: UtxoStateReader, mempool: ErgoMemPoolReader)

}
