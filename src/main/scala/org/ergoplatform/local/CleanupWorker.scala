package org.ergoplatform.local

import akka.actor.{Actor, ActorRef}
import org.ergoplatform.local.CleanupWorker.RunCleanup
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.UtxoStateReader
import org.ergoplatform.settings.NodeConfigurationSettings
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.{EliminateTransactions, RecheckedTransactions}
import scorex.core.transaction.state.TransactionValidation
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.util.{Failure, Success}

/**
  * Performs mempool validation task on demand.
  * Validation result is sent directly to `NodeViewHolder`.
  */
class CleanupWorker(nodeViewHolderRef: ActorRef,
                    nodeSettings: NodeConfigurationSettings) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    log.info("Cleanup worker started")
  }

  override def receive: Receive = {
    case RunCleanup(validator, mempool) =>
      runCleanup(validator, mempool)
      sender() ! CleanupDone

    //Should not be here, if non-expected signal comes, check logic
    case a: Any => log.warn(s"Strange input: $a")
  }

  private def runCleanup(validator: TransactionValidation,
                         mempool: ErgoMemPoolReader): Unit = {
    val (validated, toEliminate) = validatePool(validator, mempool)

    if(validated.nonEmpty) {
      nodeViewHolderRef ! RecheckedTransactions(validated)
    }
    if (toEliminate.nonEmpty) {
      log.info(s"${toEliminate.size} transactions from mempool were invalidated")
      nodeViewHolderRef ! EliminateTransactions(toEliminate)
    }
  }

  /**
    * Validates transactions from mempool for some specified amount of time.
    *
    * @return - invalidated transaction ids
    */
  private def validatePool(validator: TransactionValidation,
                           mempool: ErgoMemPoolReader): (Seq[UnconfirmedTransaction], Seq[ModifierId]) = {

    val now = System.currentTimeMillis()

    // Check transactions sorted by priority. Parent transaction comes before its children.
    val txsToValidate = mempool.getAllPrioritized.filter { utx =>
      (now - utx.lastCheckedTime) > nodeSettings.mempoolCleanupDuration.toMillis
    }.toList

    val costLimit = 7000000

    //internal loop function validating transactions, returns validated and invalidated transaction ids
    @tailrec
    def validationLoop(txs: Seq[UnconfirmedTransaction],
                       validated: Seq[UnconfirmedTransaction],
                       invalidated: Seq[ModifierId],
                       costAcc: Long): (Seq[UnconfirmedTransaction], Seq[ModifierId]) = {
      txs match {
        case head :: tail if costAcc < costLimit =>

          // Take into account previously validated transactions from the pool.
          // This provides possibility to validate transactions which are spending off-chain outputs.
          val state = validator match {
            case u: UtxoStateReader => u.withUnconfirmedTransactions(txsToValidate)
            case _ => validator
          }

          val validationResult = state.validateWithCost(head.transaction, nodeSettings.maxTransactionCost)

          val txId = head.id
          validationResult match {
            case Success(txCost) =>
              val updTx = head.updateCost(txCost)
              validationLoop(tail, validated :+ updTx, invalidated, txCost + costAcc)
            case Failure(e) =>
              log.info(s"Transaction $txId invalidated: ${e.getMessage}")
              validationLoop(tail, validated, invalidated :+ txId, head.lastCost.getOrElse(0) + costAcc) //add old cost
          }
        case _ =>
          validated -> invalidated
      }
    }

    validationLoop(txsToValidate, Seq.empty, Seq.empty, 0L)
  }

}

object CleanupWorker {

  /**
    * Constant which shows on how many cleanup operations (called when a new block arrives) a transaction
    * re-check happens.
    *
    * If transactions set is large and stable, then about (1/RevisionInterval)-th of the pool is checked
    *
    */
  val RevisionInterval: Int = 4

  /**
    *
    * A command to run (partial) memory pool cleanup
    *
    * @param validator - a state implementation which provides transaction validation
    * @param mempool - mempool reader instance
    */
  case class RunCleanup(validator: TransactionValidation, mempool: ErgoMemPoolReader)

}
