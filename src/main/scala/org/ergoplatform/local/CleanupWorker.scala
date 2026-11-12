package org.ergoplatform.local

import akka.actor.Actor
import org.ergoplatform.local.CleanupWorker.RunCleanup
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.RecheckMempool
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.nodeView.mempool.{ErgoMemPool, ErgoMemPoolReader}
import org.ergoplatform.nodeView.state.UtxoStateReader
import org.ergoplatform.settings.NodeConfigurationSettings
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.RecheckedTransactions
import scorex.db.ByteArrayWrapper
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
  */
class CleanupWorker(nodeSettings: NodeConfigurationSettings) extends Actor with ScorexLogging {

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

  /**
    * Validates transactions from mempool for some specified amount of time.
    *
    * @return - updated valid transactions and invalidated transaction ids
    */
  private def validatePool(validator: UtxoStateReader,
                           mempool: ErgoMemPoolReader,
                           stateRevision: UUID): Future[RecheckedTransactions] = {
    CleanupWorker.validatePool(validator, mempool, stateRevision, nodeSettings)
  }

}

object CleanupWorker extends ScorexLogging {

  // Limit for total cost of transactions to be re-checked. Hard-coded for now.
  private val CostLimit = 7000000

  /**
    *
    * A command to run (partial) memory pool cleanup
    *
    * @param jobId - identifies this run independently of the state revision
    * @param request - state, pool and owner-issued state revision to validate
    */
  case class RunCleanup(jobId: UUID, request: RecheckMempool)

  /**
    * Validates transactions from mempool for some specified amount of time.
    *
    * @return - updated valid transactions and invalidated transaction ids
    */
  private[local] def validatePool(validator: UtxoStateReader,
                                  mempool: ErgoMemPoolReader,
                                  stateRevision: UUID,
                                  nodeSettings: NodeConfigurationSettings): Future[RecheckedTransactions] = Future {

    val now = System.currentTimeMillis()
    val timeLimit = nodeSettings.mempoolCleanupDuration.toMillis

    // Check transactions sorted by priority. Parent transaction comes before its children.
    val allPoolTxs = mempool.getAllPrioritized
    val txsToValidate = allPoolTxs.filter { utx =>
      (now - utx.lastCheckedTime) > timeLimit
    }.toList


    // Take into account other transactions from the pool.
    // This provides possibility to validate transactions which are spending off-chain outputs.
    val state = validator.withUnconfirmedTransactions(allPoolTxs)
    val invalidatedOutputIds = mutable.Set.empty[ByteArrayWrapper]

    def markOutputsInvalidated(utx: UnconfirmedTransaction): Unit = {
      utx.transaction.outputs.foreach { box =>
        invalidatedOutputIds += ByteArrayWrapper(box.id)
      }
    }

    def spendsInvalidatedParentOutput(utx: UnconfirmedTransaction): Boolean = {
      utx.transaction.inputIds.exists(id => invalidatedOutputIds.contains(ByteArrayWrapper(id))) ||
        utx.transaction.dataInputs.exists(di => invalidatedOutputIds.contains(ByteArrayWrapper(di.boxId)))
    }

    //internal loop function validating transactions, returns validated and invalidated transaction ids
    @tailrec
    def validationLoop(txs: Seq[UnconfirmedTransaction],
                       validated: mutable.ArrayBuilder[UnconfirmedTransaction],
                       invalidated: mutable.ArrayBuilder[ModifierId],
                       costAcc: Long
                      ): (mutable.ArrayBuilder[UnconfirmedTransaction], mutable.ArrayBuilder[ModifierId]) = {
      txs match {
        case head :: tail if costAcc < CostLimit =>
          if (spendsInvalidatedParentOutput(head)) {
            val txId = head.id
            val txCost = head.lastCost.getOrElse(0)
            markOutputsInvalidated(head)
            log.debug(s"Transaction $txId failed cleanup validation: parent transaction output was invalidated")
            validationLoop(tail, validated, invalidated += txId, txCost + costAcc)
          } else if (head.isUsingBlockchainContext.contains(false) && head.validationResult.contains(true)) {
            val inputsExist = head.transaction.inputIds.forall(id => state.boxById(id).isDefined) &&
              head.transaction.dataInputs.forall(di => state.boxById(di.boxId).isDefined)
            val txCost = head.lastCost.getOrElse(0)
            if (inputsExist) {
              val updTx = head.withCost(txCost)
              validationLoop(tail, validated += updTx, invalidated, txCost + costAcc)
            } else {
              val txId = head.id
              markOutputsInvalidated(head)
              log.debug(s"Transaction $txId failed cleanup validation: not all inputs exist in state")
              validationLoop(tail, validated, invalidated += txId, txCost + costAcc)
            }
          } else {
            val validationContext = state.stateContext.simplifiedUpcoming()
            state.validateWithCost(head.transaction, validationContext, nodeSettings.maxTransactionCost, None) match {
              case Success(txCost) =>
                val usesBlockchainContext = head.isUsingBlockchainContext.getOrElse {
                  head.transaction.inputIds.exists { id =>
                    state.boxById(id).exists(b => b.ergoTree.isUsingBlockchainContext || b.ergoTree.hasDeserialize)
                  } || ErgoMemPool.containsStorageRentClaim(head.transaction)
                }
                val updTx = head.withValidationResult(
                  usesBlockchainContext = usesBlockchainContext,
                  isValid = true,
                  costOpt = Some(txCost)
                )
                validationLoop(tail, validated += updTx, invalidated, txCost + costAcc)
              case Failure(e) =>
                val txId = head.id
                markOutputsInvalidated(head)
                log.debug(s"Transaction $txId failed cleanup validation: ${e.getMessage}")
                validationLoop(tail, validated, invalidated += txId, head.lastCost.getOrElse(0) + costAcc) //add old cost
            }
          }
        case _ =>
          validated -> invalidated
      }
    }

    val res = validationLoop(txsToValidate, mutable.ArrayBuilder.make(), mutable.ArrayBuilder.make(), 0L)
    RecheckedTransactions(stateRevision, allPoolTxs, wrapRefArray(res._1.result()), wrapRefArray(res._2.result()))
  }

}
