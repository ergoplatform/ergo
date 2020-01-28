package org.ergoplatform.local

import akka.actor.{Actor, ActorRef}
import org.ergoplatform.local.CleanupWorker.RunCleanup
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.UtxoStateReader
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.core.NodeViewHolder.ReceivableMessages.EliminateTransactions
import scorex.core.transaction.state.TransactionValidation
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.util.{Failure, Success}

/**
  * Performs mempool validation task on demand.
  * Validation result is sent directly to `NodeViewHolder`.
  */
class CleanupWorker(nodeViewHolderRef: ActorRef,
                    nodeSettings: NodeConfigurationSettings) extends Actor with ScorexLogging {

  // keep some number of recently validated transactions in order
  // to avoid validating the same transactions too many times.
  private var validatedIndex: TreeSet[ModifierId] = TreeSet.empty[ModifierId]
  // count validation sessions in order to perform index cleanup.
  private var epochNr: Int = 0

  override def receive: Receive = {
    case RunCleanup(validator, mempool) =>
      runCleanup(validator, mempool)
      sender() ! CleanupDone
  }

  private def runCleanup(validator: TransactionValidation[ErgoTransaction],
                         mempool: ErgoMemPoolReader): Unit = {
    val toEliminate = validatePool(validator, mempool)
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
  private def validatePool(validator: TransactionValidation[ErgoTransaction],
                           mempool: ErgoMemPoolReader): Seq[ModifierId] = {

    @tailrec
    def validationLoop(txs: Seq[ErgoTransaction],
                       invalidated: Seq[ModifierId],
                       etAcc: Long): Seq[ModifierId] = txs match {
      case head :: tail if etAcc < nodeSettings.mempoolCleanupDuration.toNanos
        && !validatedIndex.contains(head.id) =>

        val state = validator match {
          case u: UtxoStateReader => u.withTransactions(txs)
          case _ => validator
        }

        val t0 = System.nanoTime()
        val validationResult = state.validate(head)
        val t1 = System.nanoTime()
        val accumulatedTime = etAcc + (t1 - t0)
        validationResult match {
          case Success(_) => validationLoop(tail, invalidated, accumulatedTime)
          case Failure(e) =>
            log.debug(s"Transaction ${head.id} invalidated: ${e.getMessage}")
            validationLoop(tail, invalidated :+ head.id, accumulatedTime)
        }
      case _ :: tail if etAcc < nodeSettings.mempoolCleanupDuration.toNanos =>
        // this transaction was validated earlier, skip it
        validationLoop(tail, invalidated, etAcc)
      case _ =>
        invalidated
    }

    val txsToValidate = mempool.getAll
   // val txsToValidate = Random.shuffle(mempoolTxs)

    val invalidatedIds = validationLoop(txsToValidate, Seq.empty, 0L)
    val validatedIds = txsToValidate.map(_.id).filterNot(invalidatedIds.contains)

    epochNr += 1
    if (epochNr % CleanupWorker.IndexRevisionInterval == 0) {
      // drop old index in order to check potentially outdated transactions again.
      validatedIndex = TreeSet(validatedIds: _*)
    } else {
      validatedIndex ++= validatedIds
    }

    invalidatedIds
  }

}

object CleanupWorker {

  case class RunCleanup(validator: TransactionValidation[ErgoTransaction],
                        mempool: ErgoMemPoolReader)

  val IndexRevisionInterval: Int = 16
}
