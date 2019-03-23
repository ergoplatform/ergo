package org.ergoplatform.local

import akka.actor.{Actor, ActorRef}
import org.ergoplatform.local.CleanupWorker.RunCleanup
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.core.NodeViewHolder.ReceivableMessages.EliminateTransactions
import scorex.core.transaction.state.TransactionValidation
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.util.{Random, Success}

class CleanupWorker(nodeViewHolderRef: ActorRef,
                    nodeSettings: NodeConfigurationSettings) extends Actor with ScorexLogging {

  private var validatedIndex: TreeSet[ModifierId] = TreeSet.empty[ModifierId]
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
    def validationLoop(txs: List[ErgoTransaction],
                       invalidated: Seq[ModifierId],
                       etAcc: Long): Seq[ModifierId] = txs match {
      case head :: tail if etAcc < nodeSettings.mempoolCleanupDuration.toNanos && !validatedIndex.contains(head.id) =>
        val t0 = System.nanoTime()
        val validationResult = validator.validate(head)
        val t1 = System.nanoTime()
        val accumulatedTime = etAcc + (t1 - t0)
        validationResult match {
          case Success(_) => validationLoop(tail, invalidated, accumulatedTime)
          case _ => validationLoop(tail, invalidated :+ head.id, accumulatedTime)
        }
      case _ :: tail if etAcc < nodeSettings.mempoolCleanupDuration.toNanos =>
        validationLoop(tail, invalidated, etAcc)
      case _ =>
        invalidated
    }

    val mempoolTxs = mempool.getAll.toList
    val txsToValidate = Random.shuffle(mempoolTxs)
    val invalidatedIds = validationLoop(txsToValidate, Seq.empty, 0L)
    val validatedIds = txsToValidate.map(_.id).filterNot(invalidatedIds.contains)

    epochNr += 1
    if (epochNr % CleanupWorker.IndexRevisionInterval == 0) { // drop ids which are no longer presented in pool from index
      validatedIndex = validatedIndex.filter(mempoolTxs.map(_.id).contains) ++ validatedIds
    } else {
      validatedIndex ++= validatedIds
    }

    invalidatedIds
  }

}

object CleanupWorker {
  case class RunCleanup(validator: TransactionValidation[ErgoTransaction],
                        mempool: ErgoMemPoolReader)

  val IndexRevisionInterval: Int = 512
}
