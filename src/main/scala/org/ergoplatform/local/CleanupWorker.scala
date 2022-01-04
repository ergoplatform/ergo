package org.ergoplatform.local

import akka.actor.{Actor, ActorRef}
import org.ergoplatform.local.CleanupWorker.{CleanupState, RunCleanup}
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.nodeView.state.UtxoStateReader
import org.ergoplatform.settings.NodeConfigurationSettings
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.EliminateTransactions
import scorex.core.transaction.state.TransactionValidation
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
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

  def cleaningUp(cleanupState: CleanupState): Receive = {
    case RunCleanup(validator, mempool) =>
      val newCleanupState = runCleanup(cleanupState, validator, mempool.getAllPrioritized)
      context.become(cleaningUp(newCleanupState))
      sender() ! CleanupDone

    //Should not be here, if non-expected signal comes, check logic
    case a: Any => log.warn(s"Strange input: $a")
  }

  override def receive: Receive =
    cleaningUp(CleanupState(validatedIndex = TreeSet.empty[ModifierId], epochNr = 0))

  private def runCleanup(state: CleanupState,
                         validator: TransactionValidation,
                         txsToValidate: IndexedSeq[ErgoTransaction]): CleanupState = {
    val (newCleanupState, toEliminate) =
      CleanupWorker.validatePool(state, validator, txsToValidate, nodeSettings.mempoolCleanupDuration)
    if (toEliminate.nonEmpty) {
      log.info(s"${toEliminate.length} transactions from mempool were invalidated")
      nodeViewHolderRef ! EliminateTransactions(toEliminate)
    }
    newCleanupState
  }
}

object CleanupWorker extends ScorexLogging {

  /**
    * @param validatedIndex keep some number of recently validated transactions in order to avoid
    *                       validating the same transactions too many times.
    * @param epochNr count validation sessions in order to perform index cleanup.
    */
  case class CleanupState(validatedIndex: TreeSet[ModifierId], epochNr: Int)

  /**
    * Constant which shows on how many cleanup operations (called when a new block arrives) a transaction
    * re-check happens.
    *
    * If transactions set is large and stable, then about (1/RevisionInterval)-th of the pool is checked
    *
    */
  val RevisionInterval: Int = 4

  /**
    * Validates transactions from mempool for some specified amount of time.
    * @param cleanupState actor's state
    * @param validator supports stateful validation of any transaction
    * @param txsToValidate Check transactions sorted by priority. Parent transaction comes before its children.
    * @param cleanupDuration Time window within which a node performs mempool cleanup in between blocks application
    * @return new CleanupState and invalidated transaction ids
    */
  def validatePool(
    cleanupState: CleanupState,
    validator: TransactionValidation,
    txsToValidate: IndexedSeq[ErgoTransaction],
    cleanupDuration: FiniteDuration
  ): (CleanupState, IndexedSeq[ModifierId]) = {
    // internal loop function validating transactions, returns validated and invalidated transaction ids
    @tailrec
    def validationLoop(index: Int,
                       validated: mutable.ArrayBuilder[ModifierId],
                       invalidated: mutable.ArrayBuilder[ModifierId],
                       etAcc: Long): (IndexedSeq[ModifierId], IndexedSeq[ModifierId]) = {
      if (index < txsToValidate.length && etAcc < cleanupDuration.toNanos) {
        val tx = txsToValidate(index)
        if (!cleanupState.validatedIndex.contains(tx.id)) {
          // Take into account previously validated transactions from the pool.
          // This provides possibility to validate transactions which are spending off-chain outputs.
          val state = validator match {
            case u: UtxoStateReader => u.withTransactions(txsToValidate)
            case _ => validator
          }

          val t0 = System.nanoTime()
          val validationResult = state.validate(tx)
          val t1 = System.nanoTime()
          val accumulatedTime = etAcc + (t1 - t0)

          val txId = tx.id
          validationResult match {
            case Success(_) =>
              validationLoop(index+1, validated += txId, invalidated, accumulatedTime)
            case Failure(e) =>
              log.info(s"Transaction $txId invalidated: ${e.getMessage}")
              validationLoop(index+1, validated, invalidated += txId, accumulatedTime)
          }
        } else {
          // this transaction was validated earlier, skip it
          validationLoop(index+1, validated, invalidated, etAcc)
        }
      } else {
        mutable.WrappedArray.make(validated.result()) -> mutable.WrappedArray.make(invalidated.result())
      }
    }

    val (validatedIds, invalidatedIds) =
      validationLoop(index = 0, mutable.ArrayBuilder.make(), mutable.ArrayBuilder.make(), 0L)

    val newEpochNr = cleanupState.epochNr + 1
    val newValidatedIndex =
      if (newEpochNr % CleanupWorker.RevisionInterval == 0) {
        // drop old index in order to check potentially outdated transactions again.
        TreeSet(validatedIds: _*)
      } else {
        cleanupState.validatedIndex ++ validatedIds
      }

    CleanupState(newValidatedIndex, newEpochNr) -> invalidatedIds
  }

  /**
    *
    * A command to run (partial) memory pool cleanup
    *
    * @param validator - a state implementation which provides transaction validation
    * @param mempool - mempool reader instance
    */
  case class RunCleanup(validator: TransactionValidation, mempool: ErgoMemPoolReader)

}
