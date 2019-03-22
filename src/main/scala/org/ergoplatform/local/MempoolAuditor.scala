package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.core.NodeViewHolder.ReceivableMessages.{EliminateTransactions, GetNodeViewChanges}
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedMempool, ChangedState, SemanticallySuccessfulModifier}
import scorex.core.transaction.state.TransactionValidation
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.util.{Random, Success}

/**
  * Performs mempool validation in between blocks application.
  */
class MempoolAuditor(nodeViewHolderRef: ActorRef,
                     nodeSettings: NodeConfigurationSettings) extends Actor with ScorexLogging {

  private var stateReaderOpt: Option[TransactionValidation[ErgoTransaction]] = None
  private var poolReaderOpt: Option[ErgoMemPoolReader] = None

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(_: ErgoFullBlock) | SemanticallySuccessfulModifier(_: Header) =>
      stateReaderOpt = None
      poolReaderOpt = None
      nodeViewHolderRef ! GetNodeViewChanges(history = false, state = true, mempool = true, vault = false)

    case ChangedMempool(mp: ErgoMemPoolReader) =>
      stateReaderOpt.fold[Any](poolReaderOpt = Some(mp))(runCleanup(_, mp))

    case ChangedState(st: TransactionValidation[ErgoTransaction@unchecked]) =>
      poolReaderOpt.fold[Any](stateReaderOpt = Some(st))(runCleanup(st, _))

    case ChangedState(_) | ChangedMempool(_) => // do nothing
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
    */
  private def validatePool(validator: TransactionValidation[ErgoTransaction],
                           mempool: ErgoMemPoolReader): Seq[ModifierId] = {
    @tailrec
    def validationLoop(txs: List[ErgoTransaction],
                       invalidated: Seq[ModifierId],
                       etAcc: Long): Seq[ModifierId] = txs match {
      case head :: tail if etAcc < nodeSettings.mempoolCleanupDuration.toNanos =>
        val t0 = System.nanoTime()
        val validationResult = validator.validate(head)
        val t1 = System.nanoTime()
        val accumulatedTime = etAcc + (t1 - t0)
        validationResult match {
          case Success(_) => validationLoop(tail, invalidated, accumulatedTime)
          case _ => validationLoop(tail, invalidated :+ head.id, accumulatedTime)
        }
      case _ =>
        invalidated
    }
    validationLoop(Random.shuffle(mempool.getAll.toList), Seq.empty, 0L)
  }

}

object MempoolAuditorRef {

  def props(nodeViewHolderRef: ActorRef,
            nodeSettings: NodeConfigurationSettings): Props =
    Props(new MempoolAuditor(nodeViewHolderRef, nodeSettings))

  def apply(nodeViewHolderRef: ActorRef,
            nodeSettings: NodeConfigurationSettings)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(nodeViewHolderRef, nodeSettings))
}
