package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.core.NodeViewHolder.ReceivableMessages.EliminateTransactions
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.transaction.state.TransactionValidation
import scorex.util.{ModifierId, ScorexLogging}

import scala.annotation.tailrec
import scala.util.{Random, Success}

/**
  * Performs mempool validation in between blocks application.
  */
class MempoolAuditor(nodeViewHolderRef: ActorRef,
                     readersHolderRef: ActorRef,
                     nodeSettings: NodeConfigurationSettings) extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = {
    case SemanticallySuccessfulModifier(_: ErgoFullBlock) | SemanticallySuccessfulModifier(_: Header) =>
      readersHolderRef ! GetReaders
    case Readers(_, st: TransactionValidation[ErgoTransaction@unchecked], mp, _) =>
      val toEliminate = validatePool(st, mp)
      if (toEliminate.nonEmpty) {
        log.info(s"${toEliminate.size} transactions from mempool were invalidated")
        nodeViewHolderRef ! EliminateTransactions(toEliminate)
      }
    case _: Readers => // do nothing
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
            readersHolderRef: ActorRef,
            nodeSettings: NodeConfigurationSettings): Props =
    Props(new MempoolAuditor(nodeViewHolderRef, readersHolderRef, nodeSettings))

  def apply(nodeViewHolderRef: ActorRef,
            readersHolderRef: ActorRef,
            nodeSettings: NodeConfigurationSettings)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(nodeViewHolderRef, readersHolderRef, nodeSettings))
}
