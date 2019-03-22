package org.ergoplatform.local

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorRefFactory, DeathPactException, OneForOneStrategy, Props}
import org.ergoplatform.local.CleanupWorker.RunCleanup
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.settings.NodeConfigurationSettings
import scorex.core.NodeViewHolder.ReceivableMessages.GetNodeViewChanges
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedMempool, ChangedState, SemanticallySuccessfulModifier}
import scorex.core.transaction.state.TransactionValidation
import scorex.util.ScorexLogging

import scala.concurrent.duration._

/**
  * Performs mempool validation in between blocks application.
  */
class MempoolAuditor(nodeViewHolderRef: ActorRef,
                     nodeSettings: NodeConfigurationSettings) extends Actor with ScorexLogging {

  override val supervisorStrategy: OneForOneStrategy = OneForOneStrategy(
    maxNrOfRetries = 5,
    withinTimeRange = 1.minute) {
    case _: ActorKilledException => Stop
    case _: DeathPactException => Stop
    case e: ActorInitializationException =>
      log.warn(s"Cleanup worker failed during initialization with: $e")
      Stop
    case e: Exception =>
      log.warn(s"Cleanup worker failed with: $e")
      context become awaiting // turn ctx into awaiting mode if worker failed
      Restart
  }

  private var stateReaderOpt: Option[TransactionValidation[ErgoTransaction]] = None
  private var poolReaderOpt: Option[ErgoMemPoolReader] = None

  private val worker: ActorRef = context.actorOf(Props(new CleanupWorker(nodeViewHolderRef, nodeSettings)))

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
  }

  override def receive: Receive = awaiting

  private def awaiting: Receive = {
    case SemanticallySuccessfulModifier(_: ErgoFullBlock) | SemanticallySuccessfulModifier(_: Header) =>
      stateReaderOpt = None
      poolReaderOpt = None
      nodeViewHolderRef ! GetNodeViewChanges(history = false, state = true, mempool = true, vault = false)

    case ChangedMempool(mp: ErgoMemPoolReader) =>
      stateReaderOpt.fold[Any](poolReaderOpt = Some(mp))(initiateCleanup(_, mp))

    case ChangedState(st: TransactionValidation[ErgoTransaction@unchecked]) =>
      poolReaderOpt.fold[Any](stateReaderOpt = Some(st))(initiateCleanup(st, _))

    case ChangedState(_) | ChangedMempool(_) => // do nothing
  }

  private def working: Receive = {
    case CleanupDone =>
      log.info("Cleanup done. Switching to awaiting mode")
      context become awaiting

    case _ => // ignore other triggers until work is done
  }

  private def initiateCleanup(validator: TransactionValidation[ErgoTransaction],
                              mempool: ErgoMemPoolReader): Unit = {
    log.info("Initiating cleanup. Switching to working mode")
    worker ! RunCleanup(validator, mempool)
    context become working // ignore other triggers until work is done
  }

}

object MempoolAuditor {
  case object CleanupDone
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
