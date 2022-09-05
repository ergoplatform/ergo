package org.ergoplatform.local

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorRefFactory, DeathPactException, OneForOneStrategy, Props}
import org.ergoplatform.local.CleanupWorker.RunCleanup
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.modifiers.mempool.UnconfirmedTransaction
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.settings.ErgoSettings
import scorex.core.network.Broadcast
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages.{RecheckMempool}
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoStateReader}
import scorex.core.network.message.{InvData, InvSpec, Message}
import scorex.core.transaction.Transaction
import scorex.core.transaction.state.TransactionValidation
import scorex.util.ScorexLogging

import scala.concurrent.duration._

/**
  * Controls mempool cleanup workflow. Watches NodeView events and delegates
  * mempool cleanup task to [[CleanupWorker]] when needed.
  */
class MempoolAuditor(nodeViewHolderRef: ActorRef,
                     networkControllerRef: ActorRef,
                     settings: ErgoSettings) extends Actor with ScorexLogging {

  override def postRestart(reason: Throwable): Unit = {
    log.error(s"Mempool auditor actor restarted due to ${reason.getMessage}", reason)
    super.postRestart(reason)
  }

  override def postStop(): Unit = {
    log.info("Mempool auditor stopped")
    super.postStop()
  }

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

  private var poolReaderOpt: Option[ErgoMemPoolReader] = None
  private var stateReaderOpt: Option[ErgoStateReader] = None

  private val worker: ActorRef =
    context.actorOf(Props(new CleanupWorker(nodeViewHolderRef, settings.nodeSettings)))

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[RecheckMempool])
  }

  override def receive: Receive = awaiting

  private def awaiting: Receive = {
    case RecheckMempool(st: TransactionValidation, mp: ErgoMemPoolReader) =>
      stateReaderOpt = Some(st)
      poolReaderOpt = Some(mp)
      initiateCleanup(st, mp)
  }

  private def working: Receive = {
    case CleanupDone =>
      log.info("Cleanup done. Switching to awaiting mode")
      //rebroadcast transactions
      rebroadcastTransactions()
      context become awaiting

    case _ => // ignore other triggers until work is done
  }

  private def initiateCleanup(validator: TransactionValidation, mempool: ErgoMemPoolReader): Unit = {
    log.info("Initiating cleanup. Switching to working mode")
    worker ! RunCleanup(validator, mempool)
    context become working // ignore other triggers until work is done
  }

  private def broadcastTx(unconfirmedTx: UnconfirmedTransaction): Unit = {
    val msg = Message(
      InvSpec,
      Right(InvData(Transaction.ModifierTypeId, Seq(unconfirmedTx.id))),
      None
    )
    networkControllerRef ! SendToNetwork(msg, Broadcast)
  }

  private def rebroadcastTransactions(): Unit = {
    log.debug("Rebroadcasting transactions")
    poolReaderOpt.foreach { pr =>
      val toBroadcast = pr.random(settings.nodeSettings.rebroadcastCount).toSeq
      stateReaderOpt match {
        case Some(utxoState: UtxoStateReader) =>
          val stateToCheck = utxoState.withUnconfirmedTransactions(toBroadcast)
          toBroadcast.foreach { unconfirmedTx =>
            if (unconfirmedTx.transaction.inputIds.forall(inputBoxId => stateToCheck.boxById(inputBoxId).isDefined)) {
              log.info(s"Rebroadcasting $unconfirmedTx")
              broadcastTx(unconfirmedTx)
            } else {
              log.info(s"Not rebroadcasting $unconfirmedTx as not all the inputs are in place")
            }
          }
        case _ =>
          toBroadcast.foreach { unconfirmedTx =>
            log.warn(s"Rebroadcasting $unconfirmedTx while state is not ready or not UTXO set")
            broadcastTx(unconfirmedTx)
          }
      }
    }
  }
}

object MempoolAuditor {

  case object CleanupDone

}

object MempoolAuditorRef {

  def props(nodeViewHolderRef: ActorRef,
            networkControllerRef: ActorRef,
            settings: ErgoSettings): Props =
    Props(new MempoolAuditor(nodeViewHolderRef, networkControllerRef, settings))

  def apply(nodeViewHolderRef: ActorRef,
            networkControllerRef: ActorRef,
            settings: ErgoSettings)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(nodeViewHolderRef, networkControllerRef, settings))

}
