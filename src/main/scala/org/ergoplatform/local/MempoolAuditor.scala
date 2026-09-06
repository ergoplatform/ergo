package org.ergoplatform.local

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorRefFactory, DeathPactException, OneForOneStrategy, Props}
import org.ergoplatform.local.CleanupWorker.RunCleanup
import org.ergoplatform.local.MempoolAuditor.CleanupDone
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.ErgoMemPoolReader
import org.ergoplatform.settings.ErgoSettings
import scorex.core.network.Broadcast
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages.RecheckMempool
import org.ergoplatform.nodeView.state.{ErgoStateReader, UtxoStateReader}
import org.ergoplatform.network.message.{InvData, InvSpec, Message}
import org.ergoplatform.nodeView.ErgoNodeViewHolder.ReceivableMessages.RecheckedTransactions
import scorex.util.ScorexLogging

import java.util.UUID
import scala.concurrent.duration._
import scala.util.Try

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
  private var pending: Option[RecheckMempool] = None

  private val worker: ActorRef =
    context.actorOf(Props(new CleanupWorker(settings.nodeSettings)))

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[RecheckMempool])
  }

  override def receive: Receive = awaiting

  private def awaiting: Receive = {
    case request: RecheckMempool => initiateCleanup(request)
    case _: CleanupDone => // a completion from an obsolete worker run
  }

  private def working(jobId: UUID): Receive = {
    case CleanupDone(`jobId`, result) if sender() == worker =>
      result.foreach(nodeViewHolderRef ! _)
      context become awaiting
      pending match {
        case Some(request) => initiateCleanup(request)
        case None => result.foreach { _ => rebroadcastTransactions() }
      }
    case request: RecheckMempool => pending = Some(request)
    case _: CleanupDone => // a stale completion must not release the current job
  }

  private def initiateCleanup(request: RecheckMempool): Unit = {
    log.info("Initiating mempool cleanup")
    stateReaderOpt = Some(request.state)
    poolReaderOpt = Some(request.mempool)
    pending = None
    val jobId = UUID.randomUUID()
    worker ! RunCleanup(jobId, request)
    context become working(jobId)
  }

  private def broadcastTx(unconfirmedTx: UnconfirmedTransaction): Unit = {
    val msg = Message(
      InvSpec,
      Right(InvData(ErgoTransaction.modifierTypeId, Seq(unconfirmedTx.id))),
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

  case class CleanupDone(jobId: UUID, result: Try[RecheckedTransactions])

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
