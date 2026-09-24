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
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.collection.mutable
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
    case RecheckMempool(st: UtxoStateReader, mp: ErgoMemPoolReader) =>
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

  private def initiateCleanup(validator: UtxoStateReader, mempool: ErgoMemPoolReader): Unit = {
    log.info("Initiating mempool cleanup")
    worker ! RunCleanup(validator, mempool)
    context become working // ignore other triggers until work is done
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
      val toBroadcast = MempoolAuditor.withAncestorsParentsFirst(
        pr.random(settings.nodeSettings.rebroadcastCount).toSeq, pr.getAllPrioritized)
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

  /**
    * How deep the ancestor walk in `withAncestorsParentsFirst` follows in-pool parents (the same bound the pool puts
    * on `updateFamily`).
    */
  private[local] val MaxAncestorDepth = 500

  /**
    * Adds the in-pool ancestors of the selected transactions and orders the result parents first. The rebroadcast
    * selection is a window of the pool, and a pooled transaction whose inputs are neither in the state nor in the
    * selection is not announced; its children, whose inputs the selection does hold, would then be announced
    * without it. A peer declines a transaction whose inputs it does not have yet and does not request it again for
    * a while, so those children stay out of the peer's pool (e.g. a chain of wallet payments returned to the pool
    * by a rollback). Selections with no in-pool parent are returned unchanged.
    */
  private[local] def withAncestorsParentsFirst(selected: Seq[UnconfirmedTransaction],
                                               pool: Seq[UnconfirmedTransaction]): Seq[UnconfirmedTransaction] = {
    val creatorOf: Map[ModifierId, UnconfirmedTransaction] =
      pool.flatMap(utx => utx.transaction.outputs.map(out => bytesToId(out.id) -> utx)).toMap
    def parentsOf(utx: UnconfirmedTransaction): Iterator[UnconfirmedTransaction] =
      utx.transaction.inputIds.flatMap(boxId => creatorOf.get(bytesToId(boxId))).distinct.iterator
    val ordered = mutable.LinkedHashMap[ModifierId, UnconfirmedTransaction]()
    // iterative post-order walk: a transaction is emitted after its in-pool parents, down to MaxAncestorDepth
    selected.foreach { root =>
      if (!ordered.contains(root.id)) {
        var stack: List[(UnconfirmedTransaction, Int, Iterator[UnconfirmedTransaction])] = List((root, 0, parentsOf(root)))
        val onStack = mutable.Set[ModifierId](root.id)
        while (stack.nonEmpty) {
          val (utx, depth, parents) = stack.head
          if (depth < MaxAncestorDepth && parents.hasNext) {
            val parent = parents.next()
            if (!ordered.contains(parent.id) && !onStack.contains(parent.id)) {
              onStack += parent.id
              stack = (parent, depth + 1, parentsOf(parent)) :: stack
            }
          } else {
            stack = stack.tail
            onStack -= utx.id
            ordered.put(utx.id, utx)
          }
        }
      }
    }
    ordered.values.toSeq
  }

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
