package org.ergoplatform.local

import akka.actor.SupervisorStrategy.{Restart, Stop}
import akka.actor.{Actor, ActorInitializationException, ActorKilledException, ActorRef, ActorRefFactory, DeathPactException, OneForOneStrategy, Props}
import org.ergoplatform.ErgoBox.BoxId
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
      val limit = MempoolAuditor.roundLimit(settings.nodeSettings.rebroadcastCount)
      stateReaderOpt match {
        case Some(utxoState: UtxoStateReader) =>
          // toBroadcast is parents first: an input or data input counts if its box is in the state or is created by
          // a transaction already announced in this round, so no transaction goes out without its in-pool parents,
          // and stopping the round at `limit` announcements cannot separate a child from its parent
          val announcedOutputs = mutable.Set[ModifierId]()
          def inPlace(boxId: BoxId): Boolean =
            announcedOutputs.contains(bytesToId(boxId)) || utxoState.boxById(boxId).isDefined
          val skipped = mutable.ArrayBuffer[ModifierId]()
          var announced = 0
          val candidates = toBroadcast.iterator
          while (announced < limit && candidates.hasNext) {
            val unconfirmedTx = candidates.next()
            val tx = unconfirmedTx.transaction
            if (tx.inputIds.forall(inPlace) && tx.dataInputs.forall(dataInput => inPlace(dataInput.boxId))) {
              log.info(s"Rebroadcasting $unconfirmedTx")
              broadcastTx(unconfirmedTx)
              announcedOutputs ++= tx.outputs.map(out => bytesToId(out.id))
              announced += 1
            } else {
              log.debug(s"Not rebroadcasting $unconfirmedTx as not all the inputs and data inputs are in place")
              skipped += unconfirmedTx.id
            }
          }
          if (skipped.nonEmpty) {
            log.info(s"Not rebroadcasting ${skipped.size} transactions as not all their inputs and data inputs are in " +
              s"the state or announced in this round, the first of them ${skipped.head}")
          }
          if (candidates.hasNext) {
            log.info(s"Rebroadcast round stopped after $limit announced transactions, " +
              s"${candidates.size} of ${toBroadcast.size} candidates not considered in this round")
          }
        case _ =>
          if (toBroadcast.size > limit) {
            log.info(s"Rebroadcast round limited to $limit of ${toBroadcast.size} candidates")
          }
          toBroadcast.take(limit).foreach { unconfirmedTx =>
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
    * How many transactions one rebroadcast round announces at most, unless `rebroadcastCount` is set higher (see
    * `roundLimit`). With the in-pool ancestors added, a round could otherwise announce the whole pool, each id in its
    * own `Inv` to every peer. 100 lets a chain of up to 100 pooled transactions go out in one round, against 3
    * selected by default. The round list is parents first and a transaction is announced only after its in-pool
    * parents, so stopping early never announces a child without its parent; the rest of a longer chain goes out in
    * a later round that selects one of its transactions, once its front is confirmed.
    */
  private[local] val MaxAnnouncedPerRound = 100

  /**
    * The most transactions a round announces: `MaxAnnouncedPerRound`, or `rebroadcastCount` if that is larger, so the
    * limit never cuts a round below the configured selection size.
    */
  private[local] def roundLimit(rebroadcastCount: Int): Int = math.max(rebroadcastCount, MaxAnnouncedPerRound)

  /**
    * Adds the in-pool ancestors of the selected transactions and orders the result parents first. A transaction's
    * in-pool parents are the pooled transactions that create its inputs and its data inputs. The rebroadcast
    * selection is a window of the pool, and a pooled transaction whose inputs are neither in the state nor in the
    * selection is not announced; its children, whose inputs the selection does hold, would then be announced
    * without it. A peer declines a transaction whose inputs it does not have yet and requests it again only when it
    * is announced again after the next block, so such a chain (e.g. wallet payments returned to the pool by a
    * rollback) reaches the peer a few transactions per round at best; a peer that lacks a data input's creator does
    * not decline the transaction but rejects it. Selections with no in-pool parent are returned unchanged.
    */
  private[local] def withAncestorsParentsFirst(selected: Seq[UnconfirmedTransaction],
                                               pool: Seq[UnconfirmedTransaction]): Seq[UnconfirmedTransaction] = {
    val creatorOf: Map[ModifierId, UnconfirmedTransaction] =
      pool.flatMap(utx => utx.transaction.outputs.map(out => bytesToId(out.id) -> utx)).toMap
    def parentsOf(utx: UnconfirmedTransaction): Iterator[UnconfirmedTransaction] =
      (utx.transaction.inputIds ++ utx.transaction.dataInputs.map(_.boxId))
        .flatMap(boxId => creatorOf.get(bytesToId(boxId))).distinct.iterator
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
