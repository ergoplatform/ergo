package org.ergoplatform.network

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.CheckModifiersToDownload
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.{Constants, ErgoSettings}
import scorex.core.NodeViewHolder._
import scorex.core.{ModifierTypeId, PersistentNodeViewModifier}
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
import scorex.core.network.message.{InvData, Message}
import scorex.core.network.{ConnectedPeer, ModifiersStatus, NodeViewSynchronizer, SendToRandom}
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.Transaction
import scorex.core.utils.NetworkTimeProvider
import scorex.util.ModifierId

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
  * Tweaks on top of Scorex' NodeViewSynchronizer made for optimizing Ergo network
  */
class ErgoNodeViewSynchronizer(networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                               settings: ErgoSettings,
                               timeProvider: NetworkTimeProvider)
                              (implicit ex: ExecutionContext)
  extends NodeViewSynchronizer[ErgoTransaction, ErgoSyncInfo, ErgoSyncInfoMessageSpec.type, ErgoPersistentModifier,
    ErgoHistory, ErgoMemPool](networkControllerRef, viewHolderRef, syncInfoSpec,
    settings.scorexSettings.network, timeProvider, Constants.modifierSerializers) {

  override protected val deliveryTracker =
    new ErgoDeliveryTracker(context.system, deliveryTimeout, maxDeliveryChecks, self, timeProvider)

  protected val networkSettings: NetworkSettings = settings.scorexSettings.network

  /**
    * Approximate number of modifiers to be downloaded simultaneously
    */
  protected val desiredSizeOfExpectingQueue: Int = networkSettings.desiredInvObjects

  override def preStart(): Unit = {
    val toDownloadCheckInterval = networkSettings.syncInterval
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.scheduler.schedule(toDownloadCheckInterval, toDownloadCheckInterval)(self ! CheckModifiersToDownload)
  }

  /**
    * Requests BlockSections with `Unknown` status that are defined by block headers but not downloaded yet.
    * Trying to keep size of requested queue equals to `desiredSizeOfExpectingQueue`.
    */
  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      historyReaderOpt.foreach { h =>
        def downloadRequired(id: ModifierId): Boolean = deliveryTracker.status(id, Seq(h)) == ModifiersStatus.Unknown

        val toDownload =
          h.nextModifiersToDownload(desiredSizeOfExpectingQueue - deliveryTracker.requestedSize, downloadRequired)

        log.info(s"${toDownload.length} persistent modifiers to be downloaded")

        toDownload.groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
  }

  // todo: this method is just a copy of the ancestor from Scorex, however, smarter logic is needed, not just
  //  asking from a random peer
  override protected def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    deliveryTracker.setRequested(modifierIds, modifierTypeId, None)
    val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, modifierIds)), None)
    networkControllerRef ! SendToNetwork(msg, SendToRandom)
  }

  /**
    * Object ids coming from other node.
    * Filter out modifier ids that are already in process (requested, received or applied),
    * request unknown ids from peer and set this ids to requested state.
    */
  override protected def processInv(invData: InvData, peer: ConnectedPeer): Unit = {
    (mempoolReaderOpt, historyReaderOpt) match {
      case (Some(mempool), Some(history)) =>

        val modifierTypeId = invData.typeId

        val newModifierIds = modifierTypeId match {
          case Transaction.ModifierTypeId =>
            // We download transactions only if the node is not needed for externally provided proofs
            // (so having UTXO set, and the chain is synced
            if (!settings.nodeSettings.stateType.requireProofs &&
              history.isHeadersChainSynced &&
              history.fullBlockHeight == history.headersHeight) {
              invData.ids.filter(mid => deliveryTracker.status(mid, mempool) == ModifiersStatus.Unknown)
            } else {
              Seq.empty
            }
          case _ =>
            invData.ids.filter(mid => deliveryTracker.status(mid, history) == ModifiersStatus.Unknown)
        }

        if (newModifierIds.nonEmpty) {
          val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, newModifierIds)), None)
          peer.handlerRef ! msg
          deliveryTracker.setRequested(newModifierIds, modifierTypeId, Some(peer))
        }

      case _ =>
        log.warn(s"Got data from peer while readers are not ready ${(mempoolReaderOpt, historyReaderOpt)}")
    }
  }

  /**
    * If our requested list is more than half empty, enforce to request more:
    * - headers, if our headers chain is not synced yet (by sending sync message)
    * - block sections, if our headers chain is synced
    */
  override protected def requestMoreModifiers(applied: Seq[ErgoPersistentModifier]): Unit = {
    super.requestMoreModifiers(applied)
    if (deliveryTracker.requestedSize < desiredSizeOfExpectingQueue / 2) {
      historyReaderOpt foreach { h =>
        if (h.isHeadersChainSynced) {
          // our requested list is is half empty - request more missed modifiers
          self ! CheckModifiersToDownload
        } else {
          // headers chain is not synced yet, but our requested list is half empty - ask for more headers
          sendSync(statusTracker, h)
        }
      }
    }
  }

  /**
    * If new enough semantically valid ErgoFullBlock was applied, send inv for block header and all its sections
    */
  private val onSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(mod) =>
      broadcastInvForNewModifier(mod)
  }

  protected def broadcastInvForNewModifier(mod: PersistentNodeViewModifier): Unit = {
    mod match {
      case fb: ErgoFullBlock if fb.header.isNew(timeProvider, 1.hour) => fb.toSeq.foreach(s => broadcastModifierInv(s))
      case _ =>
    }
  }

  override protected def viewHolderEvents: Receive =
    onSemanticallySuccessfulModifier orElse
      onCheckModifiersToDownload orElse
      super.viewHolderEvents
}

object ErgoNodeViewSynchronizer {

  def props(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider)
           (implicit ex: ExecutionContext): Props =
    Props(new ErgoNodeViewSynchronizer(networkControllerRef, viewHolderRef, syncInfoSpec, settings,
      timeProvider))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, settings, timeProvider))

  case object CheckModifiersToDownload

}
