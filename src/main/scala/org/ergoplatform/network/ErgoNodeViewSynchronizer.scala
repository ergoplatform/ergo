package org.ergoplatform.network

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.network.ErgoNodeViewSynchronizer.CheckModifiersToDownload
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import scorex.core.NodeViewHolder.ReceivableMessages.Subscribe
import scorex.core.NodeViewHolder._
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SyntacticallySuccessfulModifier
import scorex.core.network.message.Message
import scorex.core.network.{NodeViewSynchronizer, SendToRandom}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, ModifierTypeId, NodeViewHolder}

import scala.concurrent.ExecutionContext.Implicits.global

class ErgoNodeViewSynchronizer(networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               localInterfaceRef: ActorRef,
                               syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                               networkSettings: NetworkSettings,
                               timeProvider: NetworkTimeProvider)
  extends NodeViewSynchronizer[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction,
    ErgoSyncInfo, ErgoSyncInfoMessageSpec.type, ErgoPersistentModifier, ErgoHistory,
    ErgoMemPool](networkControllerRef, viewHolderRef, localInterfaceRef,
    syncInfoSpec, networkSettings, timeProvider) {

  override protected val deliveryTracker = new ErgoDeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, self,
    timeProvider)

  private val downloadListSize = networkSettings.networkChunkSize

  override def preStart(): Unit = {
    val toDownloadCheckInterval = networkSettings.syncInterval
    super.preStart()
    viewHolderRef ! Subscribe(Seq(NodeViewHolder.EventType.DownloadNeeded))
    context.system.scheduler.schedule(toDownloadCheckInterval, toDownloadCheckInterval)(self ! CheckModifiersToDownload)
  }

  def requestDownload(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    deliveryTracker.expectFromRandom(modifierTypeId, modifierId)
    val msg = Message(requestModifierSpec, Right(modifierTypeId -> Seq(modifierId)), None)
    //todo: Full nodes should be here, not a random peer
    networkControllerRef ! SendToNetwork(msg, SendToRandom)
  }

  protected val onSyntacticallySuccessfulModifier: Receive = {
    case SyntacticallySuccessfulModifier(mod) if (mod.isInstanceOf[Header] || mod.isInstanceOf[BlockTransactions]) &&
      historyReaderOpt.exists(_.isHeadersChainSynced) =>

      broadcastModifierInv(mod)
  }

  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      deliveryTracker.removeOutdatedExpectingFromRandom(historyReaderOpt)
      historyReaderOpt.foreach { h =>
        val currentQueue = deliveryTracker.expectingFromRandomQueue
        val newIds = h.nextModifiersToDownload(downloadListSize - currentQueue.size, currentQueue)
        val oldIds = deliveryTracker.idsExpectingFromRandomToRetry()
        (newIds ++ oldIds).foreach(id => requestDownload(id._1, id._2))
      }
  }

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      requestDownload(modifierTypeId, modifierId)
  }

  override protected def viewHolderEvents: Receive =
    onSyntacticallySuccessfulModifier orElse
      onDownloadRequest orElse
      onCheckModifiersToDownload orElse
      super.viewHolderEvents
}

object ErgoNodeViewSynchronizer {
  def props(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            localInterfaceRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider): Props =
    Props(new ErgoNodeViewSynchronizer(networkControllerRef, viewHolderRef, localInterfaceRef,
      syncInfoSpec, networkSettings, timeProvider))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            localInterfaceRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, localInterfaceRef,
      syncInfoSpec, networkSettings, timeProvider))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            localInterfaceRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider,
            name: String)
           (implicit context: ActorRefFactory): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, localInterfaceRef,
      syncInfoSpec, networkSettings, timeProvider), name)


  case object CheckModifiersToDownload

}
