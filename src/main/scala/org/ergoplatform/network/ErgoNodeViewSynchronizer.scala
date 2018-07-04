package org.ergoplatform.network

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.{BlockTransactions, Header}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.network.ErgoNodeViewSynchronizer.CheckModifiersToDownload
import org.ergoplatform.nodeView.ErgoModifiersCache
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import scorex.core.NodeViewHolder._
import scorex.core.network.NetworkController.ReceivableMessages.SendToNetwork
import scorex.core.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{ChangedVault, SyntacticallySuccessfulModifier}
import scorex.core.network.message.BasicMsgDataTypes.ModifiersData
import scorex.core.network.message.{Message, ModifiersSpec}
import scorex.core.network.{NodeViewSynchronizer, SendToRandom}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, ModifierTypeId}

import scala.collection.mutable
import scala.concurrent.ExecutionContext

class ErgoNodeViewSynchronizer(networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                               networkSettings: NetworkSettings,
                               timeProvider: NetworkTimeProvider,
                               modifiersCache: ErgoModifiersCache)(implicit ex: ExecutionContext)
  extends NodeViewSynchronizer[ErgoTransaction,
    ErgoSyncInfo, ErgoSyncInfoMessageSpec.type, ErgoPersistentModifier, ErgoHistory,
    ErgoMemPool](networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider) {

  override protected val deliveryTracker = new ErgoDeliveryTracker(context, deliveryTimeout, maxDeliveryChecks, self,
    timeProvider)

  private val downloadListSize = networkSettings.networkChunkSize

  override def preStart(): Unit = {
    val toDownloadCheckInterval = networkSettings.syncInterval
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.scheduler.schedule(toDownloadCheckInterval, toDownloadCheckInterval)(self ! CheckModifiersToDownload)
  }

  private def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    modifierIds.foreach(id => deliveryTracker.expectFromRandom(modifierTypeId, id))
    val msg = Message(requestModifierSpec, Right(modifierTypeId -> modifierIds), None)
    //todo: Full nodes should be here, not a random peer
    networkControllerRef ! SendToNetwork(msg, SendToRandom)
  }

  override protected def modifiersFromRemote: Receive = {
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote) if spec.messageCode == ModifiersSpec.messageCode =>
      super.modifiersFromRemote(DataFromPeer(spec, data, remote))
      //If queue is empty - check, whether there are more modifiers to download
      historyReaderOpt foreach { h =>
        if (!h.isHeadersChainSynced && !deliveryTracker.isExpecting) {
          // headers chain is not synced yet, but our expecting list is empty - ask for more headers
          sendSync(statusTracker, h)
        } else if (h.isHeadersChainSynced && !deliveryTracker.isExpectingFromRandom) {
          // headers chain is synced, but our full block list is empty - request more full blocks
          self ! CheckModifiersToDownload
        }
      }
  }

  /**
    * Broadcast inv on successful Header and BlockTransactions application
    * Do not broadcast Inv messages during initial synchronization (the rest of the network should already have all
    * this messages)
    *
    */
  protected val onSyntacticallySuccessfulModifier: Receive = {
    case SyntacticallySuccessfulModifier(mod) if (mod.isInstanceOf[Header] || mod.isInstanceOf[BlockTransactions]) &&
      historyReaderOpt.exists(_.isHeadersChainSynced) =>

      broadcastModifierInv(mod)
  }

  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      deliveryTracker.removeOutdatedExpectingFromRandom()
      historyReaderOpt.foreach { h =>
        val currentQueue = deliveryTracker.expectingFromRandomQueue.map(a => a: mutable.WrappedArray[Byte])
        val currentCache = modifiersCache.keySet
        val excluding = currentCache ++ currentQueue
        val newIds = h.missedModifiersForFullChain(downloadListSize - excluding.size, excluding)
        val oldIds = deliveryTracker.idsExpectingFromRandomToRetry()
        (newIds ++ oldIds).groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
  }

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      requestDownload(modifierTypeId, Seq(modifierId))
  }

  def onChangedVault: Receive = {
    case _: ChangedVault =>
  }

  override protected def viewHolderEvents: Receive =
    onSyntacticallySuccessfulModifier orElse
      onDownloadRequest orElse
      onCheckModifiersToDownload orElse
      onChangedVault orElse
      super.viewHolderEvents
}

object ErgoNodeViewSynchronizer {
  def props(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider,
            cache: ErgoModifiersCache)
           (implicit ex: ExecutionContext): Props =
    Props(new ErgoNodeViewSynchronizer(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings,
      timeProvider, cache))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider,
            cache: ErgoModifiersCache)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider, cache))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider,
            cache: ErgoModifiersCache,
            name: String)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider, cache), name)


  case object CheckModifiersToDownload

}
