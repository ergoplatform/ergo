package org.ergoplatform.network

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.network.ErgoNodeViewSynchronizer.{CheckModifiersToDownload, MissedModifiers}
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.UtxoState
import org.ergoplatform.nodeView.wallet.ErgoWallet
import scorex.core.NodeViewHolder._
import scorex.core.network.NetworkController.SendToNetwork
import scorex.core.network.message.Message
import scorex.core.network.{NodeViewSynchronizer, SendToRandom}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, ModifierTypeId, NodeViewHolder}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

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

  //todo: move to config
  private val toDownloadCheckInterval = 3.seconds

  override def preStart(): Unit = {
    super.preStart()
    viewHolderRef ! Subscribe(Seq(NodeViewHolder.EventType.DownloadNeeded))
    context.system.scheduler.schedule(toDownloadCheckInterval, toDownloadCheckInterval)(self ! CheckModifiersToDownload)
    initializeToDownload()
  }

  protected def initializeToDownload(): Unit = {
    viewHolderRef ! GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, MissedModifiers] { v =>
      MissedModifiers(v.history.missedModifiersForFullChain())
    }
  }

  def requestDownload(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val msg = Message(requestModifierSpec, Right(modifierTypeId -> Seq(modifierId)), None)
    //todo: Full nodes should be here, not a random peer
    networkControllerRef ! SendToNetwork(msg, SendToRandom)
    deliveryTracker.downloadRequested(modifierTypeId, modifierId)
  }

  protected def onMissedModifiers(): Receive = {
    case MissedModifiers(ids) =>
      log.info(s"Initialize toDownload with ${ids.length} ids: ${scorex.core.idsToString(ids)}")
      ids.foreach { id => requestDownload(id._1, id._2) }
  }

  protected val onSyntacticallySuccessfulModifier: Receive = {
    case SyntacticallySuccessfulModifier(mod: Header@unchecked) if mod.isInstanceOf[Header] =>
      broadcastModifierInv(mod)
  }

  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      val modifiersToDownloadNow = deliveryTracker.downloadRetry(historyReaderOpt)
      if (modifiersToDownloadNow.nonEmpty) log.debug(s"Going to request ${modifiersToDownloadNow.size} of " +
        s"${deliveryTracker.toDownload.size} missed modifiers")
      modifiersToDownloadNow.foreach(i => requestDownload(i._2.tp, i._1))
  }

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      requestDownload(modifierTypeId, modifierId)
  }

  override protected def viewHolderEvents: Receive =
    onSyntacticallySuccessfulModifier orElse
      onDownloadRequest orElse
      onCheckModifiersToDownload orElse
      onMissedModifiers orElse
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

  case class MissedModifiers(m: Seq[(ModifierTypeId, ModifierId)])

}
