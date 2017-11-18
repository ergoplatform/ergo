package org.ergoplatform.network

import akka.actor.ActorRef
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.network.ErgoNodeViewSynchronizer.CheckModifiersToDownload
import org.ergoplatform.nodeView.history.{ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import scorex.core.NodeViewHolder._
import scorex.core.network.NetworkController.SendToNetwork
import scorex.core.network.message.Message
import scorex.core.network.{NodeViewSynchronizer, SendToRandom}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.NetworkTime
import scorex.core.{ModifierId, ModifierTypeId, NodeViewHolder}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class ErgoNodeViewSynchronizer(networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               localInterfaceRef: ActorRef,
                               syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                               networkSettings: NetworkSettings) extends NodeViewSynchronizer[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction,
  ErgoSyncInfo, ErgoSyncInfoMessageSpec.type](networkControllerRef, viewHolderRef, localInterfaceRef,
  syncInfoSpec, networkSettings) {
  override protected val deliveryTracker = new ErgoDeliveryTracker

  private val toDownloadCheckInterval = 5.seconds

  override def preStart(): Unit = {
    viewHolderRef ! Subscribe(Seq(NodeViewHolder.EventType.DownloadNeeded))
    super.preStart()
    context.system.scheduler.schedule(toDownloadCheckInterval, toDownloadCheckInterval)(self ! CheckModifiersToDownload)
  }

  protected val onSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(mod: ErgoFullBlock) =>
      mod.toSeq.foreach(m => broadcastModifierInv(m))
    case SemanticallySuccessfulModifier(mod) =>
      broadcastModifierInv(mod)
  }

  override protected def viewHolderEvents: Receive = onSemanticallySuccessfulModifier orElse onDownloadRequest orElse
    super.viewHolderEvents

  def onDownloadRequest: Receive = {
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      requestDownload(modifierTypeId, modifierId)
  }

  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      val currentTime = NetworkTime.time()
      val outdatedIds = deliveryTracker.toDownload.filter(_._2._2 < currentTime - toDownloadCheckInterval.toMillis)
      outdatedIds.foreach(i => requestDownload(i._2._1, ModifierId @@ i._1.array))
  }


  def requestDownload(modifierTypeId: ModifierTypeId, modifierId: ModifierId): Unit = {
    val msg = Message(requestModifierSpec, Right(modifierTypeId -> Seq(modifierId)), None)
    //Full nodes should be here, not a random peer
    networkControllerRef ! SendToNetwork(msg, SendToRandom)
    deliveryTracker.downloadRequested(modifierTypeId, modifierId)
  }
}

object ErgoNodeViewSynchronizer {

  case object CheckModifiersToDownload

}