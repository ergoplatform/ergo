package org.ergoplatform.network

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.CheckModifiersToDownload
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.Constants
import scorex.core.NodeViewHolder._
import scorex.core.consensus.History.Younger
import scorex.core.network.NetworkControllerSharedMessages.ReceivableMessages.DataFromPeer
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{OtherNodeSyncingStatus, SemanticallySuccessfulModifier}
import scorex.core.network.message.{InvData, InvSpec, Message}
import scorex.core.network.{ModifiersStatus, NodeViewSynchronizer}
import scorex.core.settings.NetworkSettings
import scorex.core.transaction.Transaction
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{PersistentNodeViewModifier, idsToString}
import scorex.util.ModifierId

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class ErgoNodeViewSynchronizer(networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                               networkSettings: NetworkSettings,
                               timeProvider: NetworkTimeProvider)
                              (implicit ec: ExecutionContext)
  extends NodeViewSynchronizer[ErgoTransaction, ErgoSyncInfo, ErgoSyncInfoMessageSpec.type, ErgoPersistentModifier,
    ErgoHistory, ErgoMemPool](networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider,
    Constants.modifierSerializers) {

  override protected val deliveryTracker = new ErgoDeliveryTracker(
    context.system, deliveryTimeout, maxDeliveryChecks, self, timeProvider)

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
      historyReaderOpt.foreach { hr =>
        def downloadRequired(id: ModifierId): Boolean = deliveryTracker.status(id, Seq(hr)) == ModifiersStatus.Unknown

        hr.nextModifiersToDownload(desiredSizeOfExpectingQueue - deliveryTracker.requestedSize, downloadRequired)
          .groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
  }

  /**
    * If our requested list is more than half empty, enforce to request more:
    * - headers, if our headers chain is not synced yet (by sending sync message)
    * - block sections, if our headers chain is synced
    */
  override protected def requestMoreModifiers(applied: Seq[ErgoPersistentModifier]): Unit =
    if (deliveryTracker.requestedSize < desiredSizeOfExpectingQueue / 2) {
      historyReaderOpt foreach { h =>
        if (h.isHeadersChainSynced) {
          // our requested list is half empty - request more missed modifiers
          self ! CheckModifiersToDownload
        } else {
          // headers chain is not synced yet, but our requested list is half empty - ask for more headers
          sendSync(statusTracker, h)
        }
      }
    }

  override protected def processSync: Receive = {
    case DataFromPeer(spec, syncInfo: ErgoSyncInfo, remote)
      if spec.messageCode == syncInfoSpec.messageCode =>

      (historyReaderOpt, syncInfo.poPowParamsOpt) match {
        case (Some(historyReader), Some(poPowParams)) =>
          historyReader.prove(poPowParams).foreach { proof =>
            val ext = Seq[ErgoPersistentModifier](proof.prefix, proof.suffix).map(x => x.modifierTypeId -> x.id)
            self ! OtherNodeSyncingStatus(remote, Younger, ext)
          }
        case (Some(historyReader), _) =>
          val ext = historyReader.continuationIds(syncInfo, networkSettings.desiredInvObjects)
          val comparison = historyReader.compare(syncInfo)
          log.debug(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}")
          log.debug(s"Extension ids: ${idsToString(ext)}")

          if (!(ext.nonEmpty || comparison != Younger)) log.warn("Extension is empty while comparison is younger")

          self ! OtherNodeSyncingStatus(remote, comparison, ext)
        case _ =>
      }
  }

  override protected def processInv: Receive = {
    case DataFromPeer(spec, invData: InvData, peer)
      if spec.messageCode == InvSpec.MessageCode =>

      (mempoolReaderOpt, historyReaderOpt) match {
        case (Some(mempool), Some(history)) =>
          val modifierTypeId = invData.typeId
          val acceptedModifierIds = modifierTypeId match {
            case Transaction.ModifierTypeId =>
              invData.ids.filter(mid => deliveryTracker.status(mid, mempool) == ModifiersStatus.Unknown)
            case typeId if history.acceptModifierType(typeId) =>
              invData.ids.filter(mid => deliveryTracker.status(mid, history) == ModifiersStatus.Unknown)
            case _ =>
              Seq.empty
          }

          if (acceptedModifierIds.nonEmpty) {
            val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, acceptedModifierIds)), None)
            peer.handlerRef ! msg
            deliveryTracker.setRequested(acceptedModifierIds, modifierTypeId, Some(peer))
          }

        case _ =>
          log.warn(s"Got data from peer while readers are not ready ${(mempoolReaderOpt, historyReaderOpt)}")
      }
  }

  /**
    * If new enough semantically valid ErgoFullBlock was applied, send inv for block header and all its sections
    */
  private val onSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(mod) =>
      broadcastInvForNewModifier(mod)
  }

  protected def broadcastInvForNewModifier(mod: PersistentNodeViewModifier): Unit =
    mod match {
      case fb: ErgoFullBlock if fb.header.isNew(timeProvider, 1.hour) =>
        fb.toSeq.foreach(s => broadcastModifierInv(s))
      case _ =>
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
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider)
           (implicit ex: ExecutionContext): Props =
    Props(new ErgoNodeViewSynchronizer(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings,
      timeProvider))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider,
            name: String)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider), name)

  final case object CheckModifiersToDownload

}
