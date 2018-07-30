package org.ergoplatform.network

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{BlockSection, ErgoPersistentModifier}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.CheckModifiersToDownload
import org.ergoplatform.nodeView.ErgoModifiersCache
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.Constants
import scorex.core.NodeViewHolder._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.{SemanticallySuccessfulModifier, SyntacticallySuccessfulModifier}
import scorex.core.network.{ModifiersStatus, NodeViewSynchronizer}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{ModifierId, PersistentNodeViewModifier}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class ErgoNodeViewSynchronizer(networkControllerRef: ActorRef,
                               viewHolderRef: ActorRef,
                               syncInfoSpec: ErgoSyncInfoMessageSpec.type,
                               networkSettings: NetworkSettings,
                               timeProvider: NetworkTimeProvider)
                              (implicit ex: ExecutionContext)
  extends NodeViewSynchronizer[ErgoTransaction, ErgoSyncInfo, ErgoSyncInfoMessageSpec.type, ErgoPersistentModifier,
    ErgoHistory, ErgoMemPool](networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider,
    Constants.modifierSerializers) {

  override protected lazy val modifiersCache = new ErgoModifiersCache(networkSettings.maxModifiersCacheSize)

  override protected val deliveryTracker = new ErgoDeliveryTracker(context.system, deliveryTimeout, maxDeliveryChecks,
    self, timeProvider)

  private val downloadListSize = networkSettings.maxInvObjects

  override def preStart(): Unit = {
    val toDownloadCheckInterval = networkSettings.syncInterval
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.scheduler.schedule(toDownloadCheckInterval, toDownloadCheckInterval)(self ! CheckModifiersToDownload)
  }

  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      historyReaderOpt.foreach { h =>
        def downloadRequired(id: ModifierId): Boolean = deliveryTracker.status(id, Seq(h)) == ModifiersStatus.Unknown

        h.nextModifiersToDownload(downloadListSize - deliveryTracker.expectingSize, downloadRequired)
          .groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
  }

  /**
    * Put modifier to applied status
    * Request more modifiers, if expecting queue is small enough
    */
  private val onSyntacticallySuccessfulModifier: Receive = {
    case SyntacticallySuccessfulModifier(mod) =>
      deliveryTracker.toApplied(mod.id)

      //If queue is empty - check, whether there are more modifiers to download
      historyReaderOpt foreach { h =>
        mod match {
          case _: Header if !h.isHeadersChainSynced && !deliveryTracker.isExpecting =>
            // headers chain is not synced yet, but our expecting list is empty - ask for more headers
            sendSync(statusTracker, h)
          case _: BlockSection if downloadListSize - deliveryTracker.expectingSize > downloadListSize / 2 =>
            // our expecting list list is is half empty - request more missed modifiers
            self ! CheckModifiersToDownload
          case _ =>
        }
      }
  }

  /**
    * Broadcast inv if modifier is new enough
    */
  private val onSemanticallySuccessfulModifier: Receive = {
    case SemanticallySuccessfulModifier(mod) =>
      broadcastInvForNewModifier(mod)
  }


  private def broadcastInvForNewModifier(mod: PersistentNodeViewModifier): Unit = {
    historyReaderOpt foreach { h =>
      val modifierHeader: Option[Header] = mod match {
        case header: Header => Some(header)
        case s: BlockSection => h.typedModifierById[Header](s.headerId)
        case _ => None
      }
      modifierHeader.foreach { header =>
        if (header.isNew(timeProvider, 1.hour)) {
          broadcastModifierInv(mod)
        }
      }
    }
  }

  override protected def viewHolderEvents: Receive =
    onSyntacticallySuccessfulModifier orElse
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


  case object CheckModifiersToDownload

}
