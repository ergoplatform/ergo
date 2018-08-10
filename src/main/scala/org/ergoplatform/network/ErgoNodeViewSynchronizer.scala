package org.ergoplatform.network

import akka.actor.{ActorRef, ActorRefFactory, Props}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.ErgoTransaction
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.CheckModifiersToDownload
import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoSyncInfo, ErgoSyncInfoMessageSpec}
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.settings.Constants
import scorex.core.NodeViewHolder._
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages.SemanticallySuccessfulModifier
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
                               timeProvider: NetworkTimeProvider,
                               historyOpt: Option[ErgoHistory],
                               mempoolOpt: Option[ErgoMemPool])
                              (implicit ex: ExecutionContext)
  extends NodeViewSynchronizer[ErgoTransaction, ErgoSyncInfo, ErgoSyncInfoMessageSpec.type, ErgoPersistentModifier,
    ErgoHistory, ErgoMemPool](networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider,
    Constants.modifierSerializers) {
  /**
    * Approximate number of modifiers to be downloaded simultaneously
    */
  protected val desiredSizeOfExpectingQueue: Int = networkSettings.desiredInvObjects

  override protected val deliveryTracker = new ErgoDeliveryTracker(context.system, deliveryTimeout, maxDeliveryChecks,
    self, timeProvider)

  override def preStart(): Unit = {
    this.historyReaderOpt = historyOpt
    this.mempoolReaderOpt = mempoolOpt
    val toDownloadCheckInterval = networkSettings.syncInterval
    super.preStart()
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.scheduler.schedule(toDownloadCheckInterval, toDownloadCheckInterval)(self ! CheckModifiersToDownload)
  }

  protected val onCheckModifiersToDownload: Receive = {
    case CheckModifiersToDownload =>
      historyReaderOpt.foreach { h =>
        def downloadRequired(id: ModifierId): Boolean = deliveryTracker.status(id, Seq(h)) == ModifiersStatus.Unknown

        h.nextModifiersToDownload(desiredSizeOfExpectingQueue - deliveryTracker.requestedSize, downloadRequired)
          .groupBy(_._1).foreach(ids => requestDownload(ids._1, ids._2.map(_._2)))
      }
  }

  override protected def requestMoreModifiers(applied: Seq[ErgoPersistentModifier]): Unit = {
    super.requestMoreModifiers(applied)
    if (deliveryTracker.requestedSize < desiredSizeOfExpectingQueue / 2) {
      historyReaderOpt foreach { h =>
        if (h.isHeadersChainSynced) {
          // our expecting list list is is half empty - request more missed modifiers
          self ! CheckModifiersToDownload
        } else {
          sendSync(statusTracker, h)
        }
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
    mod match {
      case fb: ErgoFullBlock if fb.header.isNew(timeProvider, 1.hour) => fb.toSeq.foreach(s => broadcastModifierInv(s))
      case h: Header if h.isNew(timeProvider, 1.hour) => broadcastModifierInv(h)
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
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider,
            historyOpt: Option[ErgoHistory],
            mempoolOpt: Option[ErgoMemPool])
           (implicit ex: ExecutionContext): Props =
    Props(new ErgoNodeViewSynchronizer(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings,
      timeProvider, historyOpt, mempoolOpt)
    )

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider,
            historyOpt: Option[ErgoHistory] = None,
            mempoolOpt: Option[ErgoMemPool] = None)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider, historyOpt, mempoolOpt))

  def apply(networkControllerRef: ActorRef,
            viewHolderRef: ActorRef,
            syncInfoSpec: ErgoSyncInfoMessageSpec.type,
            networkSettings: NetworkSettings,
            timeProvider: NetworkTimeProvider,
            name: String)
           (implicit context: ActorRefFactory, ex: ExecutionContext): ActorRef =
    context.actorOf(props(networkControllerRef, viewHolderRef, syncInfoSpec, networkSettings, timeProvider, None, None), name)


  case object CheckModifiersToDownload

}
