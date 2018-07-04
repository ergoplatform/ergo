package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.ergoplatform.Version
import org.ergoplatform.local.ErgoStatsCollector.GetNodeInfo
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.network.Handshake
import scorex.core.network.NodeViewSynchronizer.ReceivableMessages._
import scorex.core.network.peer.PeerManager.ReceivableMessages.GetConnectedPeers
import scorex.core.utils.{NetworkTimeProvider, ScorexLogging}

import scala.concurrent.duration._

/**
  * Class that subscribes to NodeViewHolderEvents and collects them to provide fast response to API requests.
  */
class ErgoStatsCollector(viewHolderRef: ActorRef,
                         peerManager: ActorRef,
                         settings: ErgoSettings,
                         timeProvider: NetworkTimeProvider)
  extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ChangedHistory[_]])
    context.system.eventStream.subscribe(self, classOf[ChangedMempool[_]])
    context.system.eventStream.subscribe(self, classOf[SemanticallySuccessfulModifier[_]])
    context.system.scheduler.schedule(10.second, 10.second)(peerManager ! GetConnectedPeers)(context.system.dispatcher)
  }

  var nodeInfo = NodeInfo(settings.scorexSettings.network.nodeName, Version.VersionString, 0, 0, None,
    settings.nodeSettings.stateType, None, isMining = settings.nodeSettings.mining, None, None, None, None,
    timeProvider.time())

  override def receive: Receive = onConnectedPeers orElse getNodeInfo orElse onMempoolChanged orElse
    onHistoryChanged orElse onSemanticallySuccessfulModification

  private def getNodeInfo: Receive = {
    case GetNodeInfo => sender ! nodeInfo
  }

  private def onMempoolChanged: Receive = {
    case ChangedMempool(p) =>
      nodeInfo = nodeInfo.copy(unconfirmedCount = p.size)
  }

  private def onHistoryChanged: Receive = {
    case ChangedHistory(h: ErgoHistory@unchecked) if h.isInstanceOf[ErgoHistory] =>
      nodeInfo = nodeInfo.copy(bestFullBlockOpt = h.bestFullBlockOpt,
        bestHeaderOpt = h.bestHeaderOpt,
        headersScore = h.bestHeaderOpt.flatMap(m => h.scoreOf(m.id)),
        fullBlocksScore = h.bestFullBlockOpt.flatMap(m => h.scoreOf(m.id))
      )
  }

  private def onConnectedPeers: Receive = {
    case peers: Seq[Handshake@unchecked] if peers.headOption.forall(_.isInstanceOf[Handshake]) =>
      nodeInfo = nodeInfo.copy(peersCount = peers.length)
  }

  def onSemanticallySuccessfulModification: Receive = {
    case SemanticallySuccessfulModifier(fb: ErgoFullBlock) =>
      nodeInfo = nodeInfo.copy(stateRoot = Some(Algos.encode(fb.header.stateRoot)),
        stateVersion = Some(fb.encodedId))
  }

}

object ErgoStatsCollector {

  case object GetNodeInfo

}

object ErgoStatsCollectorRef {
  def props(viewHolderRef: ActorRef,
            peerManager: ActorRef,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider): Props =
    Props(new ErgoStatsCollector(viewHolderRef, peerManager, settings, timeProvider))

  def apply(viewHolderRef: ActorRef, peerManager: ActorRef, settings: ErgoSettings, timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(viewHolderRef, peerManager, settings, timeProvider))

  def apply(name: String,
            viewHolderRef: ActorRef,
            peerManager: ActorRef,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider)
           (implicit system: ActorSystem): ActorRef =
    system.actorOf(props(viewHolderRef, peerManager, settings, timeProvider), name)
}
