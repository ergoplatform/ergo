package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import io.circe.syntax._
import io.circe.{Encoder, JsonNumber}
import org.ergoplatform.Version
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, NodeInfo}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.StateType
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

  private val votes = Algos.encode(Algos.hash(settings.scorexSettings.network.nodeName).take(5))

  // TODO get actual votes and isMining from miner
  var nodeInfo = NodeInfo(settings.scorexSettings.network.nodeName, Version.VersionString, 0, 0, None,
    settings.nodeSettings.stateType, None, isMining = settings.nodeSettings.mining, votes, None, None,
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
    case ChangedHistory(h) if h.isInstanceOf[ErgoHistory] =>
      nodeInfo = nodeInfo.copy(bestFullBlockOpt = h.asInstanceOf[ErgoHistory].bestFullBlockOpt,
        bestHeaderOpt = h.asInstanceOf[ErgoHistory].bestHeaderOpt)
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

  case class NodeInfo(nodeName: String,
                      appVersion: String,
                      unconfirmedCount: Int,
                      peersCount: Int,
                      stateRoot: Option[String],
                      stateType: StateType,
                      stateVersion: Option[String],
                      isMining: Boolean,
                      votes: String,
                      bestHeaderOpt: Option[Header],
                      bestFullBlockOpt: Option[ErgoFullBlock],
                      launchTime: Long) {
  }

  object NodeInfo {
    implicit val jsonEncoder: Encoder[NodeInfo] = (ni: NodeInfo) =>
      Map(
        "name" -> ni.nodeName.asJson,
        "appVersion" -> Version.VersionString.asJson,
        "headersHeight" -> ni.bestHeaderOpt.map(_.height).asJson,
        "fullHeight" -> ni.bestFullBlockOpt.map(_.header.height).asJson,
        "bestHeaderId" -> ni.bestHeaderOpt.map(_.encodedId).asJson,
        "bestFullHeaderId" -> ni.bestFullBlockOpt.map(_.header.encodedId).asJson,
        "previousFullHeaderId" -> ni.bestFullBlockOpt.map(_.header.parentId).map(Algos.encode).asJson,
        "difficulty" -> ni.bestFullBlockOpt.map(_.header.requiredDifficulty.toString(10)).map(JsonNumber.fromString).asJson,
        "unconfirmedCount" -> ni.unconfirmedCount.asJson,
        "stateRoot" -> ni.stateRoot.asJson,
        "stateType" -> ni.stateType.stateTypeName.asJson,
        "stateVersion" -> ni.stateVersion.asJson,
        "isMining" -> ni.isMining.asJson,
        "votes" -> ni.votes.asJson,
        "peersCount" -> ni.peersCount.asJson,
        "launchTime" -> ni.launchTime.asJson
      ).asJson
  }

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
