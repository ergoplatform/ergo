package org.ergoplatform.local

import akka.actor.{ActorRef, ActorSystem, Props}
import io.circe.syntax._
import org.ergoplatform.Version
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, NodeInfo}
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.modifiers.mempool.AnyoneCanSpendTransaction
import org.ergoplatform.modifiers.mempool.proposition.AnyoneCanSpendProposition
import org.ergoplatform.modifiers.{ErgoFullBlock, ErgoPersistentModifier}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.{ErgoStateReader, StateType}
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.NodeViewHolder.{ChangedHistory, ChangedMempool, Subscribe}
import scorex.core.network.Handshake
import scorex.core.network.peer.PeerManager
import scorex.core.transaction.state.StateReader
import scorex.core.utils.NetworkTimeProvider
import scorex.core.{LocalInterface, ModifierId, NodeViewHolder}
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * Class that subscribes to NodeViewHolderEvents and collects them to provide fast response to API requests.
  */
class ErgoStatsCollector(override val viewHolderRef: ActorRef,
                         peerManager: ActorRef,
                         settings: ErgoSettings,
                         timeProvider: NetworkTimeProvider)
  extends LocalInterface[AnyoneCanSpendProposition.type, AnyoneCanSpendTransaction, ErgoPersistentModifier] {

  override def preStart(): Unit = {
    val events = Seq(
      NodeViewHolder.EventType.StateChanged,
      NodeViewHolder.EventType.HistoryChanged,
      NodeViewHolder.EventType.MempoolChanged
    )
    viewHolderRef ! Subscribe(events)
    context.system.scheduler.schedule(10.second, 10.second)(peerManager ! PeerManager.GetConnectedPeers)
  }

  private val votes = Algos.encode(Algos.hash(settings.scorexSettings.network.nodeName).take(5))

  // TODO get actual votes and isMining from miner
  var nodeInfo = NodeInfo(settings.scorexSettings.network.nodeName, Version.VersionString, 0, 0, "null",
    settings.nodeSettings.stateType, "null", isMining = settings.nodeSettings.mining, votes, None, None,
    timeProvider.time())

  override def receive: Receive = onConnectedPeers orElse getNodeInfo orElse onMempoolChanged orElse
    onHistoryChanged orElse super.receive

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

  override protected def onChangedState(stateReader: StateReader): Unit = stateReader match {
    case r: ErgoStateReader =>
      nodeInfo = nodeInfo.copy(stateRoot = Algos.encode(r.asInstanceOf[ErgoStateReader].rootHash),
        stateVersion = Algos.encode(r.version))
    case _ =>
      log.warn(s"Got state reader of incorrect type $stateReader")
  }


  //TODO move default empty implementations to Scorex
  override protected def onStartingPersistentModifierApplication(pmod: ErgoPersistentModifier): Unit = {}

  override protected def onFailedTransaction(tx: AnyoneCanSpendTransaction): Unit = {}

  override protected def onSuccessfulTransaction(tx: AnyoneCanSpendTransaction): Unit = {}

  override protected def onNoBetterNeighbour(): Unit = {}

  override protected def onBetterNeighbourAppeared(): Unit = {}

  override protected def onSyntacticallySuccessfulModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onSyntacticallyFailedModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onSemanticallySuccessfulModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onSemanticallyFailedModification(mod: ErgoPersistentModifier): Unit = {}

  override protected def onNewSurface(newSurface: Seq[ModifierId]): Unit = {}

  override protected def onRollbackFailed(): Unit = {}
}

object ErgoStatsCollector {

  case object GetNodeInfo

  case class NodeInfo(nodeName: String,
                      appVersion: String,
                      unconfirmedCount: Int,
                      peersCount: Int,
                      stateRoot: String,
                      stateType: StateType,
                      stateVersion: String,
                      isMining: Boolean,
                      votes: String,
                      bestHeaderOpt: Option[Header],
                      bestFullBlockOpt: Option[ErgoFullBlock],
                      launchTime: Long) {
    lazy val json = Map(
      "name" -> nodeName.asJson,
      "appVersion" -> Version.VersionString.asJson,
      "headersHeight" -> bestHeaderOpt.map(_.height).getOrElse(-1).asJson,
      "fullHeight" -> bestFullBlockOpt.map(_.header.height).getOrElse(-1).asJson,
      "bestHeaderId" -> bestHeaderOpt.map(_.encodedId).getOrElse("null").asJson,
      "bestFullHeaderId" -> bestFullBlockOpt.map(_.header.encodedId).getOrElse("null").asJson,
      "previousFullHeaderId" -> bestFullBlockOpt.map(_.header.parentId).map(Base58.encode).getOrElse("null").asJson,
      "difficulty" -> bestFullBlockOpt.map(_.header.requiredDifficulty).getOrElse(BigInt(0)).toString(10).asJson,
      "unconfirmedCount" -> unconfirmedCount.asJson,
      "stateRoot" -> stateRoot.asJson,
      "stateType" -> stateType.stateTypeName.asJson,
      "stateVersion" -> stateVersion.asJson,
      "isMining" -> isMining.asJson,
      "votes" -> votes.asJson,
      "peersCount" -> peersCount.asJson,
      "launchTime" -> launchTime.asJson
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