package org.ergoplatform.local

import akka.actor.{ActorSystem, ActorRef, Actor, Props}
import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.local.ErgoStatsCollector.{NodeInfo, GetNodeInfo}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.ErgoReadersHolder.{Readers, GetReaders}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.state.{StateType, ErgoStateReader}
import org.ergoplatform.settings.{Parameters, Algos, ErgoSettings, LaunchParameters}
import scorex.core.network.ConnectedPeer
import scorex.core.network.NetworkController.ReceivableMessages.{GetPeersStatus, GetConnectedPeers}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.ReceivableMessages._
import org.ergoplatform.network.ErgoSyncTracker
import scorex.core.app.Version
import scorex.core.utils.NetworkTimeProvider
import scorex.core.utils.TimeProvider.Time
import scorex.util.ScorexLogging
import scorex.core.network.peer.PeersStatus

import java.net.URL
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._

/**
  * Class that subscribes to NodeViewHolderEvents and collects them to provide fast response to API requests.
  */
class ErgoStatsCollector(readersHolder: ActorRef,
                         networkController: ActorRef,
                         syncTracker: ErgoSyncTracker,
                         settings: ErgoSettings,
                         timeProvider: NetworkTimeProvider)
  extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    val ec: ExecutionContextExecutor = context.dispatcher

    readersHolder ! GetReaders
    context.system.eventStream.subscribe(self, classOf[ChangedHistory])
    context.system.eventStream.subscribe(self, classOf[ChangedState])
    context.system.eventStream.subscribe(self, classOf[ChangedMempool])
    context.system.eventStream.subscribe(self, classOf[FullBlockApplied])
    context.system.scheduler.scheduleAtFixedRate(10.seconds, 20.seconds, networkController, GetConnectedPeers)(ec, self)
    context.system.scheduler.scheduleAtFixedRate(45.seconds, 30.seconds, networkController, GetPeersStatus)(ec, self)
  }

  private def networkTime(): Time = timeProvider.time()

  private var nodeInfo = NodeInfo(
    settings.scorexSettings.network.nodeName,
    Version.VersionString,
    settings.networkType.verboseName,
    0,
    0,
    None,
    settings.nodeSettings.stateType,
    None,
    settings.nodeSettings.mining,
    None,
    None,
    None,
    None,
    None,
    launchTime = networkTime(),
    lastIncomingMessageTime = networkTime(),
    None,
    LaunchParameters,
    eip27Supported = true,
    settings.scorexSettings.restApi.publicUrl)

  override def receive: Receive =
    onConnectedPeers orElse
      onPeersStatus orElse
      getInfo orElse
      onMempoolChanged orElse
      onStateChanged orElse
      onHistoryChanged orElse
      onSemanticallySuccessfulModification orElse
      init orElse {
        case a: Any => log.warn(s"Stats collector got strange input: $a")
      }

  private def init: Receive = {
    case Readers(h, s, _, _) =>
      nodeInfo = nodeInfo.copy(
        bestFullBlockOpt = h.bestFullBlockOpt,
        bestHeaderOpt = h.bestHeaderOpt,
        headersScore = h.bestHeaderOpt.flatMap(m => h.scoreOf(m.id)),
        fullBlocksScore = h.bestFullBlockOpt.flatMap(m => h.scoreOf(m.id)),
        genesisBlockIdOpt = h.headerIdsAtHeight(ErgoHistory.GenesisHeight).headOption,
        stateRoot = Some(Algos.encode(s.rootHash)),
        stateVersion = Some(s.version),
        parameters = s.stateContext.currentParameters
      )
  }

  private def getInfo: Receive = {
    case GetNodeInfo => sender() ! nodeInfo
  }

  private def onMempoolChanged: Receive = {
    case ChangedMempool(p) =>
      nodeInfo = nodeInfo.copy(unconfirmedCount = p.size)
  }

  private def onStateChanged: Receive = {
    case ChangedState(s: ErgoStateReader@unchecked) =>
      val sc = s.stateContext
      nodeInfo = nodeInfo.copy(parameters = sc.currentParameters)
  }

  private def onHistoryChanged: Receive = {
    case ChangedHistory(h: ErgoHistory@unchecked) if h.isInstanceOf[ErgoHistory] =>

      if (nodeInfo.genesisBlockIdOpt.isEmpty) {
        nodeInfo = nodeInfo.copy(genesisBlockIdOpt = h.headerIdsAtHeight(ErgoHistory.GenesisHeight).headOption)
      }

      nodeInfo = nodeInfo.copy(bestFullBlockOpt = h.bestFullBlockOpt,
        bestHeaderOpt = h.bestHeaderOpt,
        headersScore = h.bestHeaderOpt.flatMap(m => h.scoreOf(m.id)),
        fullBlocksScore = h.bestFullBlockOpt.flatMap(m => h.scoreOf(m.id))
      )
  }

  private def onConnectedPeers: Receive = {
    case peers: Seq[ConnectedPeer@unchecked] if peers.headOption.forall(_.isInstanceOf[ConnectedPeer]) =>
      nodeInfo = nodeInfo.copy(
        peersCount = peers.length,
        maxPeerHeight = syncTracker.maxHeight()
      )
  }

  private def onPeersStatus: Receive = {
    case p2pStatus: PeersStatus =>
      nodeInfo = nodeInfo.copy(
        lastIncomingMessageTime = p2pStatus.lastIncomingMessage,
        maxPeerHeight = syncTracker.maxHeight()
      )
  }

  def onSemanticallySuccessfulModification: Receive = {
    case FullBlockApplied(header) =>
      nodeInfo = nodeInfo.copy(
        stateRoot = Some(Algos.encode(header.stateRoot)),
        stateVersion = Some(header.encodedId))
  }

}

object ErgoStatsCollector {

  case object GetNodeInfo

  /**
    * Data container for /info API request output
    *
    * @param nodeName - node (peer) self-chosen name from config
    * @param appVersion - node version
    * @param network - network type (mainnet/testnet)
    * @param unconfirmedCount - number of unconfirmed transactions in the mempool
    * @param peersCount - number of peer the node is connected with
    * @param stateRoot - current UTXO set digest
    * @param stateType - whether the node storing UTXO set, or only its digest
    * @param stateVersion - id of a block UTXO set digest is taken from
    * @param isMining - whether the node is mining
    * @param bestHeaderOpt - best header ID
    * @param headersScore - cumulative difficulty of best headers-chain
    * @param bestFullBlockOpt - best full-block id (header id of such block)
    * @param fullBlocksScore - cumulative difficulty of best full blocks chain
    * @param launchTime - when the node was launched (in Java time format, basically, UNIX time * 1000)
    * @param lastIncomingMessageTime - when the node received last p2p message (in Java time)
    * @param genesisBlockIdOpt - header id of genesis block
    * @param parameters - array with network parameters at the moment
    * @param eip27Supported - whether EIP-27 locked in
    * @param restApiUrl publicly accessible url of node which exposes restApi in firewall
    */
  case class NodeInfo(nodeName: String,
                      appVersion: String,
                      network: String,
                      unconfirmedCount: Int,
                      peersCount: Int,
                      stateRoot: Option[String],
                      stateType: StateType,
                      stateVersion: Option[String],
                      isMining: Boolean,
                      bestHeaderOpt: Option[Header],
                      headersScore: Option[BigInt],
                      bestFullBlockOpt: Option[ErgoFullBlock],
                      fullBlocksScore: Option[BigInt],
                      maxPeerHeight : Option[Int], // Maximum block height of connected peers
                      launchTime: Long,
                      lastIncomingMessageTime: Long,
                      genesisBlockIdOpt: Option[String],
                      parameters: Parameters,
                      eip27Supported: Boolean,
                      restApiUrl: Option[URL])

  object NodeInfo extends ApiCodecs {
    implicit val paramsEncoder: Encoder[Parameters] = org.ergoplatform.settings.ParametersSerializer.jsonEncoder

    implicit val jsonEncoder: Encoder[NodeInfo] = (ni: NodeInfo) => {
      val optionalFields =
        ni.restApiUrl.map(_.toString).map(restApiUrl => Map("restApiUrl" -> restApiUrl.asJson)).getOrElse(Map.empty)
      (Map(
        "name" -> ni.nodeName.asJson,
        "appVersion" -> Version.VersionString.asJson,
        "network" -> ni.network.asJson,
        "headersHeight" -> ni.bestHeaderOpt.map(_.height).asJson,
        "fullHeight" -> ni.bestFullBlockOpt.map(_.header.height).asJson,
        "maxPeerHeight" -> ni.maxPeerHeight.asJson,
        "bestHeaderId" -> ni.bestHeaderOpt.map(_.encodedId).asJson,
        "bestFullHeaderId" -> ni.bestFullBlockOpt.map(_.header.encodedId).asJson,
        "previousFullHeaderId" -> ni.bestFullBlockOpt.map(_.header.parentId).map(Algos.encode).asJson,
        "difficulty" -> ni.bestFullBlockOpt.map(_.header.requiredDifficulty).map(difficultyEncoder.apply).asJson,
        "headersScore" -> ni.headersScore.map(difficultyEncoder.apply).asJson,
        "fullBlocksScore" -> ni.fullBlocksScore.map(difficultyEncoder.apply).asJson,
        "unconfirmedCount" -> ni.unconfirmedCount.asJson,
        "stateRoot" -> ni.stateRoot.asJson,
        "stateType" -> ni.stateType.stateTypeName.asJson,
        "stateVersion" -> ni.stateVersion.asJson,
        "isMining" -> ni.isMining.asJson,
        "peersCount" -> ni.peersCount.asJson,
        "launchTime" -> ni.launchTime.asJson,
        "lastSeenMessageTime" -> ni.lastIncomingMessageTime.asJson,
        "genesisBlockId" -> ni.genesisBlockIdOpt.asJson,
        "parameters" -> ni.parameters.asJson,
        "eip27Supported" -> ni.eip27Supported.asJson,
        "eip37Supported" -> true.asJson
      ) ++ optionalFields).asJson
    }
  }

}

object ErgoStatsCollectorRef {

  def props(readersHolder: ActorRef,
            networkController: ActorRef,
            syncTracker : ErgoSyncTracker,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider): Props =
    Props(new ErgoStatsCollector(readersHolder, networkController, syncTracker, settings, timeProvider))


  def apply(readersHolder: ActorRef,
            networkController: ActorRef,
            syncTracker : ErgoSyncTracker,
            settings: ErgoSettings,
            timeProvider: NetworkTimeProvider)(implicit system: ActorSystem): ActorRef =
    system.actorOf(props(readersHolder, networkController, syncTracker, settings, timeProvider))

}
