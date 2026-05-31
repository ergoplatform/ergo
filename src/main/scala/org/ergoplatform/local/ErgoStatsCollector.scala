package org.ergoplatform.local

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import io.circe.Encoder
import io.circe.syntax._
import org.ergoplatform.Version
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.local.ErgoStatsCollector.{GetNodeInfo, GetRecentRollbacks, NodeInfo, RollbackInfo}
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import org.ergoplatform.nodeView.state.{ErgoStateReader, StateType}
import org.ergoplatform.settings.{Algos, ErgoSettings, Parameters}
import scorex.core.network.ConnectedPeer
import scorex.core.network.NetworkController.ReceivableMessages.{GetConnectedPeers, GetPeersStatus}
import org.ergoplatform.network.ErgoNodeViewSynchronizerMessages._
import org.ergoplatform.network.ErgoSyncTracker
import scorex.util.ScorexLogging
import org.ergoplatform.network.peer.PeersStatus

import java.net.URL
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._

/**
  * Class that subscribes to NodeViewHolderEvents and collects them to provide fast response to API requests.
  */
class ErgoStatsCollector(readersHolder: ActorRef,
                         networkController: ActorRef,
                         syncTracker: ErgoSyncTracker,
                         settings: ErgoSettings)
  extends Actor with ScorexLogging {

  override def preStart(): Unit = {
    val ec: ExecutionContextExecutor = context.dispatcher

    readersHolder ! GetReaders
    context.system.eventStream.subscribe(self, classOf[ChangedHistory])
    context.system.eventStream.subscribe(self, classOf[ChangedState])
    context.system.eventStream.subscribe(self, classOf[ChangedMempool])
    context.system.eventStream.subscribe(self, classOf[FullBlockApplied])
    context.system.eventStream.subscribe(self, classOf[Rollback])
    context.system.scheduler.scheduleAtFixedRate(10.seconds, 20.seconds, networkController, GetConnectedPeers)(ec, self)
    context.system.scheduler.scheduleAtFixedRate(45.seconds, 30.seconds, networkController, GetPeersStatus)(ec, self)
  }

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
    launchTime = System.currentTimeMillis(),
    lastIncomingMessageTime = System.currentTimeMillis(),
    lastMemPoolUpdateTime = System.currentTimeMillis(),
    None,
    settings.launchParameters,
    eip27Supported = true,
    settings.scorexSettings.restApi.publicUrl,
    settings.nodeSettings.extraIndex)

  // most recent chain rollbacks observed by the node, newest first, kept in memory only
  private val MaxRecentRollbacks = 20
  private var recentRollbacks: Seq[RollbackInfo] = Seq.empty

  override def receive: Receive =
    onConnectedPeers orElse
      onPeersStatus orElse
      getInfo orElse
      getRollbacks orElse
      onMempoolChanged orElse
      onStateChanged orElse
      onHistoryChanged orElse
      onSemanticallySuccessfulModification orElse
      onRollback orElse
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
        genesisBlockIdOpt = h.headerIdsAtHeight(GenesisHeight).headOption,
        stateRoot = Some(Algos.encode(s.rootDigest)),
        stateVersion = Some(s.version),
        parameters = s.stateContext.currentParameters
      )
  }

  private def getInfo: Receive = {
    case GetNodeInfo => sender() ! nodeInfo
  }

  private def getRollbacks: Receive = {
    case GetRecentRollbacks => sender() ! recentRollbacks
  }

  private def onMempoolChanged: Receive = {
    case ChangedMempool(p) =>
      nodeInfo = nodeInfo.copy(lastMemPoolUpdateTime = System.currentTimeMillis())
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
        nodeInfo = nodeInfo.copy(genesisBlockIdOpt = h.headerIdsAtHeight(GenesisHeight).headOption)
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

  private def onRollback: Receive = {
    case Rollback(branchPoint, branchPointHeight, depth, appliedBlocks, timestamp) =>
      val record =
        RollbackInfo(branchPoint, branchPointHeight, depth, appliedBlocks, timestamp)
      recentRollbacks = (record +: recentRollbacks).take(MaxRecentRollbacks)
      log.info(s"Recorded rollback to $branchPoint " +
        s"(height ${branchPointHeight.getOrElse("?")}, depth $depth)")
  }

}

object ErgoStatsCollector {

  case object GetNodeInfo

  case object GetRecentRollbacks

  /**
    * Information about a single chain rollback performed by the node when switching to a better chain.
    *
    * @param branchPointId - header id of the block which is last in the chain after the rollback
    *                        (the common branch point before applying the new chain suffix)
    * @param branchPointHeight - height of the branch point block, if known
    * @param depth - number of full blocks rolled back
    * @param appliedBlocks - length of the new chain suffix applied after rollback
    *                        (intended applies captured at rollback time)
    * @param timestamp - when the rollback happened (in Java time, basically, UNIX time * 1000)
    */
  case class RollbackInfo(branchPointId: String,
                          branchPointHeight: Option[Int],
                          depth: Int,
                          appliedBlocks: Int,
                          timestamp: Long)

  object RollbackInfo {
    implicit val jsonEncoder: Encoder[RollbackInfo] = (ri: RollbackInfo) =>
      Map(
        "branchPointId" -> ri.branchPointId.asJson,
        "branchPointHeight" -> ri.branchPointHeight.asJson,
        "depth" -> ri.depth.asJson,
        "appliedBlocks" -> ri.appliedBlocks.asJson,
        "timestamp" -> ri.timestamp.asJson
      ).asJson
  }

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
    * @param bestFullBlockOpt - Best full-block known to the node. Can be None if state is empty (no full block is applied since node launch)
    * @param fullBlocksScore - cumulative difficulty of best full blocks chain
    * @param maxPeerHeight - maximum block height of connected peers
    * @param launchTime - when the node was launched (in Java time format, basically, UNIX time * 1000)
    * @param lastIncomingMessageTime - when the node received last p2p message (in Java time)
    * @param lastMemPoolUpdateTime - when the mempool was last updated (in Java time)
    * @param genesisBlockIdOpt - header id of genesis block
    * @param parameters - array with network parameters at the moment
    * @param eip27Supported - whether EIP-27 locked in
    * @param restApiUrl - publicly accessible url of node which exposes restApi in firewall
    * @param extraIndex - whether the node has additional indexing enabled
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
                      maxPeerHeight : Option[Int],
                      launchTime: Long,
                      lastIncomingMessageTime: Long,
                      lastMemPoolUpdateTime: Long,
                      genesisBlockIdOpt: Option[String],
                      parameters: Parameters,
                      eip27Supported: Boolean,
                      restApiUrl: Option[URL],
                      extraIndex: Boolean)

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
        "isExplorer" -> ni.extraIndex.asJson,
        "peersCount" -> ni.peersCount.asJson,
        "launchTime" -> ni.launchTime.asJson,
        "lastSeenMessageTime" -> ni.lastIncomingMessageTime.asJson,
        "lastMemPoolUpdateTime" -> ni.lastMemPoolUpdateTime.asJson,
        "genesisBlockId" -> ni.genesisBlockIdOpt.asJson,
        "parameters" -> ni.parameters.asJson,
        "eip27Supported" -> ni.eip27Supported.asJson,
        "eip37Supported" -> true.asJson
      ) ++ optionalFields).asJson
    }
  }

}

object ErgoStatsCollectorRef {

  private def props(readersHolder: ActorRef,
            networkController: ActorRef,
            syncTracker : ErgoSyncTracker,
            settings: ErgoSettings): Props =
    Props(new ErgoStatsCollector(readersHolder, networkController, syncTracker, settings))


  def apply(readersHolder: ActorRef,
            networkController: ActorRef,
            syncTracker : ErgoSyncTracker,
            settings: ErgoSettings)(implicit system: ActorSystem): ActorRef =
    system.actorOf(props(readersHolder, networkController, syncTracker, settings))

}
