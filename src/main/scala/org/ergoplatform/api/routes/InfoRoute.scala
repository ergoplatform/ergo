package org.ergoplatform.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.Version
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings.{Algos, ErgoSettings}
import scorex.core.network.Handshake
import scorex.core.network.peer.PeerManager
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base58

import scala.concurrent.Future

case class InfoRoute(readersHolder: ActorRef,
                     miner: ActorRef,
                     peerManager: ActorRef,
                     ergoSettings: ErgoSettings,
                     nodeId: Array[Byte])
                    (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
  override val route = info

  override val settings: RESTApiSettings = ergoSettings.scorexSettings.restApi

  private def getConnectedPeers: Future[Int] = (peerManager ? PeerManager.GetConnectedPeers).mapTo[Seq[Handshake]].map(_.size)

  private def stateType: StateType = ergoSettings.nodeSettings.stateType

  private def nodeName: String = ergoSettings.scorexSettings.network.nodeName

  private def getMinerInfo: Future[MiningStatusResponse] = (miner ? MiningStatusRequest).mapTo[MiningStatusResponse]

  def info: Route = (path("info") & get) {
    val minerInfoF = getMinerInfo
    val connectedPeersF = getConnectedPeers
    val readersF: Future[Readers] = (readersHolder ? GetReaders).mapTo[Readers]
    (for {
      minerInfo <- minerInfoF
      connectedPeers <- connectedPeersF
      readers <- readersF
    } yield {
      InfoRoute.makeInfoJson(nodeId, minerInfo, connectedPeers, readers, stateType, nodeName)
    }).okJson()
  }
}

object InfoRoute {
  def makeInfoJson(nodeId: Array[Byte],
                   minerInfo: MiningStatusResponse,
                   connectedPeersLength: Int,
                   readers: Readers,
                   stateType: StateType,
                   nodeName: String): Json = {
    val stateVersion = readers.s.map(_.version).map(Algos.encode)
    val bestHeader = readers.h.flatMap(_.bestHeaderOpt)
    val bestFullBlock = readers.h.flatMap(_.bestFullBlockOpt)
    val unconfirmedCount = readers.m.map(_.size).getOrElse(0)
    val stateRoot = readers.s.map(s => Algos.encode(s.rootHash)).getOrElse("null")
    Map(
      "name" -> nodeName.asJson,
      "appVersion" -> Version.VersionString.asJson,
      "headersHeight" -> bestHeader.map(_.height).getOrElse(0).asJson,
      "fullHeight" -> bestFullBlock.map(_.header.height).getOrElse(0).asJson,
      "bestHeaderId" -> bestHeader.map(_.encodedId).getOrElse("null").asJson,
      "bestFullHeaderId" -> bestFullBlock.map(_.header.encodedId).getOrElse("null").asJson,
      "previousFullHeaderId" -> bestFullBlock.map(_.header.parentId).map(Base58.encode).getOrElse("null").asJson,
      "stateRoot" -> stateRoot.asJson,
      "difficulty" -> bestFullBlock.map(_.header.requiredDifficulty).getOrElse(BigInt(0)).toString(10).asJson,
      "unconfirmedCount" -> unconfirmedCount.asJson,
      "stateType" -> stateType.asJson,
      "stateVersion" -> stateVersion.asJson,
      "isMining" -> minerInfo.isMining.asJson,
      "votes" -> Algos.encode(minerInfo.votes).asJson,
      "peersCount" -> connectedPeersLength.asJson
    ).asJson
  }
}
