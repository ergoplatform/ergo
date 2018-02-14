package org.ergoplatform.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.{Encoder, Json}
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.ergoplatform.Version
import org.ergoplatform.api.routes.InfoRoute.Info
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.settings.Algos
import scorex.core.network.Handshake
import scorex.core.network.peer.PeerManager
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base58

import scala.concurrent.Future

case class InfoRoute(readersHolder: ActorRef,
                     miner: ActorRef,
                     peerManager: ActorRef,
                     digest: Boolean,
                     override val settings: RESTApiSettings, nodeId: Array[Byte])
                    (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
  override val route = info

  import InfoRoute.Info._

  private def getConnectedPeers: Future[Int] = (peerManager ? PeerManager.GetConnectedPeers).mapTo[Seq[Handshake]].map(_.size)

  private def getStateType: String = if (digest) "digest" else "utxo"

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
      Info.make(nodeId, minerInfo, connectedPeers, readers, getStateType).asJson
    }).okJson()
  }
}

object InfoRoute {
  def makeInfoJson(nodeId: Array[Byte],
                   minerInfo: MiningStatusResponse,
                   connectedPeersLength: Int,
                   readers: Readers,
                   stateType: String): Json = {
    val stateVersion = readers.s.map(_.version).map(Algos.encode)
    val bestHeader = readers.h.flatMap(_.bestHeaderOpt)
    val bestFullBlock = readers.h.flatMap(_.bestFullBlockOpt)
    val unconfirmedCount = readers.m.map(_.size).getOrElse(0)
    val stateRoot = readers.s.map(s => Algos.encode(s.rootHash)).getOrElse("null")
    Map(
      "name" -> Algos.encode(nodeId).asJson,
      "appVersion" -> Version.VersionString.asJson,
      "headersHeight" -> bestHeader.map(_.height).getOrElse(0).asJson,
      "fullHeight" -> bestFullBlock.map(_.header.height).getOrElse(0).asJson,
      "bestHeaderId" -> bestHeader.map(_.encodedId).getOrElse("null").asJson,
      "bestFullHeaderId" -> bestFullBlock.map(_.header.encodedId).getOrElse("null").asJson,
      "previousFullHeaderId" -> bestFullBlock.map(_.header.parentId).map(Base58.encode).getOrElse("null").asJson,
      "stateRoot" -> stateRoot.asJson,
      "difficulty" -> bestFullBlock.map(_.header.requiredDifficulty).getOrElse(BigInt(0)).asJson,
      "unconfirmedCount" -> unconfirmedCount.asJson,
      "stateType" -> stateType.asJson,
      "stateVersion" -> stateVersion.asJson,
      "isMining" -> minerInfo.isMining.asJson,
      "votes" -> Algos.encode(minerInfo.votes).asJson,
      "peersCount" -> connectedPeersLength.asJson
    ).asJson
  }


  case class Info(
                   name: String,
                   version: String,
                   headersHeight: Int,
                   fullHeight: Int,
                   bestHeaderId: Option[String],
                   bestFullHeaderId: Option[String],
                   previousFullHeaderId: Option[String],
                   stateRoot: Option[String],
                   difficulty: Long,
                   unconfirmedCount: Int,
                   stateType: String,
                   stateVersion: Option[String],
                   isMining: Boolean,
                   votes: String,
                   peersCount: Int
                 )

  object Info {

    def make(nodeId: Array[Byte],
             minerInfo: MiningStatusResponse,
             connectedPeersLength: Int,
             readers: Readers,
             stateType: String): Info = {
      val stateVersion = readers.s.map(_.version).map(Algos.encode)
      val bestHeader = readers.h.flatMap(_.bestHeaderOpt)
      val bestFullBlock = readers.h.flatMap(_.bestFullBlockOpt)
      val unconfirmedCount = readers.m.map(_.size).getOrElse(0)
      val stateRoot = readers.s.map(s => Algos.encode(s.rootHash))
      Info(
        Algos.encode(nodeId),
        Version.VersionString,
        bestHeader.map(_.height).getOrElse(0),
        bestFullBlock.map(_.header.height).getOrElse(0),
        bestHeader.map(_.encodedId),
        bestFullBlock.map(_.header.encodedId),
        bestFullBlock.map(_.header.parentId).map(Base58.encode),
        stateRoot,
        bestFullBlock.map(_.header.requiredDifficulty).getOrElse(BigInt(0)).toLong,
        unconfirmedCount,
        stateType,
        stateVersion,
        minerInfo.isMining,
        Algos.encode(minerInfo.votes),
        connectedPeersLength
      )
    }


    implicit val encoder: Encoder[Info] = deriveEncoder
  }
}


