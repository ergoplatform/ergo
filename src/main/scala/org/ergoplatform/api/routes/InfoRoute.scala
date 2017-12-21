package org.ergoplatform.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import io.circe.syntax._
import org.ergoplatform.Version
import org.ergoplatform.local.ErgoMiner.{MiningStatusRequest, MiningStatusResponse}
import org.ergoplatform.nodeView.ErgoReadersHolder.{GetReaders, Readers}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.mempool.ErgoMemPool
import org.ergoplatform.nodeView.state.{DigestState, ErgoState, UtxoState}
import org.ergoplatform.nodeView.wallet.ErgoWallet
import org.ergoplatform.settings.Algos
import scorex.core.NodeViewHolder.GetDataFromCurrentView
import scorex.core.network.Handshake
import scorex.core.network.peer.PeerManager
import scorex.core.settings.RESTApiSettings
import scorex.crypto.encode.Base58

import scala.concurrent.Future

case class InfoRoute(readersHolder: ActorRef,
                     nodeViewActorRef: ActorRef,
                     miner: ActorRef,
                     peerManager: ActorRef,
                     digest: Boolean,
                     override val settings: RESTApiSettings, nodeId: Array[Byte])
                    (implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
  override val route = info

  private val request = if (digest) {
    GetDataFromCurrentView[ErgoHistory, DigestState, ErgoWallet, ErgoMemPool, DigestState](_.state)
  } else {
    GetDataFromCurrentView[ErgoHistory, UtxoState, ErgoWallet, ErgoMemPool, UtxoState](_.state)
  }

  private def getState = if (digest) {
    (nodeViewActorRef ? request).mapTo[DigestState]
  } else {
    (nodeViewActorRef ? request).mapTo[UtxoState]
  }

  private def getVersion: Future[String] = getState.map{ _.version }.map { v =>
    Base58.encode(v)
  }

  private def getConnectedPeers: Future[Int] = (peerManager ? PeerManager.GetConnectedPeers).mapTo[Seq[Handshake]].map(_.size)

  private def getStateType: String = if (digest) "digest" else "utxo"

  private def getMinerInfo: Future[MiningStatusResponse] = (miner ? MiningStatusRequest).mapTo[MiningStatusResponse]

  def info: Route = path("info") {
    get {
      toJsonResponse {
        val minerInfoF = getMinerInfo
        val stateVersionF = getVersion
        val connectedPeersF = getConnectedPeers
        val readersF = (readersHolder ? GetReaders).mapTo[Readers]
          for {
            stateVersion <- stateVersionF
            minerInfo <- minerInfoF
            connectedPeers <- connectedPeersF
            readers <- readersF
          } yield {
          val bestHeader = readers.h.flatMap(_.bestHeaderOpt)
          val bestFullBlock = readers.h.flatMap(_.bestFullBlockOpt)
          val poolSize = readers.m.map(_.size).getOrElse(-1)
          val stateRoot = readers.s.map(s => Algos.encode(s.rootHash)).getOrElse("Undefined")
          Map(
            "name" -> Algos.encode(nodeId).asJson,
            "stateVersion" -> Version.VersionString.asJson,
            "headersHeight" -> bestHeader.map(_.height).getOrElse(-1).asJson,
            "fullHeight" -> bestFullBlock.map(_.header.height).getOrElse(-1).asJson,
            "bestHeaderId" -> bestHeader.map(_.encodedId).getOrElse("None").asJson,
            "bestFullHeaderId" -> bestFullBlock.map(_.header.encodedId).getOrElse("None").asJson,
            "previousFullHeaderId" -> bestFullBlock.map(_.header.parentId).map(Base58.encode).getOrElse("None").asJson,
            "stateRoot" -> stateRoot.asJson,
            "difficulty" -> bestFullBlock.map(_.header.requiredDifficulty.toString).getOrElse("None").asJson,
            "unconfirmedCount" -> poolSize.asJson,
            "stateType" -> getStateType.asJson,
            "stateVersion" -> stateVersion.asJson,
            "isMining" -> minerInfo.isMining.asJson,
            "votes" -> Algos.encode(minerInfo.votes).asJson,
            "peersCount" -> connectedPeers.asJson
          ).asJson
        }
      }
    }
  }
}
