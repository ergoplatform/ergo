package org.ergoplatform.http.api

import java.net.{InetAddress, InetSocketAddress}
import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import io.circe.syntax._
import io.circe.{Encoder, Json}
import io.circe.generic.semiauto.deriveEncoder
import org.ergoplatform.network.ErgoSyncTracker
import scorex.core.api.http.{ApiError, ApiResponse, ApiRoute}
import scorex.core.network.{ConnectedPeer, DeliveryTracker}
import scorex.core.network.NetworkController.ReceivableMessages.{ConnectTo, GetConnectedPeers, GetPeersStatus}
import scorex.core.network.peer.{PeerInfo, PeersStatus}
import scorex.core.network.peer.PeerManager.ReceivableMessages.{GetAllPeers, GetBlacklistedPeers}
import scorex.core.settings.RESTApiSettings

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

class ErgoPeersApiRoute(peerManager: ActorRef,
                        networkController: ActorRef,
                        syncTracker: ErgoSyncTracker,
                        deliveryTracker: DeliveryTracker,
                        override val settings: RESTApiSettings)
                       (implicit val context: ActorRefFactory, val ec: ExecutionContext)
  extends ApiRoute {
  import ErgoPeersApiRoute._

  override implicit lazy val timeout: Timeout = Timeout(1.minute)

  override lazy val route: Route = pathPrefix("peers") {
    allPeers ~ connectedPeers ~ blacklistedPeers ~ connect ~ peersStatus ~ syncInfo ~ trackInfo
  }

  def syncInfo: Route = (path("syncInfo") & get) {
    ApiResponse(syncTracker.fullInfo)
  }

  def trackInfo: Route = (path("trackInfo") & get) {
    ApiResponse(deliveryTracker.fullInfo)
  }

  def allPeers: Route = (path("all") & get) {
    val result = askActor[Map[InetSocketAddress, PeerInfo]](peerManager, GetAllPeers).map {
      _.map { case (address, peerInfo) =>
        PeerInfoResponse.fromAddressAndInfo(address, peerInfo)
      }
    }
    ApiResponse(result)
  }

  def connectedPeers: Route = (path("connected") & get) {
    val result = askActor[Seq[ConnectedPeer]](networkController, GetConnectedPeers).map {
      _.flatMap { con =>
        con.peerInfo.map { peerInfo =>
          PeerInfoResponse(
            address = peerInfo.peerSpec.declaredAddress.map(_.toString).getOrElse(""),
            lastMessage = con.lastMessage,
            lastHandshake = peerInfo.lastHandshake,
            name = peerInfo.peerSpec.nodeName,
            connectionType = peerInfo.connectionType.map(_.toString)
          )
        }
      }
    }
    ApiResponse(result)
  }

  /**
    * Get status of P2P layer
    *
    * @return time of last incoming message and network time (got from NTP server)
    */
  def peersStatus: Route = (path("status") & get) {
    val result = askActor[PeersStatus](networkController, GetPeersStatus).map {
      case PeersStatus(lastIncomingMessage, currentNetworkTime) =>
        PeersStatusResponse(lastIncomingMessage, currentNetworkTime)
    }
    ApiResponse(result)
  }

  private val addressAndPortRegexp = "([\\w\\.]+):(\\d{1,5})".r

  def connect: Route = (path("connect") & post & withAuth & entity(as[Json])) { json =>
    val maybeAddress = json.asString.flatMap(addressAndPortRegexp.findFirstMatchIn)
    maybeAddress match {
      case None => ApiError.BadRequest
      case Some(addressAndPort) =>
        val host = InetAddress.getByName(addressAndPort.group(1))
        val port = addressAndPort.group(2).toInt
        networkController ! ConnectTo(PeerInfo.fromAddress(new InetSocketAddress(host, port)))
        ApiResponse.OK
    }
  }

  def blacklistedPeers: Route = (path("blacklisted") & get) {
    val result = askActor[Seq[InetAddress]](peerManager, GetBlacklistedPeers)
      .map(x => BlacklistedPeers(x.map(_.toString)).asJson)
    ApiResponse(result)
  }

}

object ErgoPeersApiRoute {

  def apply(peerManager: ActorRef,
            networkController: ActorRef,
            syncTracker: ErgoSyncTracker,
            deliveryTracker: DeliveryTracker,
            settings: RESTApiSettings)
           (implicit context: ActorRefFactory, ec: ExecutionContext): ErgoPeersApiRoute =
    new ErgoPeersApiRoute(peerManager, networkController, syncTracker, deliveryTracker, settings)(context, ec)

  case class PeerInfoResponse(address: String,
                              lastMessage: Long,
                              lastHandshake: Long,
                              name: String,
                              connectionType: Option[String])

  object PeerInfoResponse {
    def fromAddressAndInfo(address: InetSocketAddress, peerInfo: PeerInfo): PeerInfoResponse = PeerInfoResponse(
      address.toString,
      0,
      peerInfo.lastHandshake,
      peerInfo.peerSpec.nodeName,
      peerInfo.connectionType.map(_.toString)
    )
  }

  case class PeersStatusResponse(lastIncomingMessage: Long, currentSystemTime: Long)

  case class BlacklistedPeers(addresses: Seq[String])

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val encodePeerInfoResponse: Encoder[PeerInfoResponse] = deriveEncoder

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val encodeBlackListedPeers: Encoder[BlacklistedPeers] = deriveEncoder

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  implicit val encodePeersStatusResponse: Encoder[PeersStatusResponse] = deriveEncoder

}
