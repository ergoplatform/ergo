package scorex.core.network

import akka.actor.ActorRef
import io.circe.{Encoder, Json}
import org.ergoplatform.network.{ModePeerFeature, SyncV3PeerFeature}
import org.ergoplatform.network.peer.PeerInfo

/**
  * Peer connected to our node
  *
  * @param connectionId - connection address
  * @param handlerRef   - reference to PeerConnectionHandler that is responsible for communication with this peer
  * @param peerInfo     - information about this peer. May be None if peer is connected, but is not handshaked yet
  */
case class ConnectedPeer(connectionId: ConnectionId,
                         handlerRef: ActorRef,
                         peerInfo: Option[PeerInfo]) {

  override def hashCode(): Int = connectionId.remoteAddress.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case that: ConnectedPeer => this.connectionId.remoteAddress == that.connectionId.remoteAddress
    case _ => false
  }

  override def toString: String = s"ConnectedPeer(connection: $connectionId , " +
                                    s"remote version: ${peerInfo.map(_.peerSpec.protocolVersion)})"

  /**
    * Helper method to get operating mode of the peer
    */
  lazy val mode: Option[ModePeerFeature] = {
    peerInfo.flatMap(_.peerSpec.features.collectFirst[ModePeerFeature]({ case mf: ModePeerFeature => mf }))
  }

  /**
    * Whether the peer advertised SyncInfo V3 capability during handshake
    */
  lazy val supportsSyncV3: Boolean = {
    peerInfo.exists(_.peerSpec.features.collectFirst[SyncV3PeerFeature]({ case sf: SyncV3PeerFeature => sf }).isDefined)
  }

}

object ConnectedPeer {
  import io.circe.syntax._

  implicit val jsonEncoder: Encoder[ConnectedPeer] = { peer: ConnectedPeer =>
    val addressField = "address" -> peer.connectionId.remoteAddress.toString.asJson
    val optionalFields =
      List(
        peer.peerInfo.map(_.peerSpec.protocolVersion.toString).map("version" -> _.asJson),
        peer.peerInfo.map(_.lastStoredActivityTime).filter(_ != 0L).map("lastMessage" -> _.asJson),
      ).flatten
    val fields = addressField :: optionalFields
    Json.obj(fields:_*)
  }

}
