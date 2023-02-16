package org.ergoplatform.network

import io.circe.{Encoder, Json}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.app.Version
import scorex.core.consensus.PeerChainStatus
import scorex.core.network.ConnectedPeer
import org.ergoplatform.nodeView.history.ErgoHistory.Time

/**
  * Container for status of another peer
  *
  * @param peer - peer information (public address, exposed info on operating mode etc)
  * @param status - peer's blockchain status (is it ahead or behind our, or on fork)
  * @param height - peer's height
  * @param lastSyncSentTime - last time peer was asked to sync, None if never
  * @param lastSyncGetTime - last time peer received sync, None if never
  */
case class ErgoPeerStatus(peer: ConnectedPeer,
                          status: PeerChainStatus,
                          height: Height,
                          lastSyncSentTime: Option[Time],
                          lastSyncGetTime: Option[Time]) {
  val mode: Option[ModePeerFeature] = ErgoPeerStatus.mode(peer)

  def version: Option[Version] = peer.peerInfo.map(_.peerSpec.protocolVersion)
}

object ErgoPeerStatus {

  import io.circe.syntax._

  /**
    * Helper method to get operating mode of the peer
    */
  def mode(peer: ConnectedPeer): Option[ModePeerFeature] = {
    peer.peerInfo.flatMap(_.peerSpec.features.collectFirst[ModePeerFeature]({ case mf: ModePeerFeature => mf}))
  }

  implicit val jsonEncoder: Encoder[ErgoPeerStatus] = { status: ErgoPeerStatus =>
    implicit val mfEnc: Encoder[ModePeerFeature] = ModePeerFeature.jsonEncoder

    Json.obj(
      "address" -> status.peer.peerInfo.get.peerSpec.address.map(_.toString).getOrElse("N/A").asJson,
      "version" -> status.version.map(_.toString).getOrElse("N/A").asJson,
      "mode" -> status.mode.asJson,
      "status" -> status.status.toString.asJson,
      "height" -> status.height.asJson
    )
  }

}
