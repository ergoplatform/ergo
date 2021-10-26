package org.ergoplatform.network

import io.circe.{Encoder, Json}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.app.Version
import scorex.core.consensus.History.HistoryComparisonResult
import scorex.core.network.ConnectedPeer

/**
  * Container for status of another peer
  *
  * @param peer - peer information (public address, exposed info on operating mode etc)
  * @param status - peer's blockchain status (is it ahead or behind our, or on fork)
  * @param height - peer's height
  */
case class ErgoPeerStatus(peer: ConnectedPeer,
                          status: HistoryComparisonResult,
                          height: Height) {
  val mode: Option[ModeFeature] = ErgoPeerStatus.mode(peer)

  def version: Option[Version] = peer.peerInfo.map(_.peerSpec.protocolVersion)
}

object ErgoPeerStatus {

  import io.circe.syntax._

  /**
    * Helper method to get operating mode of the peer
    */
  def mode(peer: ConnectedPeer): Option[ModeFeature] = {
    peer.peerInfo.flatMap(_.peerSpec.features.collectFirst[ModeFeature]({ case mf: ModeFeature => mf}))
  }

  implicit val jsonEncoder: Encoder[ErgoPeerStatus] = { status: ErgoPeerStatus =>
    implicit val mfEnc: Encoder[ModeFeature] = ModeFeature.jsonEncoder

    Json.obj(
      "address" -> status.peer.peerInfo.get.peerSpec.address.getOrElse(_.toString, "N/A").asJson,
      "version" -> status.version.map(_.toString).getOrElse("N/A").asJson,
      "mode" -> status.mode.asJson,
      "status" -> status.status.toString.asJson,
      "height" -> status.height.asJson
    )
  }

}
