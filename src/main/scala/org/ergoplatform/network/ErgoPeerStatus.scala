package org.ergoplatform.network

import io.circe.{Encoder, Json}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.consensus.History.HistoryComparisonResult
import scorex.core.network.ConnectedPeer

case class ErgoPeerStatus(peer: ConnectedPeer, status: HistoryComparisonResult, height: Height, lastUpdate: Long) {
  val mode: Option[ModeFeature] = ErgoPeerStatus.mode(peer)
}

object ErgoPeerStatus {

  import io.circe.syntax._

  def mode(peer: ConnectedPeer): Option[ModeFeature] = {
    peer.peerInfo.flatMap(_.peerSpec.features.collectFirst[ModeFeature]({ case mf: ModeFeature => mf}))
  }

  implicit val jsonEncoder: Encoder[ErgoPeerStatus] = { status: ErgoPeerStatus =>
    implicit val mfEnc: Encoder[ModeFeature] = ModeFeature.jsonEncoder

    Json.obj(
      "address" -> status.peer.peerInfo.get.peerSpec.address.toString.asJson,
      "mode" -> status.mode.asJson,
      "status" -> status.status.toString.asJson,
      "height" -> status.height.asJson,
      "lastUpdate" -> status.lastUpdate.asJson
    )
  }

}
