package org.ergoplatform.network

import akka.actor.{ActorContext, ActorRef}
import io.circe.{Encoder, Json}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.network.{ConnectedPeer, SyncTracker}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.TimeProvider

import scala.collection.mutable
import scala.concurrent.ExecutionContext

case class ErgoPeerStatus(peer: ConnectedPeer, height: Height) {
  val mode: Option[ModeFeature] = ErgoPeerStatus.mode(peer)
}

object ErgoPeerStatus {
  import io.circe.syntax._

  def mode(peer: ConnectedPeer): Option[ModeFeature] = peer.peerInfo.flatMap(_.peerSpec.features.collectFirst[ModeFeature]({
    case mf: ModeFeature => mf
  }))

  implicit val jsonEncoder: Encoder[ErgoPeerStatus] = {status: ErgoPeerStatus =>
    Json.obj(
      "address" -> status.peer.peerInfo.get.peerSpec.address.toString.asJson,
      "mode" -> status.mode.toString.asJson,
      "height" -> status.height.asJson
    )
  }
}

class ErgoSyncTracker(nvsRef: ActorRef,
                      context: ActorContext,
                      networkSettings: NetworkSettings,
                      timeProvider: TimeProvider)(implicit ec: ExecutionContext)
 extends SyncTracker(nvsRef, context, networkSettings, timeProvider)(ec) {

  val heights = mutable.Map[ConnectedPeer, Height]()

  def fullInfo(): Iterable[ErgoPeerStatus] = {
    statuses.keys.toSeq.flatMap { cp =>
      heights.get(cp).map(h => ErgoPeerStatus(cp, h))
    }
  }

  def updateHeight(peer: ConnectedPeer, height: Height): Unit = {
    heights += peer -> height
  }

}
