package org.ergoplatform.network

import io.circe.{Encoder, Json}
import org.ergoplatform.consensus.PeerChainStatus
import org.ergoplatform.nodeView.history.ErgoHistoryUtils._
import scorex.core.network.ConnectedPeer

/**
  * Container for status of another peer
  *
  * @param peer - peer information (public address, exposed info on operating mode etc)
  * @param status - peer's blockchain status (is it ahead or behind our, or on fork)
  * @param headersHeight - peer's headers height
  * @param storedHeaders - ranges of headers stored by the peer (start, end inclusive)
  * @param storedFullblocks - ranges of full blocks stored by the peer (start, end inclusive)
  * @param lastSyncSentTime - last time peer was asked to sync, None if never
  * @param lastSyncGetTime - last time peer received sync, None if never
  */
case class ErgoPeerStatus(peer: ConnectedPeer,
                          status: PeerChainStatus,
                          headersHeight: Height,
                          storedHeaders: Seq[(Height, Height)],
                          storedFullblocks: Seq[(Height, Height)],
                          lastSyncSentTime: Option[Time],
                          lastSyncGetTime: Option[Time]) {
  val mode: Option[ModePeerFeature] = peer.mode

  def height: Height = headersHeight

  def version: Option[Version] = peer.peerInfo.map(_.peerSpec.protocolVersion)

  /**
    * Whether the peer stores a header at the given height h
    */
  def hasHeaderAt(h: Height): Boolean = {
    if (storedHeaders.nonEmpty) {
      storedHeaders.exists { case (start, end) => h >= start && h <= end }
    } else {
      mode.forall(_.allHeadersAvailable) && (headersHeight <= 0 || h <= headersHeight)
    }
  }

  /**
    * Whether the peer stores a full block at the given height h
    */
  def hasFullBlockAt(h: Height): Boolean = {
    if (storedFullblocks.nonEmpty) {
      storedFullblocks.exists { case (start, end) => h >= start && h <= end }
    } else {
      mode match {
        case Some(m) if m.allBlocksAvailable =>
          headersHeight <= 0 || h <= headersHeight
        case Some(m) if m.blocksToKeep > 0 && headersHeight > 0 =>
          val minHeight = Math.max(1, headersHeight - m.blocksToKeep + 1)
          h >= minHeight && h <= headersHeight
        case None =>
          headersHeight <= 0 || h <= headersHeight
        case _ =>
          false
      }
    }
  }
}

object ErgoPeerStatus {

  import io.circe.syntax._

  def apply(peer: ConnectedPeer,
            status: PeerChainStatus,
            headersHeight: Height,
            lastSyncSentTime: Option[Time],
            lastSyncGetTime: Option[Time]): ErgoPeerStatus = {
    new ErgoPeerStatus(peer, status, headersHeight, Seq.empty, Seq.empty, lastSyncSentTime, lastSyncGetTime)
  }

  implicit val jsonEncoder: Encoder[ErgoPeerStatus] = { status: ErgoPeerStatus =>
    implicit val mfEnc: Encoder[ModePeerFeature] = ModePeerFeature.jsonEncoder

    Json.obj(
      "address" -> status.peer.peerInfo.get.peerSpec.address.map(_.toString).getOrElse("N/A").asJson,
      "version" -> status.version.map(_.toString).getOrElse("N/A").asJson,
      "mode" -> status.mode.asJson,
      "status" -> status.status.toString.asJson,
      "height" -> status.headersHeight.asJson
    )
  }

}
