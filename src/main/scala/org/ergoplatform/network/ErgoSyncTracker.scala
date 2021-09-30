package org.ergoplatform.network

import akka.actor.{ActorContext, ActorRef}
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.consensus.History.Unknown
import scorex.core.network.{ConnectedPeer, SyncTracker}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.TimeProvider

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._


final case class ErgoSyncTracker( nvsRef: ActorRef,
                                  context: ActorContext,
                                  networkSettings: NetworkSettings,
                                  timeProvider: TimeProvider)(implicit ec: ExecutionContext)
 extends SyncTracker(nvsRef, context, networkSettings, timeProvider)(ec) {

  val heights: mutable.Map[ConnectedPeer, Height] = mutable.Map[ConnectedPeer, Height]()

  def fullInfo(): Iterable[ErgoPeerStatus] = {
    statuses.keys.toSeq.map { cp =>
      val height = heights.getOrElse(cp, ErgoHistory.EmptyHistoryHeight)
      val status = statuses.getOrElse(cp, Unknown)
      val lastUpdate = lastSyncSentTime.getOrElse(cp, 0L)
      ErgoPeerStatus(cp, status, height, lastUpdate)
    }
  }

  def isOutdated(peer: ConnectedPeer): Boolean = {
    heights.get(peer).isEmpty ||
      (timeProvider.time() - lastSyncSentTime.getOrElse(peer, 0L)).millis > maxInterval()
  }

  private[network] def updateHeight(peer: ConnectedPeer, height: Height): Unit = {
    heights += peer -> height
  }

}
