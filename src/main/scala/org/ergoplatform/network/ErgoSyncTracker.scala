package org.ergoplatform.network

import java.net.InetSocketAddress
import akka.actor.ActorSystem
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.consensus.History.{Fork, HistoryComparisonResult, Older, Unknown}
import org.ergoplatform.network.ErgoNodeViewSynchronizer.Events.{BetterNeighbourAppeared, NoBetterNeighbour}
import scorex.core.network.ConnectedPeer
import scorex.core.settings.NetworkSettings
import scorex.core.utils.TimeProvider
import scorex.core.utils.TimeProvider.Time
import scorex.util.ScorexLogging

import scala.collection.mutable
import scala.collection.immutable.Set
import scala.concurrent.duration._


final case class ErgoSyncTracker(system: ActorSystem,
                                 networkSettings: NetworkSettings,
                                 timeProvider: TimeProvider)
 extends ScorexLogging {

  val MinSyncInterval: FiniteDuration = 20.seconds
  val SyncThreshold: FiniteDuration = 1.minute

  private[network] val statuses = mutable.Map[ConnectedPeer, ErgoPeerStatus]()
  private[network] val lastSyncSentTime = mutable.Map[ConnectedPeer, Time]()

  protected var lastSyncInfoSentTime: Time = 0L

  val heights: mutable.Map[ConnectedPeer, Height] = mutable.Map[ConnectedPeer, Height]()


  def fullInfo(): Iterable[ErgoPeerStatus] = statuses.values

  def isOutdated(peer: ConnectedPeer): Boolean = {
    (timeProvider.time() - lastSyncSentTime.getOrElse(peer, 0L)).millis > SyncThreshold
  }

  def updateStatus(peer: ConnectedPeer, status: HistoryComparisonResult, height: Option[Height]): Unit = {
    val seniorsBefore = numOfSeniors()
    statuses += peer -> ErgoPeerStatus(peer, status, height.getOrElse(ErgoHistory.EmptyHistoryHeight))
    val seniorsAfter = numOfSeniors()

    // todo: we should also send NoBetterNeighbour signal when all the peers around are not seniors initially
    if (seniorsBefore > 0 && seniorsAfter == 0) {
      log.info("Syncing is done, switching to stable regime")
      system.eventStream.publish(NoBetterNeighbour)
    }
    if (seniorsBefore == 0 && seniorsAfter > 0) {
      system.eventStream.publish(BetterNeighbourAppeared)
    }
  }

  /**
    * Get synchronization status for given connected peer
    */
  def getStatus(peer: ConnectedPeer): Option[HistoryComparisonResult] = {
    statuses.get(peer).map(_.status)
  }

  //todo: combine both?
  def clearStatus(remote: InetSocketAddress): Unit = {
    statuses.find(_._1.connectionId.remoteAddress == remote) match {
      case Some((peer, _)) => statuses -= peer
      case None => log.warn(s"Trying to clear status for $remote, but it is not found")
    }

    lastSyncSentTime.find(_._1.connectionId.remoteAddress == remote) match {
      case Some((peer, _)) => lastSyncSentTime -= peer
      case None => log.warn(s"Trying to clear last sync time for $remote, but it is not found")
    }
  }

  def updateLastSyncSentTime(peer: ConnectedPeer): Unit = {
    val currentTime = timeProvider.time()
    lastSyncSentTime(peer) = currentTime
    lastSyncInfoSentTime = currentTime
  }

  protected def outdatedPeers(): Seq[ConnectedPeer] =
    lastSyncSentTime.filter(t => (timeProvider.time() - t._2).millis > SyncThreshold).keys.toSeq


  def peersByStatus: Map[HistoryComparisonResult, Set[ConnectedPeer]] =
    statuses.groupBy(_._2.status).mapValues(_.keys.toSet).view.force

  protected def numOfSeniors(): Int = statuses.count(_._2.status == Older)

  /**
    * Return the peers to which this node should send a sync signal, including:
    * outdated peers, if any, otherwise, all the peers with unknown status plus a random peer with
    * `Older` status.
    * Updates lastSyncSentTime for all returned peers as a side effect
    */
  def peersToSyncWith(): Seq[ConnectedPeer] = {
    val outdated = outdatedPeers()
    val peers =
      if (outdated.nonEmpty) {
        outdated
      } else {
        val unknowns = statuses.filter(_._2.status == Unknown).keys.toSeq
        val forks = statuses.filter(_._2.status == Fork).keys
        val elders = statuses.filter(_._2.status == Older).keys.toSeq
        val nonOutdated =
          (if (elders.nonEmpty) elders(scala.util.Random.nextInt(elders.size)) +: unknowns else unknowns) ++ forks
        nonOutdated.filter(p => (timeProvider.time() - lastSyncSentTime.getOrElse(p, 0L)).millis >= MinSyncInterval)
      }

    peers.foreach(updateLastSyncSentTime)
    peers
  }

}
