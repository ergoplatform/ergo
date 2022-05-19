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
import scorex.util.ScorexLogging

import scala.collection.mutable
import scala.concurrent.duration._
import scorex.core.utils.MapPimp

final case class ErgoSyncTracker(system: ActorSystem,
                                 networkSettings: NetworkSettings,
                                 timeProvider: TimeProvider)
 extends ScorexLogging {

  private val MinSyncInterval: FiniteDuration = 20.seconds
  private val SyncThreshold: FiniteDuration = 1.minute

  protected[network] val statuses: mutable.Map[ConnectedPeer, ErgoPeerStatus] =
    mutable.Map[ConnectedPeer, ErgoPeerStatus]()

  def fullInfo(): Iterable[ErgoPeerStatus] = statuses.values

  // returns diff
  def updateLastSyncGetTime(peer: ConnectedPeer): Long = {
    val prevSyncGetTime = statuses.get(peer).flatMap(_.lastSyncGetTime).getOrElse(0L)
    val currentTime = timeProvider.time()
    statuses.get(peer).foreach { status =>
      statuses.update(peer, status.copy(lastSyncGetTime = Option(currentTime)))
    }
    currentTime - prevSyncGetTime
  }

  def notSyncedOrOutdated(peer: ConnectedPeer): Boolean = {
    val peerOpt = statuses.get(peer)
    val notSyncedOrMissing = peerOpt.forall(_.lastSyncSentTime.isEmpty)
    val outdated =
      peerOpt
        .flatMap(_.lastSyncSentTime)
        .exists(syncTime => (timeProvider.time() - syncTime).millis > SyncThreshold)
    notSyncedOrMissing || outdated
  }

  def updateStatus(peer: ConnectedPeer, status: HistoryComparisonResult, height: Option[Height]): Unit = {
    val seniorsBefore = numOfSeniors()
    statuses.adjust(peer){
      case None =>
        ErgoPeerStatus(peer, status, height.getOrElse(ErgoHistory.EmptyHistoryHeight), None, None)
      case Some(existingPeer) =>
        existingPeer.copy(status = status, height = height.getOrElse(existingPeer.height))
    }

    val seniorsAfter = numOfSeniors()

    // todo: we should also send NoBetterNeighbour signal when all the peers around are not seniors initially
    if (seniorsBefore > 0 && seniorsAfter == 0) {
      log.info("Syncing is done, switching to stable regime")
      system.eventStream.publish(NoBetterNeighbour)
    }
    if (seniorsBefore == 0 && seniorsAfter > 0) {
      system.eventStream.publish(BetterNeighbourAppeared)
    }

    heights += (peer -> height.getOrElse(ErgoHistory.EmptyHistoryHeight))
  }

  /**
    * Get synchronization status for given connected peer
    */
  def getStatus(peer: ConnectedPeer): Option[HistoryComparisonResult] = {
    statuses.get(peer).map(_.status)
  }

  def clearStatus(remote: InetSocketAddress): Unit = {
    statuses.find(_._1.connectionId.remoteAddress == remote) match {
      case Some((peer, _)) => statuses -= peer
      case None => log.warn(s"Trying to clear status for $remote, but it is not found")
    }
  }

  def updateLastSyncSentTime(peer: ConnectedPeer): Unit = {
    val currentTime = timeProvider.time()
    statuses.get(peer).foreach { status =>
      statuses.update(peer, status.copy(lastSyncSentTime = Option(currentTime)))
    }
  }

  protected[network] def outdatedPeers: IndexedSeq[ConnectedPeer] = {
    val currentTime = timeProvider.time()
    statuses.filter { case (_, status) =>
      status.lastSyncSentTime.exists(syncTime => (currentTime - syncTime).millis > SyncThreshold)
    }.keys.toVector
  }

  def peersByStatus: Map[HistoryComparisonResult, Iterable[ConnectedPeer]] =
    statuses.groupBy(_._2.status).mapValues(_.keys).view.force

  protected def numOfSeniors(): Int = statuses.count(_._2.status == Older)

  def maxHeight(): Option[Int] = if(heights.nonEmpty) Some(heights.maxBy(_._2)._2) else None

  /**
    * Return the peers to which this node should send a sync signal, including:
    * outdated peers, if any, otherwise, all the peers with unknown status plus a random peer with
    * `Older` status.
    * Updates lastSyncSentTime for all returned peers as a side effect
    */
  def peersToSyncWith(): IndexedSeq[ConnectedPeer] = {
    val outdated = outdatedPeers
    val peers =
      if (outdated.nonEmpty) {
        outdated
      } else {
        val currentTime = timeProvider.time()
        val unknowns = statuses.filter(_._2.status == Unknown).toVector
        val forks = statuses.filter(_._2.status == Fork).toVector
        val elders = statuses.filter(_._2.status == Older).toVector
        val nonOutdated =
          (if (elders.nonEmpty) elders(scala.util.Random.nextInt(elders.size)) +: unknowns else unknowns) ++ forks
        nonOutdated.filter { case (_, status) =>
          (currentTime - status.lastSyncSentTime.getOrElse(0L)).millis >= MinSyncInterval
        }.map(_._1)
      }

    peers.foreach(updateLastSyncSentTime)
    peers
  }

  override def toString: String = {
    val now = System.currentTimeMillis()
    statuses.toSeq.sortBy(_._2.lastSyncSentTime.getOrElse(0L))(Ordering[Long].reverse).map {
      case (peer, status) =>
        (peer.connectionId.remoteAddress, statuses.get(peer), status.lastSyncSentTime.map(now - _))
    }.map { case (address, status, millisSinceLastSync) =>
      s"$address, height: ${status.map(_.height)}, status: ${status.map(_.status)}, lastSync: $millisSinceLastSync ms ago"
    }.mkString("\n")
  }
}
