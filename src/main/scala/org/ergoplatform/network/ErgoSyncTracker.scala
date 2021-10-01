package org.ergoplatform.network

import java.net.InetSocketAddress

import akka.actor.ActorContext
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.consensus.History.{Fork, HistoryComparisonResult, Older, Unknown}
import scorex.core.network.NodeViewSynchronizer.Events.{BetterNeighbourAppeared, NoBetterNeighbour}
import scorex.core.network.{ConnectedPeer, SyncTracker}
import scorex.core.settings.NetworkSettings
import scorex.core.utils.TimeProvider
import scorex.core.utils.TimeProvider.Time

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._


final case class ErgoSyncTracker( context: ActorContext,
                                  networkSettings: NetworkSettings,
                                  timeProvider: TimeProvider)(implicit ec: ExecutionContext)
 extends SyncTracker(null, context, networkSettings, timeProvider)(ec) {

  val heights: mutable.Map[ConnectedPeer, Height] = mutable.Map[ConnectedPeer, Height]()

  val MinSyncInterval: FiniteDuration = 20.seconds
  val SyncThreshold: FiniteDuration = 1.minute

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
      (timeProvider.time() - lastSyncSentTime.getOrElse(peer, 0L)).millis > SyncThreshold
  }

  private[network] def updateHeight(peer: ConnectedPeer, height: Height): Unit = {
    heights += peer -> height
  }

  // var schedule: Option[Cancellable] = None

  override protected val statuses = mutable.Map[ConnectedPeer, HistoryComparisonResult]()
  override protected val lastSyncSentTime = mutable.Map[ConnectedPeer, Time]()

 // override protected var lastSyncInfoSentTime: Time = 0L

 // override protected var stableSyncRegime = false

  override def scheduleSendSyncInfo(): Unit = {
    log.error("scheduleSendSyncInfo called")
  }

  override def maxInterval(): FiniteDuration = {
    log.error("maxInterval called")
    SyncThreshold
  }

  override def minInterval(): FiniteDuration = {
    log.error("minInterval called")
    SyncThreshold
  }

  /**
    * Get synchronization status for given connected peer
    */
  override def getStatus(peer: ConnectedPeer): Option[HistoryComparisonResult] = {
    statuses.get(peer)
  }

  override def updateStatus(peer: ConnectedPeer, status: HistoryComparisonResult): Unit = {
    val seniorsBefore = numOfSeniors()
    statuses += peer -> status
    val seniorsAfter = numOfSeniors()

    // todo: we should also send NoBetterNeighbour signal when all the peers around are not seniors initially
    if (seniorsBefore > 0 && seniorsAfter == 0) {
      log.info("Syncing is done, switching to stable regime")
      stableSyncRegime = true
      context.system.eventStream.publish(NoBetterNeighbour)
    }
    if (seniorsBefore == 0 && seniorsAfter > 0) {
      context.system.eventStream.publish(BetterNeighbourAppeared)
    }
  }

  //todo: combine both?
  override def clearStatus(remote: InetSocketAddress): Unit = {
    statuses.find(_._1.connectionId.remoteAddress == remote) match {
      case Some((peer, _)) => statuses -= peer
      case None => log.warn(s"Trying to clear status for $remote, but it is not found")
    }

    lastSyncSentTime.find(_._1.connectionId.remoteAddress == remote) match {
      case Some((peer, _)) => statuses -= peer
      case None => log.warn(s"Trying to clear last sync time for $remote, but it is not found")
    }
  }

  override def updateLastSyncSentTime(peer: ConnectedPeer): Unit = {
    val currentTime = timeProvider.time()
    lastSyncSentTime(peer) = currentTime
    lastSyncInfoSentTime = currentTime
  }

  override def elapsedTimeSinceLastSync(): Long = timeProvider.time() - lastSyncInfoSentTime

  override protected def outdatedPeers(): Seq[ConnectedPeer] =
    lastSyncSentTime.filter(t => (timeProvider.time() - t._2).millis > SyncThreshold).keys.toSeq


  override def peersByStatus: Map[HistoryComparisonResult, Iterable[ConnectedPeer]] =
    statuses.groupBy(_._2).mapValues(_.keys).view.force

  override protected def numOfSeniors(): Int = statuses.count(_._2 == Older)

  /**
    * Return the peers to which this node should send a sync signal, including:
    * outdated peers, if any, otherwise, all the peers with unknown status plus a random peer with
    * `Older` status.
    * Updates lastSyncSentTime for all returned peers as a side effect
    */
  override def peersToSyncWith(): Seq[ConnectedPeer] = {
    val outdated = outdatedPeers()
    val peers =
      if (outdated.nonEmpty) outdated
      else {
        val unknowns = statuses.filter(_._2 == Unknown).keys.toIndexedSeq
        val forks = statuses.filter(_._2 == Fork).keys.toIndexedSeq
        val elders = statuses.filter(_._2 == Older).keys.toIndexedSeq
        val nonOutdated =
          (if (elders.nonEmpty) elders(scala.util.Random.nextInt(elders.size)) +: unknowns else unknowns) ++ forks
        nonOutdated.filter(p => (timeProvider.time() - lastSyncSentTime.getOrElse(p, 0L)).millis >= MinSyncInterval)
      }

    peers.foreach(updateLastSyncSentTime)
    peers
  }

}
