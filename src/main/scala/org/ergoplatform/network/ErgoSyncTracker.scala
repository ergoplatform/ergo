package org.ergoplatform.network


import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader, ErgoSyncInfo, ErgoSyncInfoV1, ErgoSyncInfoV2}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.consensus.{Fork, HistoryComparisonResult, Older, Unknown}
import scorex.core.network.ConnectedPeer
import scorex.core.settings.NetworkSettings
import scorex.core.utils.TimeProvider
import scorex.util.ScorexLogging

import scala.collection.mutable
import scala.concurrent.duration._
import scorex.core.utils.MapPimp

final case class ErgoSyncTracker(networkSettings: NetworkSettings, timeProvider: TimeProvider) extends ScorexLogging {

  private val MinSyncInterval: FiniteDuration = 20.seconds
  private val SyncThreshold: FiniteDuration = 1.minute

  private[network] val statuses = mutable.Map[ConnectedPeer, ErgoPeerStatus]()

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

  def updateStatus(peer: ConnectedPeer,
                   syncInfo: ErgoSyncInfo,
                   hr: ErgoHistoryReader): (HistoryComparisonResult, Boolean) = {
    val oldStatus = getStatus(peer).getOrElse(Unknown)
    val status = hr.compare(syncInfo)

    val height = syncInfo match {
      case _: ErgoSyncInfoV1 => None
      case sv2: ErgoSyncInfoV2 => sv2.height
    }
    updateStatus(peer, status, height)

    val syncSendNeeded = (oldStatus != status) || notSyncedOrOutdated(peer) || status == Older || status == Fork

    (status, syncSendNeeded)
  }

  def updateStatus(peer: ConnectedPeer,
                   status: HistoryComparisonResult,
                   height: Option[Height]): Unit = {
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
      // todo: update neighbours status ?
    }
    if (seniorsBefore == 0 && seniorsAfter > 0) {
      // todo: update neighbours status?
    }
  }

  /**
    * Get synchronization status for given connected peer
    */
  def getStatus(peer: ConnectedPeer): Option[HistoryComparisonResult] = {
    statuses.get(peer).map(_.status)
  }

  def clearStatus(connectedPeer: ConnectedPeer): Unit = {
    statuses.find(_._1 == connectedPeer) match {
      case Some((peer, _)) =>
        statuses -= peer
      case None =>
        log.warn(s"Trying to clear status for $connectedPeer, but it is not found")
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

  /**
    * @return status -> peers index
    */
  def peersByStatus: Map[HistoryComparisonResult, Seq[ConnectedPeer]] = {
    statuses.groupBy(_._2.status).mapValues(_.keys.toVector).view.force
  }

  protected def numOfSeniors(): Int = {
    statuses.count(_._2.status == Older)
  }

  def maxHeight(): Option[Int] = {
    if (statuses.nonEmpty) {
      Some(statuses.maxBy(_._2.height)._2.height)
    } else {
      None
    }
  }

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
