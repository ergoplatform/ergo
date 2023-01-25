package org.ergoplatform.network


import org.ergoplatform.nodeView.history.{ErgoHistory, ErgoHistoryReader, ErgoSyncInfo, ErgoSyncInfoV1, ErgoSyncInfoV3, HeadersBasedSyncInfo}
import org.ergoplatform.nodeView.history.ErgoHistory.Height
import scorex.core.consensus.{Fork, Older, PeerChainStatus, Unknown}
import scorex.core.network.ConnectedPeer
import scorex.core.settings.NetworkSettings
import scorex.core.utils.TimeProvider
import scorex.util.ScorexLogging

import scala.collection.mutable
import scala.concurrent.duration._
import scorex.core.utils.MapPimp

final case class ErgoSyncTracker(networkSettings: NetworkSettings,
                                 timeProvider: TimeProvider) extends ScorexLogging {

  private val MinSyncInterval: FiniteDuration = 20.seconds
  private val SyncThreshold: FiniteDuration = 1.minute

  /**
    * After this timeout we clear peer's status
    */
  private val ClearThreshold: FiniteDuration = 3.minutes

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

  /**
    * Obtains peer sync status from `syncInfo` network message and updates statuses table with it
    *
    * @return (new peer status, should our node send sync message to the peer)
    */
  def updateStatus(peer: ConnectedPeer,
                   syncInfo: ErgoSyncInfo,
                   hr: ErgoHistoryReader): (PeerChainStatus, Boolean) = {
    val oldStatus = getStatus(peer).getOrElse(Unknown)
    val status = hr.compare(syncInfo)

    val height = syncInfo match {
      case _: ErgoSyncInfoV1 => None
      case otherVersion: HeadersBasedSyncInfo => otherVersion.height
    }
    val peerHeaders = syncInfo match {
      case v3: ErgoSyncInfoV3 => v3.headersRanges
      case _ => Seq.empty
    }
    val peerFullBlocks = syncInfo match {
      case v3: ErgoSyncInfoV3 => v3.fullBlocksRanges
      case _ => Seq.empty
    }
    updateStatus(peer, status, height, peerHeaders, peerFullBlocks)

    val syncSendNeeded = (oldStatus != status) || notSyncedOrOutdated(peer) || status == Older || status == Fork

    (status, syncSendNeeded)
  }

  def updateStatus(peer: ConnectedPeer,
                   status: PeerChainStatus,
                   height: Option[Height],
                   peerHeaders: Seq[(Height, Height)],
                   peerFullblocks: Seq[(Height, Height)]): Unit = {
    val seniorsBefore = numOfSeniors()
    statuses.adjust(peer){
      case None =>
        ErgoPeerStatus(peer, status, height.getOrElse(ErgoHistory.EmptyHistoryHeight), peerHeaders, peerFullblocks, None, None)
      case Some(existingPeer) =>
        existingPeer.copy(
          status = status,
          headersHeight = height.getOrElse(existingPeer.headersHeight),
          storedHeaders = peerHeaders,
          storedFullblocks = peerFullblocks
        )
    }

    val seniorsAfter = numOfSeniors()

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
  def getStatus(peer: ConnectedPeer): Option[PeerChainStatus] = {
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

  /**
    * Helper method to clear statuses of peers not updated for long enough
    */
  private[network] def clearOldStatuses(): Unit = {
    val currentTime = timeProvider.time()
    val peersToClear = statuses.filter { case (_, status) =>
      status.lastSyncSentTime.exists(syncTime => (currentTime - syncTime).millis > ClearThreshold)
    }.keys
    if (peersToClear.nonEmpty) {
      log.debug(s"Clearing stalled statuses for $peersToClear")
      // we set status to `Unknown` and reset peer's height
      peersToClear.foreach(p => updateStatus(p, Unknown, None, Seq(0 -> 0), Seq(0 -> 0)))
    }
  }

  private[network] def outdatedPeers: IndexedSeq[ConnectedPeer] = {
    val currentTime = timeProvider.time()
    statuses.filter { case (_, status) =>
      status.lastSyncSentTime.exists(syncTime => (currentTime - syncTime).millis > SyncThreshold)
    }.keys.toVector
  }

  /**
    * @return status -> peers dynamic index, so it calculates from stored peer -> status dictionary a reverse index
    */
  def peersByStatus: Map[PeerChainStatus, Seq[ConnectedPeer]] = {
    statuses.groupBy(_._2.status).mapValues(_.keys.toVector).view.force
  }

  protected def numOfSeniors(): Int = {
    statuses.count(_._2.status == Older)
  }

  def maxHeight(): Option[Int] = {
    if (statuses.nonEmpty) {
      Some(statuses.maxBy(_._2.headersHeight)._2.headersHeight)
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
    clearOldStatuses()
    val outdated = outdatedPeers
    val peers =
      if (outdated.nonEmpty) {
        outdated
      } else {
        val currentTime = timeProvider.time()
        val unknowns = statuses.filter(_._2.status == Unknown).toVector
        val forks = statuses.filter(_._2.status == Fork).toVector
        val elders = statuses.filter(_._2.status == Older).toVector

        val eldersAndUnknown = if (elders.nonEmpty) {
          elders(scala.util.Random.nextInt(elders.size)) +: unknowns
        } else {
          unknowns
        }
        val nonOutdated = eldersAndUnknown ++ forks
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
      s"$address, height: ${status.map(_.headersHeight)}, status: ${status.map(_.status)}, lastSync: $millisSinceLastSync ms ago"
    }.mkString("\n")
  }

}
