package org.ergoplatform.network.peer

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  ObjectInputStream,
  ObjectOutputStream
}
import java.net.{InetAddress, InetSocketAddress}
import org.ergoplatform.settings.ErgoSettings
import scorex.db.LDBFactory
import scorex.util.ScorexLogging

import scala.util.Random
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * In-memory peer database with .
  */
final class PeerDatabase(
  settings: ErgoSettings,
  private[peer] val maxKnownPeers: Int = PeerDatabase.MaxKnownPeers
) extends ScorexLogging {

  private val persistentStore = LDBFactory.createKvDb(s"${settings.directory}/peers")

  /**
    * banned peer ip -> ban expiration timestamp
    */
  private var blacklist = settings.scorexSettings.network.bannedPeers.map { addr =>
    addr.getAddress -> Long.MaxValue // Permanent ban
  }.toMap

  private var peers =
    loadPeers match {
      case Success(loadedPeers) =>
        loadedPeers
      case Failure(ex) =>
        log.error("Unable to load peers from database, loading from network only", ex)
        Map.empty[InetSocketAddress, PeerInfo]
    }

  private val EvictionSampleSize = 32

  /**
    * penalized peer ip -> (accumulated penalty score, last penalty timestamp)
    */
  private var penaltyBook = Map.empty[InetAddress, (Int, Long)]

  /*
   * Serialize object using standard Java serializer
   */
  private def serialize(obj: Object): Array[Byte] = {
    val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
    val oos                           = new ObjectOutputStream(stream)
    oos.writeObject(obj)
    oos.close()
    stream.toByteArray
  }

  /*
   * Deserialize object using standard Java serializer
   */
  private def deserialize(bytes: Array[Byte]): Object = {
    val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
    ois.readObject()
  }

  /*
   * Load peers from persistent storage
   */
  private def loadPeers: Try[Map[InetSocketAddress, PeerInfo]] = Try {
    var peers = Map.empty[InetSocketAddress, PeerInfo]
    for ((addr, peer) <- persistentStore.getAll) {
      val address  = deserialize(addr).asInstanceOf[InetSocketAddress]
      val peerInfo = PeerInfoSerializer.parseBytes(peer)
      peers += address -> peerInfo
    }
    peers
  }

  def get(peer: InetSocketAddress): Option[PeerInfo] = peers.get(peer)

  def addOrUpdateKnownPeer(
    peerInfo: PeerInfo,
    connectedPeers: Set[InetSocketAddress] = Set.empty
  ): Unit = {
    if (!peerInfo.peerSpec.declaredAddress.exists(x => isBlacklisted(x.getAddress))) {
      peerInfo.peerSpec.address.foreach { address =>
        if (peers.contains(address)) {
          log.debug(s"Updating peer info for $address")
          updatePeer(address, peerInfo)
        } else if (peers.size < maxKnownPeers ||
                   makeRoomForPeer(peerInfo.lastHandshake, connectedPeers)) {
          log.debug(s"Adding peer info for $address")
          updatePeer(address, peerInfo)
        } else {
          log.debug(s"Peer database is full, ignoring $address")
        }
      }
    }
  }

  private def updatePeer(address: InetSocketAddress, peerInfo: PeerInfo): Unit = {
    peers += address -> peerInfo
    persistentStore.insert(serialize(address), PeerInfoSerializer.toBytes(peerInfo))
  }

  /**
    * Evict the oldest known peer (by lastHandshake) from a random sample to make room
    * for a new peer, but never evict a currently connected peer.
    *
    * @param candidateHandshake - lastHandshake of the peer we want to insert
    * @return true if room was made, false otherwise
    */
  private def makeRoomForPeer(
    candidateHandshake: Long,
    connectedPeers: Set[InetSocketAddress]
  ): Boolean = {
    val oldest = randomPeerSample(EvictionSampleSize).foldLeft(
      Option.empty[(InetSocketAddress, PeerInfo)]
    ) { (acc, entry) =>
      val (address, info) = entry
      if (connectedPeers.contains(address)) {
        acc
      } else {
        acc match {
          case Some((_, oldestInfo)) if oldestInfo.lastHandshake <= info.lastHandshake =>
            acc
          case _ => Some(entry)
        }
      }
    }
    oldest match {
      case Some((oldestAddress, oldestInfo))
          if candidateHandshake > oldestInfo.lastHandshake =>
        log.info(
          s"Evicting peer $oldestAddress with lastHandshake " +
          s"${oldestInfo.lastHandshake} to make room for a newer peer"
        )
        remove(oldestAddress)
        true
      case _ =>
        false
    }
  }

  /**
    * Select a uniform random sample of up to `sampleSize` peers using reservoir sampling.
    */
  private def randomPeerSample(sampleSize: Int): Seq[(InetSocketAddress, PeerInfo)] = {
    if (peers.isEmpty) {
      Seq.empty
    } else {
      val start  = Random.nextInt(peers.size)
      val finish = math.min(start + sampleSize, peers.size)
      peers.slice(start, finish).toSeq
    }
  }

  /**
    * Remove peers whose lastHandshake is older than 60 days, excluding connected peers.
    */
  def removeOldPeers(connectedPeers: Set[InetSocketAddress] = Set.empty): Unit = {
    val cutoff = System.currentTimeMillis() - PeerDatabase.KnownPeerMaxAgeMs
    val toRemove = peers.collect {
      case (address, info)
          if !connectedPeers.contains(address) && info.lastHandshake < cutoff =>
        address
    }
    toRemove.foreach(remove)
  }

  def addToBlacklist(socketAddress: InetSocketAddress, penaltyType: PenaltyType): Unit = {
    remove(socketAddress)
    Option(socketAddress.getAddress).foreach { address =>
      penaltyBook -= address
      if (!blacklist.keySet.contains(address)) {
        blacklist += address -> (System.currentTimeMillis() + penaltyDuration(
          penaltyType
        ))
      } else {
        log.warn(s"${address.toString} is already blacklisted")
      }
    }
  }

  private def removeFromBlacklist(address: InetAddress): Unit = {
    log.info(s"$address removed from blacklist")
    blacklist -= address
  }

  def remove(address: InetSocketAddress): Unit = {
    peers -= address
    persistentStore.remove(Array(serialize(address)))
  }

  def knownPeers: Map[InetSocketAddress, PeerInfo] = peers

  /**
    * Close the underlying persistent store.
    */
  def close(): Unit = persistentStore.close()

  def blacklistedPeers: Seq[InetAddress] =
    blacklist.map {
      case (address, bannedTill) =>
        checkBanned(address, bannedTill)
        address
    }.toSeq

  def isEmpty: Boolean = peers.isEmpty

  def isBlacklisted(address: InetAddress): Boolean =
    blacklist.get(address).exists(checkBanned(address, _))

  def isBlacklisted(address: InetSocketAddress): Boolean =
    Option(address.getAddress).exists(isBlacklisted)

  /**
    * Registers a new penalty in the penalty book.
    *
    * @return - `true` if penalty threshold is reached, `false` otherwise.
    */
  def penalize(socketAddress: InetSocketAddress, penaltyType: PenaltyType): Boolean =
    Option(socketAddress.getAddress).exists { address =>
      val currentTime                      = System.currentTimeMillis()
      val safeInterval                     = settings.scorexSettings.network.penaltySafeInterval.toMillis
      val (penaltyScoreAcc, lastPenaltyTs) = penaltyBook.getOrElse(address, (0, 0L))
      val applyPenalty                     = currentTime - lastPenaltyTs - safeInterval > 0 || penaltyType.isPermanent
      val newPenaltyScore = if (applyPenalty) {
        penaltyScoreAcc + penaltyScore(penaltyType)
      } else {
        penaltyScoreAcc
      }

      if (newPenaltyScore > settings.scorexSettings.network.penaltyScoreThreshold) {
        true
      } else {
        penaltyBook += address -> (newPenaltyScore -> System.currentTimeMillis())
        false
      }
    }

  /**
    * Currently accumulated penalty score for a given address.
    */
  def penaltyScore(address: InetAddress): Int =
    penaltyBook.getOrElse(address, (0, 0L))._1

  def penaltyScore(socketAddress: InetSocketAddress): Int =
    Option(socketAddress.getAddress).map(penaltyScore).getOrElse(0)

  private def checkBanned(address: InetAddress, bannedTill: Long): Boolean = {
    val stillBanned = System.currentTimeMillis() < bannedTill
    if (!stillBanned) removeFromBlacklist(address)
    stillBanned
  }

  private def penaltyScore(penaltyType: PenaltyType): Int =
    penaltyType match {
      case PenaltyType.NonDeliveryPenalty =>
        PenaltyType.NonDeliveryPenalty.penaltyScore
      case PenaltyType.MisbehaviorPenalty =>
        PenaltyType.MisbehaviorPenalty.penaltyScore
      case PenaltyType.SpamPenalty =>
        PenaltyType.SpamPenalty.penaltyScore
      case PenaltyType.PermanentPenalty =>
        PenaltyType.PermanentPenalty.penaltyScore
    }

  private def penaltyDuration(penalty: PenaltyType): Long =
    penalty match {
      case PenaltyType.NonDeliveryPenalty | PenaltyType.MisbehaviorPenalty |
          PenaltyType.SpamPenalty =>
        settings.scorexSettings.network.temporalBanDuration.toMillis
      case PenaltyType.PermanentPenalty =>
        (360 * 10).days.toMillis
    }
}

object PeerDatabase {

  /**
    * Hardcoded cap on the total number of known peers.
    */
  val MaxKnownPeers: Int = 131072

  /**
    * Hardcoded maximum age (60 days) for a known peer's lastHandshake.
    */
  val KnownPeerMaxAgeMs: Long = 60L * 24 * 60 * 60 * 1000

  /**
    * Hardcoded interval (24 hours) between cleanup runs.
    */
  val KnownPeerCleanupIntervalMs: Long = 24L * 60 * 60 * 1000

}
