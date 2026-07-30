package org.ergoplatform.network.peer

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  ObjectInputStream,
  ObjectOutputStream
}
import java.net.{InetAddress, InetSocketAddress}
import java.util.concurrent.ThreadLocalRandom
import org.ergoplatform.settings.ErgoSettings
import scorex.db.LDBFactory
import scorex.util.ScorexLogging

import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
  * In-memory peer database with temporal blacklisting and peer count cap.
  */
final class PeerDatabase(
  settings: ErgoSettings,
  private[peer] val maxKnownPeers: Int = PeerDatabase.MaxKnownPeers
) extends ScorexLogging {

  private val persistentStore = LDBFactory.createKvDb(s"${settings.directory}/peers")

  /**
    * Serialized peer info size must stay below this bound. The value is twice
    * the maximum handshake size (8KB) to leave a comfortable margin while still
    * preventing a single malformed/crafted entry from consuming a lot of memory.
    */
  private val MaxSerializedPeerInfoSize = 16384

  /**
    * Serialized peer address (InetSocketAddress Java serialization) size bound.
    * Legitimate hostnames can be up to 253 characters, so leave plenty of headroom.
    */
  private val MaxSerializedPeerAddressSize = 1024

  private case class LoadedPeer(
    lastHandshake: Long,
    address: InetSocketAddress,
    peerInfo: PeerInfo,
    keyBytes: Array[Byte]
  )

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
   * Load peers from persistent storage.
   *
   * Enforces the in-memory cap and per-entry size limits at load time so a
   * pre-existing or malformed DB cannot OOM the node on startup. Oversized or
   * excess entries are dropped from the loaded set (and excess keys are removed
   * from the store to keep the DB trimmed).
   */
  private def loadPeers: Try[Map[InetSocketAddress, PeerInfo]] = Try {
    val (oversizedKeysRev, validPeersRev) =
      persistentStore.getAll.toVector.foldLeft(
        (List.empty[Array[Byte]], List.empty[LoadedPeer])
      ) { case ((badKeys, goodPeers), (addr, peer)) =>
        if (addr.length > MaxSerializedPeerAddressSize || peer.length > MaxSerializedPeerInfoSize) {
          log.warn(
            s"Dropping oversized peer entry from database: key=${addr.length} bytes, " +
            s"value=${peer.length} bytes"
          )
          (addr :: badKeys, goodPeers)
        } else {
          val addressTry  = Try(deserialize(addr).asInstanceOf[InetSocketAddress])
          val peerInfoTry = PeerInfoSerializer.parseBytesTry(peer)
          (addressTry, peerInfoTry) match {
            case (Success(address), Success(peerInfo)) =>
              val loaded = LoadedPeer(peerInfo.lastHandshake, address, peerInfo, addr)
              (badKeys, loaded :: goodPeers)
            case _ =>
              log.warn(s"Unable to deserialize peer entry from database, skipping it")
              (badKeys, goodPeers)
          }
        }
      }

    val oversizedKeys = oversizedKeysRev.reverse
    val validPeers    = validPeersRev.reverse

    val sorted       = validPeers.sortBy(_.lastHandshake)(Ordering[Long].reverse)
    val (kept, drop) = sorted.splitAt(maxKnownPeers)
    val keysToRemove = oversizedKeys ++ drop.map(_.keyBytes)

    flushKeysToRemove(keysToRemove.toArray)
    kept.map(p => p.address -> p.peerInfo).toMap
  }

  private def flushKeysToRemove(keys: Array[Array[Byte]]): Unit = {
    if (keys.nonEmpty) {
      persistentStore.remove(keys) match {
        case Success(_) => // ok
        case Failure(ex) =>
          log.warn("Unable to remove dropped peer entries from persistent store", ex)
      }
    }
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
    val EvictionSampleSize = 16
    val oldest = randomPeerSample(EvictionSampleSize)
      .filterNot { case (address, _) => connectedPeers.contains(address) }
      .sortBy(_._2.lastHandshake)
      .headOption

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
    * Select a small random slice of known peers to consider for eviction.
    * The slice is contiguous in the map's iteration order and bounded by
    * `sampleSize`, so the cost stays low even when the peer set is large.
    */
  private def randomPeerSample(sampleSize: Int): Seq[(InetSocketAddress, PeerInfo)] = {
    if (peers.isEmpty) {
      Seq.empty
    } else {
      val sample = math.min(sampleSize, peers.size)
      val start  = ThreadLocalRandom.current().nextInt(peers.size - sample + 1)
      peers.slice(start, start + sample).toSeq
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
  val MaxKnownPeers: Int = 32768

  /**
    * Hardcoded maximum age (60 days) for a known peer's lastHandshake.
    */
  val KnownPeerMaxAgeMs: Long = 60.days.toMillis

  /**
    * Hardcoded interval (24 hours) between cleanup runs.
    */
  val KnownPeerCleanupIntervalMs: Long = 24.hours.toMillis

}
