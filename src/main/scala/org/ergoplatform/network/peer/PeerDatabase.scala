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

import scala.collection.mutable
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
   * Number of store keys removed per batch while loading peers, so that
   * cleanup of a severely oversized database does not build one huge in-memory batch.
   */
  private val RemovalBatchSize = 1024

  /*
   * Load peers from persistent storage.
   *
   * Retention is driven by recency, not store iteration order: while streaming over
   * the store we keep at most `maxKnownPeers` entries with the newest `lastHandshake`
   * values, using a bounded min-heap keyed by `lastHandshake`, so peak extra memory
   * stays O(maxKnownPeers + RemovalBatchSize) and an oversized or malformed database
   * cannot OOM the node on startup.
   *
   * Oversized, unparseable, duplicated (same address, older handshake) and excess
   * entries are physically removed from the store in bounded batches during the scan.
   */
  private def loadPeers: Try[Map[InetSocketAddress, PeerInfo]] = Try {
    val kept = mutable.HashMap.empty[InetSocketAddress, LoadedPeer]
    // min-heap by lastHandshake (oldest at the head); may contain stale entries
    // superseded by a newer record for the same address, cleaned lazily on eviction
    val oldestFirst =
      mutable.PriorityQueue.empty[LoadedPeer](Ordering.by[LoadedPeer, Long](_.lastHandshake).reverse)
    val keysToRemove  = mutable.ArrayBuffer.empty[Array[Byte]]
    var removedRecords = 0L

    def flushRemovalBuffer(force: Boolean = false): Unit = {
      if (keysToRemove.nonEmpty && (force || keysToRemove.length >= RemovalBatchSize)) {
        flushKeysToRemove(keysToRemove.toArray)
        removedRecords += keysToRemove.length
        keysToRemove.clear()
      }
    }

    def dropKey(key: Array[Byte]): Unit = {
      keysToRemove += key
      flushRemovalBuffer()
    }

    persistentStore.getAll.foreach { case (key, value) =>
      if (key.length > PeerDatabase.MaxSerializedPeerAddressSize ||
          value.length > PeerDatabase.MaxSerializedPeerInfoSize) {
        log.warn(
          s"Dropping oversized peer entry from database: key=${key.length} bytes, " +
          s"value=${value.length} bytes"
        )
        dropKey(key)
      } else {
        val addressTry  = Try(deserialize(key).asInstanceOf[InetSocketAddress])
        val peerInfoTry = PeerInfoSerializer.parseBytesTry(value)
        (addressTry, peerInfoTry) match {
          case (Success(address), Success(peerInfo)) =>
            val loaded = LoadedPeer(peerInfo.lastHandshake, address, peerInfo, key)
            kept.get(address) match {
              case Some(existing) if existing.lastHandshake >= loaded.lastHandshake =>
                dropKey(key)
              case Some(existing) =>
                kept(address) = loaded
                oldestFirst.enqueue(loaded)
                dropKey(existing.keyBytes)
              case None if kept.size < maxKnownPeers =>
                kept(address) = loaded
                oldestFirst.enqueue(loaded)
              case None =>
                // evict the oldest kept peer if the loaded one is newer
                while (oldestFirst.headOption.exists(p => kept.get(p.address).forall(_ != p))) {
                  oldestFirst.dequeue()
                }
                oldestFirst.headOption match {
                  case Some(oldest) if loaded.lastHandshake > oldest.lastHandshake =>
                    kept -= oldest.address
                    kept(address) = loaded
                    oldestFirst.enqueue(loaded)
                    dropKey(oldest.keyBytes)
                  case _ =>
                    dropKey(key)
                }
            }
          case _ =>
            log.warn(s"Unable to deserialize peer entry from database, removing it")
            dropKey(key)
        }
      }
    }
    flushRemovalBuffer(force = true)

    if (removedRecords > 0) {
      log.info(s"Removed $removedRecords malformed, oversized or excess peer entries from database on startup")
    }
    kept.map { case (address, loaded) => address -> loaded.peerInfo }.toMap
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
    * Note: a candidate with `lastHandshake == 0` (a peer we have not handshaked with yet)
    * can never displace an existing peer. This is an intentional anti-spam policy: data
    * about not-yet-verified peers must not evict verified ones.
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
    * Remove peers whose lastHandshake is older than 60 days, excluding connected peers
    * and peers without a successful handshake (`lastHandshake == 0`, e.g. discovered
    * but not yet tried peers and unavailable configured seeds), so that untried peers
    * are not purged shortly after being discovered.
    */
  def removeOldPeers(connectedPeers: Set[InetSocketAddress] = Set.empty): Unit = {
    val cutoff = System.currentTimeMillis() - PeerDatabase.KnownPeerMaxAgeMs
    val toRemove = peers.collect {
      case (address, info)
          if !connectedPeers.contains(address) &&
             info.lastHandshake != 0 &&
             info.lastHandshake < cutoff =>
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
    * Serialized peer info size must stay below this bound. The value is twice
    * the maximum handshake size (8KB) to leave a comfortable margin while still
    * preventing a single malformed/crafted entry from consuming a lot of memory.
    */
  private[peer] val MaxSerializedPeerInfoSize: Int = 16384

  /**
    * Serialized peer address (InetSocketAddress Java serialization) size bound.
    * Legitimate hostnames can be up to 253 characters, so leave plenty of headroom.
    */
  private[peer] val MaxSerializedPeerAddressSize: Int = 1024

  /**
    * Hardcoded maximum age (60 days) for a known peer's lastHandshake.
    * Peers with `lastHandshake == 0` (never handshaked) are exempt from age cleanup.
    */
  val KnownPeerMaxAgeMs: Long = 60.days.toMillis

  /**
    * Hardcoded interval (24 hours) between cleanup runs.
    */
  val KnownPeerCleanupIntervalMs: Long = 24.hours.toMillis

}
