package org.ergoplatform.network

import io.circe.{Encoder, Json}
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.subblocks.InputBlockAnnouncement
import scorex.core.network.ConnectedPeer
import scorex.crypto.hash.Blake2b256
import scorex.util.{ModifierId, ScorexLogging, bytesToId}

import scala.collection.mutable

/** Actor-confined, UNTRUSTED announcements awaiting an applied ordering parent.
  * Byte accounting covers serialized announcement payloads; the entry cap bounds
  * object/index overhead. Nothing here is a validated input-block record.
  * A held announcement failing on replay penalises its ORIGINAL sender,
  * which may have disconnected by the time the ordering parent is applied.
  * Replay starts immediately on ordering apply, in arrival order, once history
  * and state agree on the applied tip; bounded batches continue via self-messages.
  * TODO(restart lag): short, unfunded early-chain devnet restarts showed higher
  * input-tip lag than stock. Hypothesis: the replay burst immediately after apply
  * near genesis. A longer funded restart did not reproduce it; the cause is not
  * isolated, so replay scheduling is unchanged.
  */
final class PendingInputAnnouncements(maxEntries: Int,
                                      maxBytes: Long,
                                      perPeer: Int,
                                      ttlMillis: Long,
                                      now: () => Long) extends ScorexLogging {
  require(maxEntries > 0 && maxBytes > 0 && perPeer > 0 && ttlMillis > 0)

  private case class Entry(announcement: InputBlockAnnouncement,
                           peer: ConnectedPeer,
                           bytes: Long,
                           arrived: Long)

  private[network] var onDiscard: (InputBlockAnnouncement, ConnectedPeer) => Unit =
    (_, _) => ()

  private[network] var onChange: () => Unit = () => ()

  private val entries = mutable.LinkedHashMap.empty[ModifierId, Entry]
  private val parents = mutable.Map.empty[ModifierId, mutable.LinkedHashSet[ModifierId]]
  private var usedBytes = 0L
  private var evictionCount = 0L
  private var dropCount = 0L
  private var lastWarning: Option[Long] = None

  def size: Int = entries.size
  def byteSize: Long = usedBytes
  def evictions: Long = evictionCount
  def drops: Long = dropCount
  def fullInfo: PendingInputAnnouncements.Stats =
    PendingInputAnnouncements.Stats(size, byteSize, evictions, drops)

  private def warnLimited(reason: String): Unit = {
    val time = now()
    if (lastWarning.forall(previous => time - previous >= 1000L)) {
      log.warn(s"Pending input announcement $reason (size=$size, bytes=$byteSize, " +
        s"evictions=$evictions, drops=$drops)")
      lastWarning = Some(time)
    }
  }

  private def reject(reason: String): Boolean = {
    dropCount += 1
    warnLimited(s"dropped: $reason")
    onChange()
    false
  }

  private def remove(id: ModifierId, discarded: Boolean = true,
                     evicted: Boolean = false): Unit = entries.remove(id).foreach { entry =>
    if (discarded) {
      if (evicted) evictionCount += 1 else dropCount += 1
      warnLimited(if (evicted) "capacity eviction" else "dropped: expired or stale parent")
      onDiscard(entry.announcement, entry.peer)
    }
    usedBytes -= entry.bytes
    val parentId = entry.announcement.header.parentId
    parents.get(parentId).foreach { ids =>
      ids -= id
      if (ids.isEmpty) parents -= parentId
    }
    onChange()
  }

  def expire(): Unit = {
    val time = now()
    entries.iterator.collect {
      case (id, entry) if time - entry.arrived >= ttlMillis => id
    }.toVector.foreach(id => remove(id))
  }

  def add(announcement: InputBlockAnnouncement, peer: ConnectedPeer): Boolean = {
    expire()
    // Count by host, so reconnecting with a new source port does not reset admission.
    val host = peer.connectionId.remoteAddress.getHostString
    val serialized = InputBlockAnnouncement.serializer.toBytes(announcement)
    val key = bytesToId(Blake2b256.hash(serialized))
    if (entries.contains(key)) {
      reject("duplicate serialized announcement")
    } else if (entries.valuesIterator.count(
      _.peer.connectionId.remoteAddress.getHostString == host) >= perPeer) {
      reject("per-peer capacity limit")
    } else {
      val bytes = serialized.length.toLong
      if (bytes > maxBytes) {
        reject("oversize payload")
      } else {
        while (entries.size >= maxEntries || usedBytes > maxBytes - bytes) {
          val occupancy = entries.valuesIterator.toSeq.groupBy(
            _.peer.connectionId.remoteAddress.getHostString).map { case (h, es) => h -> es.size }
          val activePeers = (occupancy.keySet + host).size
          val fairShare = maxEntries / math.max(1, activePeers)
          val incomingCount = occupancy.getOrElse(host, 0) + 1
          val candidates = entries.iterator.filter { case (_, entry) =>
            val count = occupancy(entry.peer.connectionId.remoteAddress.getHostString)
            incomingCount <= fairShare || count >= fairShare
          }.toVector
          if (candidates.isEmpty) return reject("capacity fairness limit")
          // Prefer the largest occupancy, then the incoming host, then oldest arrival.
          val victim = candidates.maxBy { case (_, entry) =>
            val h = entry.peer.connectionId.remoteAddress.getHostString
            (occupancy(h), if (h == host) 1 else 0)
          }._1
          remove(victim, evicted = true)
        }
        entries.put(key, Entry(announcement, peer, bytes, now()))
        parents.getOrElseUpdate(announcement.header.parentId,
          mutable.LinkedHashSet.empty[ModifierId]) += key
        usedBytes += bytes
        onChange()
        true
      }
    }
  }

  def hasReady(tip: Header): Boolean = parents.get(tip.id).exists(_.exists { id =>
    entries(id).announcement.header.height == tip.height + 1
  })

  /** Detach ready announcements in arrival order after stale/fork cleanup.
    * Call only once history and state agree on the applied full-block tip.
    * Without a best-chain lookup, +2 parents are treated as unknown and dropped.
    */
  def take(tip: Header): Seq[(InputBlockAnnouncement, ConnectedPeer)] = take(tip, Int.MaxValue)

  def take(tip: Header, limit: Int): Seq[(InputBlockAnnouncement, ConnectedPeer)] =
    take(tip, limit, _ => false)

  /** At most `limit` entries leave the store for validation in one actor receive.
    * Rollbacks discard entries outside the +2 window; +2 entries also require a
    * known header on the current best chain, as checked by the caller.
    */
  def take(tip: Header,
           limit: Int,
           knownBestChainParent: ModifierId => Boolean)
          : Seq[(InputBlockAnnouncement, ConnectedPeer)] = {
    expire()
    entries.iterator.collect {
      case (id, entry) if entry.announcement.header.height <= tip.height ||
        entry.announcement.header.height > tip.height + 2 ||
        (entry.announcement.header.height == tip.height + 1 &&
          entry.announcement.header.parentId != tip.id) ||
        (entry.announcement.header.height == tip.height + 2 &&
          !knownBestChainParent(entry.announcement.header.parentId)) => id
    }.toVector.foreach(id => remove(id))
    val ready = parents.get(tip.id).toVector.flatMap(_.toVector)
      .filter(id => entries(id).announcement.header.height == tip.height + 1).take(limit)
    ready.map { id =>
      val entry = entries(id)
      remove(id, discarded = false)
      entry.announcement -> entry.peer
    }
  }
}

object PendingInputAnnouncements {
  /** Immutable snapshot published by the owning synchronizer for /info. */
  case class Stats(size: Int = 0, bytes: Long = 0L,
                   evictions: Long = 0L, drops: Long = 0L)

  object Stats {
    implicit val jsonEncoder: Encoder[Stats] = (stats: Stats) => Json.obj(
      "size" -> Json.fromInt(stats.size),
      "bytes" -> Json.fromLong(stats.bytes),
      "evictions" -> Json.fromLong(stats.evictions),
      "drops" -> Json.fromLong(stats.drops)
    )
  }
}
