package org.ergoplatform.network

import akka.actor.ActorRef
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
  * while connection-scoped disposal removes entries when that sender disconnects.
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

  import PendingInputAnnouncements.{
    Disconnected, DropReason, Duplicate, Evicted, Expired, HostLimit, Oversize,
    RemovalReason, Replayed, StaleParent, VariantLimit, peerHostKey
  }

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
  private var dropCounts = PendingInputAnnouncements.emptyDrops
  private var admittedCount = 0L
  private var replayedCount = 0L
  private var replayNotForwardedCount = 0L
  private var lastWarning: Option[Long] = None

  def size: Int = entries.size
  def byteSize: Long = usedBytes
  def evictions: Long = evictionCount
  def drops: Map[String, Long] = dropCounts
  def fullInfo: PendingInputAnnouncements.Stats =
    PendingInputAnnouncements.Stats(size, byteSize, admittedCount, replayedCount,
      replayNotForwardedCount, evictions, drops)

  def noteReplayNotForwarded(): Unit = {
    replayNotForwardedCount += 1
    onChange()
  }

  private def countDrop(reason: DropReason): Unit = {
    dropCounts = dropCounts.updated(reason.name, dropCounts(reason.name) + 1)
    log.debug(s"Pending input announcement dropped: ${reason.name}")
  }

  private def warnLimited(reason: String): Unit = {
    val time = now()
    if (lastWarning.forall(previous => time - previous >= 1000L)) {
      log.warn(s"Pending input announcement $reason (size=$size, bytes=$byteSize, " +
        s"evictions=$evictions, drops=$drops)")
      lastWarning = Some(time)
    }
  }

  private def reject(reason: DropReason): Boolean = {
    countDrop(reason)
    onChange()
    false
  }

  private def remove(id: ModifierId, reason: RemovalReason): Unit = entries.remove(id).foreach { entry =>
    reason match {
      case Evicted =>
        evictionCount += 1
        warnLimited("capacity eviction")
      case Replayed => ()
      case drop: DropReason => countDrop(drop)
    }
    if (reason != Replayed) onDiscard(entry.announcement, entry.peer)
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
    }.toVector.foreach(id => remove(id, Expired))
  }

  def removeConnection(handler: ActorRef): Unit = {
    entries.iterator.collect {
      case (id, entry) if entry.peer.handlerRef == handler => id
    }.toVector.foreach(id => remove(id, Disconnected))
  }

  def add(announcement: InputBlockAnnouncement, peer: ConnectedPeer): Boolean = {
    expire()
    // Count by host, so reconnecting with a new source port does not reset admission.
    val host = peerHostKey(peer)
    val hostEntries = entries.valuesIterator.filter(
      entry => peerHostKey(entry.peer) == host).toVector
    // Only a same-host/header re-send pays for serialization before the host limit.
    lazy val serialized = InputBlockAnnouncement.serializer.toBytes(announcement)
    val heldHeader = hostEntries.find(_.announcement.id == announcement.id)
    if (heldHeader.exists(entry =>
      InputBlockAnnouncement.serializer.toBytes(entry.announcement).sameElements(serialized))) {
      return reject(Duplicate)
    }
    if (hostEntries.size >= perPeer) return reject(HostLimit)
    if (heldHeader.isDefined) return reject(VariantLimit)
    val key = bytesToId(Blake2b256.hash(serialized))
    if (entries.contains(key)) {
      reject(Duplicate)
    } else {
      val bytes = serialized.length.toLong
      if (bytes > maxBytes) {
        reject(Oversize)
      } else {
        while (entries.size >= maxEntries || usedBytes > maxBytes - bytes) {
          val occupancy = entries.valuesIterator.toSeq.groupBy(
            entry => peerHostKey(entry.peer)).map { case (h, es) => h -> es.size }
          val activePeers = (occupancy.keySet + host).size
          val fairShare = maxEntries / math.max(1, activePeers)
          val incomingCount = occupancy.getOrElse(host, 0) + 1
          val candidates = entries.iterator.filter { case (_, entry) =>
            val count = occupancy(peerHostKey(entry.peer))
            incomingCount <= fairShare || count >= fairShare
          }.toVector
          // Non-empty: either every entry qualifies, or the incoming host holds at
          // least fairShare entries. Oversize rejection prevents an empty-store loop.
          // Prefer the largest occupancy, then the incoming host, then oldest arrival.
          val victim = candidates.maxBy { case (_, entry) =>
            val h = peerHostKey(entry.peer)
            (occupancy(h), if (h == host) 1 else 0)
          }._1
          remove(victim, Evicted)
        }
        entries.put(key, Entry(announcement, peer, bytes, now()))
        parents.getOrElseUpdate(announcement.header.parentId,
          mutable.LinkedHashSet.empty[ModifierId]) += key
        usedBytes += bytes
        admittedCount += 1
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
    * Without a parent lookup, +2 parents are unknown and wait for TTL.
    */
  def take(tip: Header): Seq[(InputBlockAnnouncement, ConnectedPeer)] = take(tip, Int.MaxValue)

  def take(tip: Header, limit: Int): Seq[(InputBlockAnnouncement, ConnectedPeer)] =
    take(tip, limit, _ => None)

  /** At most `limit` entries leave the store for validation in one actor receive.
    * Rollbacks discard entries outside the +2 window. A known +2 parent must be
    * at tip.height + 1 and either extend the applied tip or be on the best header
    * chain (the applied tip may still be on a losing fork). Unknown parents wait for TTL.
    */
  def take(tip: Header,
           limit: Int,
           parentHeader: ModifierId => Option[Header],
           onBestHeaderChain: Header => Boolean = _ => false)
          : Seq[(InputBlockAnnouncement, ConnectedPeer)] = {
    expire()
    val futureParents = entries.valuesIterator.collect {
      case entry if entry.announcement.header.height == tip.height + 2 =>
        entry.announcement.header.parentId
    }.toSet
    val staleParents = futureParents.filter { id =>
      parentHeader(id).exists { p =>
        p.height != tip.height + 1 || (p.parentId != tip.id && !onBestHeaderChain(p))
      }
    }.toSet
    entries.iterator.collect {
      case (id, entry) if entry.announcement.header.height <= tip.height ||
        entry.announcement.header.height > tip.height + 2 ||
        (entry.announcement.header.height == tip.height + 1 &&
          entry.announcement.header.parentId != tip.id) ||
        (entry.announcement.header.height == tip.height + 2 &&
          staleParents(entry.announcement.header.parentId)) => id
    }.toVector.foreach(id => remove(id, StaleParent))
    val ready = parents.get(tip.id).toVector.flatMap(_.toVector)
      .filter(id => entries(id).announcement.header.height == tip.height + 1).take(limit)
    ready.map { id =>
      val entry = entries(id)
      replayedCount += 1
      remove(id, Replayed)
      entry.announcement -> entry.peer
    }
  }
}

object PendingInputAnnouncements {
  private[network] def peerHostKey(peer: ConnectedPeer): String = {
    val addr = peer.connectionId.remoteAddress
    Option(addr.getAddress).fold(addr.getHostString)(_.getHostAddress)
  }

  private sealed trait RemovalReason
  private sealed abstract class DropReason(val name: String) extends RemovalReason
  private case object Duplicate extends DropReason("duplicate")
  private case object HostLimit extends DropReason("hostLimit")
  private case object VariantLimit extends DropReason("variantLimit")
  private case object Oversize extends DropReason("oversize")
  private case object Expired extends DropReason("expired")
  private case object StaleParent extends DropReason("staleParent")
  private case object Disconnected extends DropReason("disconnected")
  private case object Evicted extends RemovalReason
  private case object Replayed extends RemovalReason

  private val emptyDrops: Map[String, Long] = Seq(
    Duplicate, HostLimit, VariantLimit, Oversize, Expired, StaleParent, Disconnected)
    .map(reason => reason.name -> 0L).toMap

  /** Immutable snapshot published by the owning synchronizer for /info. */
  case class Stats(size: Int = 0, bytes: Long = 0L,
                   admitted: Long = 0L, replayed: Long = 0L, replayNotForwarded: Long = 0L,
                   evictions: Long = 0L, drops: Map[String, Long] = emptyDrops)

  object Stats {
    implicit val jsonEncoder: Encoder[Stats] = (stats: Stats) => Json.obj(
      "size" -> Json.fromInt(stats.size),
      "bytes" -> Json.fromLong(stats.bytes),
      "admitted" -> Json.fromLong(stats.admitted),
      "replayed" -> Json.fromLong(stats.replayed),
      "replayNotForwarded" -> Json.fromLong(stats.replayNotForwarded),
      "evictions" -> Json.fromLong(stats.evictions),
      "drops" -> Json.obj(stats.drops.toSeq.map { case (reason, count) =>
        reason -> Json.fromLong(count)
      }: _*)
    )
  }
}
