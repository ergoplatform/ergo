package org.ergoplatform.network

import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.subblocks.InputBlockAnnouncement
import scorex.core.network.ConnectedPeer
import scorex.util.ModifierId

import scala.collection.mutable

/** Actor-confined, UNTRUSTED announcements awaiting an applied ordering parent.
  * Byte accounting covers serialized announcement payloads; the entry cap bounds
  * object/index overhead. Nothing here is a validated input-block record.
  */
final class PendingInputAnnouncements(maxEntries: Int,
                                      maxBytes: Long,
                                      perPeer: Int,
                                      ttlMillis: Long,
                                      now: () => Long) {
  require(maxEntries > 0 && maxBytes > 0 && perPeer > 0 && ttlMillis > 0)

  private case class Entry(announcement: InputBlockAnnouncement,
                           peer: ConnectedPeer,
                           bytes: Long,
                           arrived: Long)

  private val entries = mutable.LinkedHashMap.empty[ModifierId, Entry]
  private val parents = mutable.Map.empty[ModifierId, mutable.LinkedHashSet[ModifierId]]
  private var usedBytes = 0L
  private var evictionCount = 0L

  def size: Int = entries.size
  def byteSize: Long = usedBytes
  def evictions: Long = evictionCount

  private def remove(id: ModifierId): Unit = entries.remove(id).foreach { entry =>
    usedBytes -= entry.bytes
    val parentId = entry.announcement.header.parentId
    parents.get(parentId).foreach { ids =>
      ids -= id
      if (ids.isEmpty) parents -= parentId
    }
  }

  def expire(): Unit = {
    val time = now()
    entries.iterator.collect {
      case (id, entry) if time - entry.arrived >= ttlMillis => id
    }.toVector.foreach(remove)
  }

  def add(announcement: InputBlockAnnouncement, peer: ConnectedPeer): Boolean = {
    expire()
    // Count by host, so reconnecting with a new source port does not reset admission.
    val host = peer.connectionId.remoteAddress.getHostString
    if (entries.contains(announcement.id) || entries.valuesIterator.count(
      _.peer.connectionId.remoteAddress.getHostString == host) >= perPeer) {
      false
    } else {
      val bytes = InputBlockAnnouncement.serializer.toBytes(announcement).length.toLong
      if (bytes > maxBytes) {
        false
      } else {
        while (entries.size >= maxEntries || usedBytes > maxBytes - bytes) {
          remove(entries.head._1)
          evictionCount += 1
        }
        entries.put(announcement.id, Entry(announcement, peer, bytes, now()))
        parents.getOrElseUpdate(announcement.header.parentId,
          mutable.LinkedHashSet.empty[ModifierId]) += announcement.id
        usedBytes += bytes
        true
      }
    }
  }

  /** Drop stale/forked entries and detach this parent's group in arrival order.
    * A rollback also discards announcements now outside the +2 admission window.
    * Call only once history and state agree on the applied full-block tip.
    */
  def take(tip: Header): Seq[(InputBlockAnnouncement, ConnectedPeer)] = {
    expire()
    entries.iterator.collect {
      case (id, entry) if entry.announcement.header.height <= tip.height ||
        entry.announcement.header.height > tip.height + 2 ||
        (entry.announcement.header.height == tip.height + 1 &&
          entry.announcement.header.parentId != tip.id) => id
    }.toVector.foreach(remove)
    val ready = parents.get(tip.id).toVector.flatMap(_.toVector).flatMap(entries.get)
      .filter(_.announcement.header.height == tip.height + 1)
    ready.foreach(entry => remove(entry.announcement.id))
    ready.map(entry => entry.announcement -> entry.peer)
  }
}
