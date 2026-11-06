package org.ergoplatform.network

import org.ergoplatform.network.ErgoNodeViewSynchronizer.InputBlockDiffData
import org.ergoplatform.network.message.MessageConstants
import org.ergoplatform.settings.Parameters
import scorex.util.ModifierId

import scala.collection.mutable

/** Actor-confined pending transaction diffs. Limits are local resource policy, not consensus rules. */
private[network] final class InputBlockPendingCache(maxEntries: Int,
                                                   maxPeerEntries: Int,
                                                   maxWeight: Long,
                                                   maxPeerWeight: Long,
                                                   ttlMillis: Long,
                                                   clock: () => Long = () => System.currentTimeMillis(),
                                                   onExpired: (ModifierId, InputBlockDiffData, Boolean) => Unit =
                                                     (_, _, _) => ()) {
  require(maxEntries > 0 && maxPeerEntries > 0 && maxWeight > 0 && maxPeerWeight > 0 && ttlMillis > 0)

  private case class Entry(data: InputBlockDiffData, owner: String, connection: String,
                           weight: Long, announcedWeakIds: Boolean)
  private val entries = mutable.Map.empty[ModifierId, Entry]
  private var totalWeight = 0L

  def size: Int = entries.size
  def retainedWeight: Long = totalWeight

  def get(id: ModifierId): Option[InputBlockDiffData] = {
    prune()
    entries.get(id).map(_.data)
  }

  /** Whether a new entry of known retained weight can fit without evicting pending work. */
  def canAdmit(owner: String, weight: Long): Boolean = {
    prune()
    val owned = entries.valuesIterator.filter(_.owner == owner).toVector
    entries.size < maxEntries && owned.size < maxPeerEntries &&
      weight <= maxWeight - totalWeight &&
      weight <= maxPeerWeight - owned.map(_.weight).sum
  }

  /** Reject overflow without evicting another in-flight request. Replacements retain their original deadline. */
  def put(id: ModifierId, data: InputBlockDiffData, owner: String, connection: String,
          announcedWeakIds: Boolean = false): Boolean = {
    prune()
    val previous = entries.get(id)
    val weight = InputBlockPendingCache.weightOf(data)
    val owned = entries.valuesIterator.filter(_.owner == owner).toVector
    val priorWeight = previous.fold(0L)(_.weight)
    val added = if (previous.isDefined) 0 else 1
    if (previous.exists(p => p.owner != owner || p.connection != connection) ||
        entries.size + added > maxEntries || owned.size + added > maxPeerEntries ||
        weight > maxWeight - (totalWeight - priorWeight) ||
        weight > maxPeerWeight - (owned.map(_.weight).sum - priorWeight)) {
      false
    } else {
      val retained = previous.fold(data)(p => data.copy(created = p.data.created))
      entries.put(id, Entry(retained, owner, connection, weight,
        previous.exists(_.announcedWeakIds) || announcedWeakIds))
      totalWeight += weight - priorWeight
      true
    }
  }

  def remove(id: ModifierId): Unit = {
    entries.remove(id).foreach(e => totalWeight -= e.weight)
  }

  def removeConnection(connection: String): Vector[(ModifierId, InputBlockDiffData, Boolean)] = {
    val removed = entries.iterator.collect {
      case (id, e) if e.connection == connection => (id, e.data, e.announcedWeakIds)
    }.toVector
    removed.foreach { case (id, _, _) => remove(id) }
    removed
  }

  def prune(): Unit = {
    val now = clock()
    val expired = entries.iterator.collect {
      case (id, e) if now - e.data.created >= ttlMillis => (id, e)
    }.toVector
    expired.foreach { case (id, _) => remove(id) }
    // Release bodies first; the synchronizer keeps only bounded retry metadata.
    // Notify on every expiry path, including an access or admission before a cleanup tick.
    expired.foreach { case (id, e) => onExpired(id, e.data, e.announcedWeakIds) }
  }

  def clear(): Unit = {
    entries.clear()
    totalWeight = 0L
  }
}

private[network] object InputBlockPendingCache {
  // Allow two default-length chains from one address and eight globally. These are local
  // concurrency limits, not restrictions on the number of input blocks a chain may contain.
  val MaxEntries: Int = 8 * Parameters.SubsPerBlockDefault
  val MaxPeerEntries: Int = 2 * Parameters.SubsPerBlockDefault
  // A weak ID occupies 6 wire bytes but is charged 38 retained bytes. Eight transport
  // envelopes per peer leave room for this expansion and ordinary cached transactions.
  // The shared budget is twice that (~250 MiB). Overflow defers new work; it never evicts
  // another in-flight entry. Weight includes shared references and object allowances;
  // it is an accounting proxy, not an exact JVM heap-size measurement or a consensus limit.
  val MaxPeerWeight: Long = 8L * MessageConstants.MaxMessageSize
  val MaxWeight: Long = 2L * MaxPeerWeight

  def weightOf(data: InputBlockDiffData): Long = {
    256L + data.weakTxsIds.foldLeft(0L)((sum, id) => sum + 32L + id.length) +
      data.txs.foldLeft(0L)((sum, tx) => sum + 64L + tx.size.toLong)
  }
}
