package org.ergoplatform.nodeView.mempool

import org.ergoplatform.ErgoBox.BoxId
import org.ergoplatform.modifiers.mempool.{ErgoTransaction, UnconfirmedTransaction}
import org.ergoplatform.nodeView.mempool.OrderedTxPool.WeightedTxId
import org.ergoplatform.settings.{Algos, ErgoSettings, MonetarySettings}
import scorex.util.{ModifierId, ScorexLogging}

import scala.collection.immutable.TreeMap

/**
  * An immutable pool of transactions of limited size with priority management and blacklisting support.
  *
  * @param orderedTransactions  - collection containing transactions ordered by `tx.weight`
  * @param transactionsRegistry - mapping `tx.id` -> `WeightedTxId(tx.id,tx.weight)` required for getting transaction by its `id`
  * @param invalidatedTxIds     - invalidated transaction ids in bloom filters
  * @param outputs              - mapping `box.id` -> producing `tx.id`; current weight is resolved via `transactionsRegistry`
  * @param inputs               - mapping `box.id` -> spending `tx.id`; current weight is resolved via `transactionsRegistry`
  * @param dataInputReaders     - mapping `box.id` -> transaction ids which read it without consuming it
  * @param family               - explicit parent/child dependency graph between mempool transactions, used by `updateFamily`
  */
class OrderedTxPool(val orderedTransactions: TreeMap[WeightedTxId, UnconfirmedTransaction],
                    val transactionsRegistry: TreeMap[ModifierId, WeightedTxId],
                    val invalidatedTxIds: ApproximateCacheLike[String],
                    val outputs: TreeMap[BoxId, ModifierId],
                    val inputs: TreeMap[BoxId, ModifierId],
                    val dataInputReaders: TreeMap[BoxId, Set[ModifierId]],
                    val family: TxFamilyGraph)
                   (implicit settings: ErgoSettings) extends ScorexLogging {

  import OrderedTxPool.weighted

  /**
    * When a transaction has a parent in the mempool, we update its weight, weight of parent's parents etc.
    * This parameter sets max update depth
    */
  private val MaxParentScanDepth = 500

  /**
    * See `MaxParentScanDepth`, but this parameter sets max update time
    */
  private val MaxParentScanTime = 500

  private implicit val ms: MonetarySettings = settings.chainSettings.monetary

  private val mempoolCapacity = settings.nodeSettings.mempoolCapacity

  private def withoutTransaction(id: ModifierId): TreeMap[WeightedTxId, UnconfirmedTransaction] = {
    // Keep healthy mutations logarithmic; scan by ID only after cardinality diverges.
    transactionsRegistry.get(id) match {
      case Some(wtx) if orderedTransactions.size == transactionsRegistry.size &&
        orderedTransactions.contains(wtx) =>
        orderedTransactions - wtx
      case None if orderedTransactions.size == transactionsRegistry.size =>
        orderedTransactions
      case _ =>
        orderedTransactions.filter { case (wtx, utx) => wtx.id != id && utx.id != id }
    }
  }

  private def hasUnregisteredTransaction(id: ModifierId): Boolean =
    orderedTransactions.size != transactionsRegistry.size &&
      orderedTransactions.valuesIterator.exists(_.id == id)

  private[mempool] def currentTransaction(id: ModifierId): Option[(WeightedTxId, UnconfirmedTransaction)] =
    transactionsRegistry.get(id)
      .flatMap(wtx => orderedTransactions.get(wtx).filter(_.id == id).map(wtx -> _))
      .orElse {
        orderedTransactions.iterator.collectFirst {
          case (wtx, utx) if wtx.id == id && utx.id == id => wtx -> utx
        }
      }

  private def trackedTransaction(id: ModifierId): Option[(WeightedTxId, UnconfirmedTransaction)] =
    transactionsRegistry.get(id) match {
      case Some(_) => currentTransaction(id)
      // Preserve an orphan's existing weight on re-put: its contribution may
      // already be present in ancestors, so treating it as new would add it twice.
      case None if hasUnregisteredTransaction(id) => currentTransaction(id)
      case None => None
    }

  private def addDataInputReaders(tx: ErgoTransaction): TreeMap[BoxId, Set[ModifierId]] =
    tx.dataInputs.foldLeft(dataInputReaders) { (readers, dataInput) =>
      readers.updated(
        dataInput.boxId,
        readers.getOrElse(dataInput.boxId, Set.empty) + tx.id
      )
    }

  private def removeDataInputReaders(tx: ErgoTransaction): TreeMap[BoxId, Set[ModifierId]] =
    tx.dataInputs.foldLeft(dataInputReaders) { (readers, dataInput) =>
      val remaining = readers.getOrElse(dataInput.boxId, Set.empty) - tx.id
      if (remaining.isEmpty) readers - dataInput.boxId
      else readers.updated(dataInput.boxId, remaining)
    }

  private def liveSpendChildren(tx: ErgoTransaction): Set[ModifierId] =
    tx.outputs.flatMap(output => inputs.get(output.id)).filter(currentTransaction(_).isDefined).toSet

  private def liveReadChildren(tx: ErgoTransaction): Set[ModifierId] =
    tx.outputs
      .flatMap(output => dataInputReaders.getOrElse(output.id, Set.empty))
      .filter(currentTransaction(_).isDefined)
      .toSet

  def size: Int = orderedTransactions.size

  def get(id: ModifierId): Option[UnconfirmedTransaction] = {
    transactionsRegistry.get(id).flatMap { wtx =>
      orderedTransactions.get(wtx) match {
        case s@Some(_) => s
        case None => log.warn(s"Found $id in registry but not ordered transactions"); None
      }
    }
  }


  /**
    * Add new transaction to the pool and throw away from the pool transaction with the smallest weight
    * if pool is overflown. We should first add transaction and only after it find candidate for replacement
    * because new transaction may affect weights of existed transaction in mempool (see updateFamily).
    * So candidate for replacement (transaction with minimal weight) can be changed after adding new transaction.
    * put() is preceded by canAccept method which enforces that newly added transaction will not be immediately
    * thrown from the pool.
    *
    * @param unconfirmedTx - transaction to add
    * @return - modified pool
    */
  def put(unconfirmedTx: UnconfirmedTransaction, feeFactor: Int): OrderedTxPool = {
    val tx = unconfirmedTx.transaction
    val tracked = trackedTransaction(tx.id)
    // A registry-only key may represent weight already propagated to ancestors.
    // Keep it distinct from a discoverable body so current child weight can be reconciled.
    val registryOnlyWtx = if (tracked.isEmpty) transactionsRegistry.get(tx.id) else None
    val currentWtx = tracked.map(_._1).orElse(registryOnlyWtx)

    val newPool = currentWtx match {
      case Some(existingWtx) =>
        val parentIds = tx.inputs.flatMap(in => outputs.get(in.boxId)).toSet
        val readParentIds = tx.dataInputs.flatMap(in => outputs.get(in.boxId)).toSet
        val spendChildIds = liveSpendChildren(tx)
        val readChildIds = liveReadChildren(tx)
        val restoredWtx = registryOnlyWtx match {
          case Some(_) =>
            val childWeight = spendChildIds.toSeq
              .flatMap(currentTransaction)
              .map(_._1.weight)
              .sum
            existingWtx.copy(weight = existingWtx.feePerFactor + childWeight)
          case None =>
            existingWtx
        }
        val updatedFamily = family
          .addTx(tx.id, parentIds, readParentIds)
          .addChildren(tx.id, spendChildIds, readChildIds)
        val restoredPool = new OrderedTxPool(
          withoutTransaction(tx.id).updated(restoredWtx, unconfirmedTx),
          transactionsRegistry.updated(tx.id, restoredWtx),
          invalidatedTxIds,
          outputs ++ tx.outputs.map(_.id -> tx.id),
          inputs ++ tx.inputs.map(_.boxId -> tx.id),
          addDataInputReaders(tx),
          updatedFamily
        )
        registryOnlyWtx match {
          case Some(_) =>
            restoredPool.reconcileFamilyWeights(parentIds, System.currentTimeMillis(), depth = 0)
          case None => restoredPool
        }
      case None =>
        val baseWtx = weighted(tx, feeFactor)
        val parentIds = tx.inputs.flatMap(in => outputs.get(in.boxId)).toSet
        val readParentIds = tx.dataInputs.flatMap(in => outputs.get(in.boxId)).toSet
        val spendChildIds = liveSpendChildren(tx)
        val readChildIds = liveReadChildren(tx)
        val childWeight = spendChildIds.toSeq.flatMap(currentTransaction).map(_._1.weight).sum
        val wtx = baseWtx.copy(weight = baseWtx.weight + childWeight)
        val updatedFamily = family
          .addTx(tx.id, parentIds, readParentIds)
          .addChildren(tx.id, spendChildIds, readChildIds)
        new OrderedTxPool(
          withoutTransaction(tx.id).updated(wtx, unconfirmedTx),
          transactionsRegistry.updated(wtx.id, wtx),
          invalidatedTxIds,
          outputs ++ tx.outputs.map(_.id -> tx.id),
          inputs ++ tx.inputs.map(_.boxId -> tx.id),
          addDataInputReaders(tx),
          updatedFamily
        ).updateFamily(tx, parentIds, wtx.weight, System.currentTimeMillis(), 0)
    }
    if (newPool.orderedTransactions.size > mempoolCapacity) {
      val victim = newPool.orderedTransactions.last._2
      newPool.remove(victim)
    } else {
      newPool
    }
  }

  def remove(txs: Seq[UnconfirmedTransaction]): OrderedTxPool = {
    txs.foldLeft(this) { case (pool, tx) => pool.remove(tx) }
  }

  private def removeStored(tx: ErgoTransaction,
                           wtx: WeightedTxId,
                           nextInvalidatedTxIds: ApproximateCacheLike[String]): OrderedTxPool = {
    // Snapshot parents from the live graph before removeTx, so updateFamily can still walk them.
    val parentIds = family.parentsOf(tx.id)
    new OrderedTxPool(
      withoutTransaction(tx.id),
      transactionsRegistry - tx.id,
      nextInvalidatedTxIds,
      outputs -- tx.outputs.map(_.id),
      inputs -- tx.inputs.map(_.boxId),
      removeDataInputReaders(tx),
      family.removeTx(tx.id)
    ).updateFamily(tx, parentIds, -wtx.weight, System.currentTimeMillis(), depth = 0)
  }

  /**
    * Removes transaction from the pool
    *
    * @param tx - Transaction to remove
    */
  def remove(tx: ErgoTransaction): OrderedTxPool = {
    trackedTransaction(tx.id) match {
      case Some((wtx, stored)) =>
        removeStored(stored.transaction, wtx, invalidatedTxIds)
      case None =>
        // A registry-only entry has no stored transaction body or trustworthy live weight.
        // Keep the v6.0.4 conservative no-op instead of subtracting a guessed family weight.
        this
    }
  }

  def remove(utx: UnconfirmedTransaction): OrderedTxPool = remove(utx.transaction)

  /**
    * Remove transaction from the pool and add it to invalidated transaction ids cache
    */
  def invalidate(unconfirmedTx: UnconfirmedTransaction): OrderedTxPool = {
    val tx = unconfirmedTx.transaction
    val nextInvalidatedTxIds = invalidatedTxIds.put(tx.id)
    trackedTransaction(tx.id) match {
      case Some((wtx, stored)) =>
        removeStored(stored.transaction, wtx, nextInvalidatedTxIds)
      case None =>
        // As in remove(), do not mutate indexes or family weights for an unresolved registry-only entry.
        // Invalidating the supplied id is still safe and prevents immediate re-admission.
        new OrderedTxPool(
          orderedTransactions,
          transactionsRegistry,
          nextInvalidatedTxIds,
          outputs,
          inputs,
          dataInputReaders,
          family
        )
    }
  }

  /**
    * Do not place transaction in the pool if the transaction known to be invalid, pool already has it, or the pool
    * is overfull.
    *
    * TODO: the latter should not happen likely as we clean pool immediately as it becomes overfull.
    *
    */
  def canAccept(unconfirmedTx: UnconfirmedTransaction): Boolean = {
    !contains(unconfirmedTx.id) && size <= mempoolCapacity
  }

  /**
    *
    * @param id - transaction id
    * @return - true, if transaction is in the pool or invalidated earlier, false otherwise
    */
  def contains(id: ModifierId): Boolean = {
    transactionsRegistry.contains(id)
  }

  def isInvalidated(id: ModifierId): Boolean = invalidatedTxIds.mightContain(id)

  /**
    * Rebuild live ancestor weights from their direct live children.
    *
    * Registry-only transactions can return in any order. Recomputing each
    * affected parent from the graph makes restoration independent of that
    * order while preserving per-path weight semantics at reconvergence.
    */
  private def reconcileFamilyWeights(txIds: Set[ModifierId],
                                     startTime: Long,
                                     depth: Int): OrderedTxPool = {
    val now = System.currentTimeMillis()
    val timeDiff = now - startTime
    if (depth > MaxParentScanDepth || timeDiff > MaxParentScanTime) {
      log.warn(s"reconcileFamilyWeights takes too long, depth: $depth, time diff: $timeDiff")
      this
    } else {
      txIds.foldLeft(this) { case (pool, txId) =>
        pool.currentTransaction(txId) match {
          case Some((wtx, utx)) =>
            val childWeight = pool.family.childrenOf(txId).toSeq
              .flatMap(pool.currentTransaction)
              .map(_._1.weight)
              .sum
            val reconciledWeight = wtx.feePerFactor + childWeight
            if (reconciledWeight == wtx.weight) {
              pool
            } else {
              val reconciledWtx = wtx.copy(weight = reconciledWeight)
              val reconciledPool = new OrderedTxPool(
                pool.withoutTransaction(txId).updated(reconciledWtx, utx),
                pool.transactionsRegistry.updated(txId, reconciledWtx),
                pool.invalidatedTxIds,
                pool.outputs,
                pool.inputs,
                pool.dataInputReaders,
                pool.family
              )
              reconciledPool.reconcileFamilyWeights(
                reconciledPool.family.parentsOf(txId),
                startTime,
                depth + 1
              )
            }
          case None =>
            pool
        }
      }
    }
  }

  /**
    *
    * Form families of transactions: take in account relations between transactions when performing ordering.
    * If transaction X is spending output of transaction Y, then X weight should be greater than of Y.
    * Y should be proceeded prior to X or swapped out of mempool after X.
    * To achieve this goal we recursively add weight of new transaction to all transactions which
    * outputs it directly or indirectly spending.
    *
    * @param tx
    * @param weight
    * @return
    */
  private def updateFamily(tx: ErgoTransaction,
                           parentIds: Set[ModifierId],
                           weight: Long,
                           startTime: Long,
                           depth: Int): OrderedTxPool = {
    val now = System.currentTimeMillis()
    val timeDiff = now - startTime
    if (depth > MaxParentScanDepth || timeDiff > MaxParentScanTime) {
      log.warn(s"updateFamily takes too long, depth: $depth, time diff: $timeDiff, transaction: ${tx.id}")
      this
    } else {

      parentIds.foldLeft(this) { case (pool, parentId) =>
        pool.currentTransaction(parentId) match {
          case Some((wtx, ut)) =>
            val parent = ut.transaction
            val newWtx = WeightedTxId(wtx.id, wtx.weight + weight, wtx.feePerFactor, wtx.created)
            // Weight propagation does not add or remove graph nodes/edges, nor change which tx produced/spent a box,
            // so `family`, `outputs` and `inputs` are threaded through unchanged. Only the weight-bearing maps rebuild.
            val newPool = new OrderedTxPool(
              pool.withoutTransaction(parent.id).updated(newWtx, ut),
              pool.transactionsRegistry.updated(parent.id, newWtx),
              pool.invalidatedTxIds,
              pool.outputs,
              pool.inputs,
              pool.dataInputReaders,
              pool.family
            )
            newPool.updateFamily(parent, pool.family.parentsOf(parent.id), weight, startTime, depth + 1)
          case None =>
            pool
        }
      }
    }
  }
}

object OrderedTxPool {

  /**
    * Weighted transaction id
    *
    * @param id       - Transaction id
    * @param weight   - Weight of transaction
    * @param feePerFactor - Transaction's fee per factor (byte or execution cost)
    * @param created  - Transaction creation time
    */
  case class WeightedTxId(id: ModifierId, weight: Long, feePerFactor: Long, created: Long) {
    // `id` depends on `weight` so we can use only the former for comparison.
    override def equals(obj: Any): Boolean = obj match {
      case that: WeightedTxId => that.id == id
      case _ => false
    }

    override def hashCode(): Int = id.hashCode()
  }

  private implicit val ordWeight: Ordering[WeightedTxId] = Ordering[(Long, ModifierId)].on(x => (-x.weight, x.id))
  private implicit val ordBoxId: Ordering[BoxId] = Ordering[String].on(b => Algos.encode(b))

  def empty(settings: ErgoSettings): OrderedTxPool = {
    val cacheSettings = settings.cacheSettings.mempool
    val frontCacheSize = cacheSettings.invalidModifiersCacheSize
    val frontCacheExpiration = cacheSettings.invalidModifiersCacheExpiration
    new OrderedTxPool(
      TreeMap.empty[WeightedTxId, UnconfirmedTransaction],
      TreeMap.empty[ModifierId, WeightedTxId],
      ExpiringApproximateCache.empty(frontCacheSize, frontCacheExpiration),
      TreeMap.empty[BoxId, ModifierId],
      TreeMap.empty[BoxId, ModifierId],
      TreeMap.empty[BoxId, Set[ModifierId]],
      TxFamilyGraph.empty
    )(settings)
  }

  def weighted(unconfirmedTx: UnconfirmedTransaction, feeFactor: Int)(implicit ms: MonetarySettings): WeightedTxId = {
    weighted(unconfirmedTx.transaction, feeFactor)
  }

  /**
    * Wrap transaction into an entity which is storing its mempool sorting weight also
    *
    * @param tx - transaction
    * @param feeFactor - fee-related factor of the transaction `tx`, so size or cost
    * @param ms - monetary settings to extract fee proposition from
    * @return - transaction and its weight wrapped in `WeightedTxId`
    */
  def weighted(tx: ErgoTransaction, feeFactor: Int)(implicit ms: MonetarySettings): WeightedTxId = {
    val fee = tx.outputs
      .filter(b => java.util.Arrays.equals(b.propositionBytes, ms.feePropositionBytes))
      .map(_.value)
      .sum

    // We multiply by 1024 for better precision
    val feePerFactor = fee * 1024 / feeFactor
    // Weight is equal to feePerFactor here, however, it can be modified later when children transactions will arrive
    WeightedTxId(tx.id, feePerFactor, feePerFactor, System.currentTimeMillis())
  }
}
