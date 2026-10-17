package org.ergoplatform.nodeView.mempool

import scorex.util.ModifierId

import scala.annotation.tailrec

/**
  * Explicit parent/child dependency graph over mempool transactions.
  *
  * Nodes are identified by `ModifierId`. An edge `parent -> child` exists when
  * `child` spends an output produced by `parent`. Adjacency is stored eagerly
  * in both directions so parent lookups (used by weight propagation in
  * [[OrderedTxPool.updateFamily]]) and child / descendant lookups (used by
  * removal and future algorithms over the pool) are constant-time.
  *
  * The graph lives alongside the BoxId-keyed `outputs`/`inputs` maps in
  * [[OrderedTxPool]]: the box maps stay authoritative for double-spend
  * detection; this graph stays authoritative for tx-to-tx traversal.
  *
  * Empty adjacency sets are pruned so map keys do not accumulate on
  * long-running nodes.
  */
final case class TxFamilyGraph(parents: Map[ModifierId, Set[ModifierId]],
                               children: Map[ModifierId, Set[ModifierId]]) {

  /**
    * Register `txId` with the given direct `parentIds`. Idempotent: a repeat
    * call overwrites prior parents and reconciles the matching `children`
    * back-edges (former parents that are no longer parents lose the back-edge).
    */
  def addTx(txId: ModifierId, parentIds: Set[ModifierId]): TxFamilyGraph = {
    val previousParents = parents.getOrElse(txId, Set.empty)
    val stale           = previousParents -- parentIds
    val fresh           = parentIds -- previousParents

    val childrenAfterStale = stale.foldLeft(children) { (acc, p) =>
      val updated = acc.getOrElse(p, Set.empty) - txId
      if (updated.isEmpty) acc - p else acc.updated(p, updated)
    }
    val childrenAfterFresh = fresh.foldLeft(childrenAfterStale) { (acc, p) =>
      acc.updated(p, acc.getOrElse(p, Set.empty) + txId)
    }
    val newParents =
      if (parentIds.isEmpty) parents - txId
      else parents.updated(txId, parentIds)

    TxFamilyGraph(newParents, childrenAfterFresh)
  }

  /**
    * Remove `txId` from the graph, cleaning both directions: drop the
    * `parents(txId)` and `children(txId)` entries, and remove `txId` from the
    * adjacency set of every former parent and former child. Empty sets are
    * pruned. No-op if `txId` is not in the graph.
    */
  def removeTx(txId: ModifierId): TxFamilyGraph = {
    val myParents  = parents.getOrElse(txId, Set.empty)
    val myChildren = children.getOrElse(txId, Set.empty)

    val parentsAfter = myChildren.foldLeft(parents - txId) { (acc, c) =>
      val updated = acc.getOrElse(c, Set.empty) - txId
      if (updated.isEmpty) acc - c else acc.updated(c, updated)
    }
    val childrenAfter = myParents.foldLeft(children - txId) { (acc, p) =>
      val updated = acc.getOrElse(p, Set.empty) - txId
      if (updated.isEmpty) acc - p else acc.updated(p, updated)
    }
    TxFamilyGraph(parentsAfter, childrenAfter)
  }

  def parentsOf(txId: ModifierId): Set[ModifierId]  = parents.getOrElse(txId, Set.empty)
  def childrenOf(txId: ModifierId): Set[ModifierId] = children.getOrElse(txId, Set.empty)

  /** Transitive ancestors of `txId` via BFS over `parents`. */
  def ancestorsOf(txId: ModifierId): Set[ModifierId] =
    bfs(parentsOf(txId), Set.empty, parentsOf)

  /** Transitive descendants of `txId` via BFS over `children`. */
  def descendantsOf(txId: ModifierId): Set[ModifierId] =
    bfs(childrenOf(txId), Set.empty, childrenOf)

  @tailrec
  private def bfs(frontier: Set[ModifierId],
                  visited: Set[ModifierId],
                  step: ModifierId => Set[ModifierId]): Set[ModifierId] = {
    if (frontier.isEmpty) visited
    else {
      val visitedNext = visited ++ frontier
      val next        = frontier.flatMap(step) -- visitedNext
      bfs(next, visitedNext, step)
    }
  }
}

object TxFamilyGraph {
  val empty: TxFamilyGraph = TxFamilyGraph(Map.empty, Map.empty)
}
