package org.ergoplatform.nodeView.mempool

import scorex.util.ModifierId

import scala.annotation.tailrec

/**
  * Explicit parent/child dependency graph over mempool transactions.
  *
  * Nodes are identified by `ModifierId`. A spend edge `parent -> child` exists
  * when `child` consumes an output produced by `parent`; a read edge exists
  * when `child` references the output as a data input. Both edge kinds are
  * stored eagerly in both directions. Only spend edges participate in family
  * weight propagation and double-spend policy.
  *
  * The graph lives alongside the BoxId-keyed `outputs`/`inputs` maps in
  * [[OrderedTxPool]]: the box maps stay authoritative for double-spend
  * detection; this graph stays authoritative for tx-to-tx traversal.
  *
  * Empty adjacency sets are pruned so map keys do not accumulate on
  * long-running nodes.
  */
final case class TxFamilyGraph(parents: Map[ModifierId, Set[ModifierId]],
                               children: Map[ModifierId, Set[ModifierId]],
                               readParents: Map[ModifierId, Set[ModifierId]],
                               readChildren: Map[ModifierId, Set[ModifierId]]) {

  /**
    * Register `txId` with the given direct `parentIds`. Idempotent: a repeat
    * call overwrites prior parents and reconciles the matching `children`
    * back-edges (former parents that are no longer parents lose the back-edge).
    */
  def addTx(txId: ModifierId, parentIds: Set[ModifierId]): TxFamilyGraph = {
    val (newParents, newChildren) = reconcileEdges(txId, parentIds, parents, children)
    copy(parents = newParents, children = newChildren)
  }

  /** Register or reconcile both consuming and read-only parents of `txId`. */
  def addTx(txId: ModifierId,
            spendParentIds: Set[ModifierId],
            readParentIds: Set[ModifierId]): TxFamilyGraph = {
    val (newParents, newChildren) =
      reconcileEdges(txId, spendParentIds, parents, children)
    val (newReadParents, newReadChildren) =
      reconcileEdges(txId, readParentIds, readParents, readChildren)

    TxFamilyGraph(newParents, newChildren, newReadParents, newReadChildren)
  }

  /**
    * Restore outgoing edges for a producer which entered after some of its
    * children, or which was reinserted after a rollback. The caller resolves
    * children from the box indexes; this method only reconciles tx adjacency.
    */
  def addChildren(txId: ModifierId,
                  spendChildIds: Set[ModifierId],
                  readChildIds: Set[ModifierId]): TxFamilyGraph = {
    val withSpendChildren = spendChildIds.foldLeft(this) { (graph, childId) =>
      graph.addEdge(txId, childId, isRead = false)
    }
    readChildIds.foldLeft(withSpendChildren) { (graph, childId) =>
      graph.addEdge(txId, childId, isRead = true)
    }
  }

  /**
    * Remove `txId` from the graph, cleaning both directions: drop the
    * `parents(txId)` and `children(txId)` entries, and remove `txId` from the
    * adjacency set of every former parent and former child. Empty sets are
    * pruned. No-op if `txId` is not in the graph.
    */
  def removeTx(txId: ModifierId): TxFamilyGraph = {
    val (parentsAfter, childrenAfter) = removeEdges(txId, parents, children)
    val (readParentsAfter, readChildrenAfter) =
      removeEdges(txId, readParents, readChildren)
    TxFamilyGraph(parentsAfter, childrenAfter, readParentsAfter, readChildrenAfter)
  }

  def parentsOf(txId: ModifierId): Set[ModifierId]  = parents.getOrElse(txId, Set.empty)
  def childrenOf(txId: ModifierId): Set[ModifierId] = children.getOrElse(txId, Set.empty)

  def readParentsOf(txId: ModifierId): Set[ModifierId] =
    readParents.getOrElse(txId, Set.empty)

  def readChildrenOf(txId: ModifierId): Set[ModifierId] =
    readChildren.getOrElse(txId, Set.empty)

  def dependencyParentsOf(txId: ModifierId): Set[ModifierId] =
    parentsOf(txId) ++ readParentsOf(txId)

  def dependencyChildrenOf(txId: ModifierId): Set[ModifierId] =
    childrenOf(txId) ++ readChildrenOf(txId)

  /** Transitive ancestors of `txId` via BFS over `parents`. */
  def ancestorsOf(txId: ModifierId): Set[ModifierId] =
    bfs(parentsOf(txId), Set(txId), parentsOf) - txId

  /** Transitive descendants of `txId` via BFS over `children`. */
  def descendantsOf(txId: ModifierId): Set[ModifierId] =
    bfs(childrenOf(txId), Set(txId), childrenOf) - txId

  /** Transitive ancestors across both spend and read dependencies. */
  def dependencyAncestorsOf(txId: ModifierId): Set[ModifierId] =
    bfs(dependencyParentsOf(txId), Set(txId), dependencyParentsOf) - txId

  /** Transitive descendants across both spend and read dependencies. */
  def dependencyDescendantsOf(txId: ModifierId): Set[ModifierId] =
    bfs(dependencyChildrenOf(txId), Set(txId), dependencyChildrenOf) - txId

  private def addEdge(parentId: ModifierId,
                      childId: ModifierId,
                      isRead: Boolean): TxFamilyGraph = {
    if (isRead) {
      copy(
        readParents = readParents.updated(
          childId,
          readParents.getOrElse(childId, Set.empty) + parentId
        ),
        readChildren = readChildren.updated(
          parentId,
          readChildren.getOrElse(parentId, Set.empty) + childId
        )
      )
    } else {
      copy(
        parents = parents.updated(childId, parents.getOrElse(childId, Set.empty) + parentId),
        children = children.updated(parentId, children.getOrElse(parentId, Set.empty) + childId)
      )
    }
  }

  private def reconcileEdges(
    txId: ModifierId,
    parentIds: Set[ModifierId],
    parentIndex: Map[ModifierId, Set[ModifierId]],
    childIndex: Map[ModifierId, Set[ModifierId]]
  ): (Map[ModifierId, Set[ModifierId]], Map[ModifierId, Set[ModifierId]]) = {
    val previousParents = parentIndex.getOrElse(txId, Set.empty)
    val stale           = previousParents -- parentIds
    val fresh           = parentIds -- previousParents

    val childrenAfterStale = stale.foldLeft(childIndex) { (acc, parentId) =>
      val updated = acc.getOrElse(parentId, Set.empty) - txId
      if (updated.isEmpty) acc - parentId else acc.updated(parentId, updated)
    }
    val childrenAfterFresh = fresh.foldLeft(childrenAfterStale) { (acc, parentId) =>
      acc.updated(parentId, acc.getOrElse(parentId, Set.empty) + txId)
    }
    val parentsAfter =
      if (parentIds.isEmpty) parentIndex - txId
      else parentIndex.updated(txId, parentIds)

    parentsAfter -> childrenAfterFresh
  }

  private def removeEdges(
    txId: ModifierId,
    parentIndex: Map[ModifierId, Set[ModifierId]],
    childIndex: Map[ModifierId, Set[ModifierId]]
  ): (Map[ModifierId, Set[ModifierId]], Map[ModifierId, Set[ModifierId]]) = {
    val myParents  = parentIndex.getOrElse(txId, Set.empty)
    val myChildren = childIndex.getOrElse(txId, Set.empty)

    val parentsAfter = myChildren.foldLeft(parentIndex - txId) { (acc, childId) =>
      val updated = acc.getOrElse(childId, Set.empty) - txId
      if (updated.isEmpty) acc - childId else acc.updated(childId, updated)
    }
    val childrenAfter = myParents.foldLeft(childIndex - txId) { (acc, parentId) =>
      val updated = acc.getOrElse(parentId, Set.empty) - txId
      if (updated.isEmpty) acc - parentId else acc.updated(parentId, updated)
    }

    parentsAfter -> childrenAfter
  }

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
  val empty: TxFamilyGraph = TxFamilyGraph(Map.empty, Map.empty, Map.empty, Map.empty)
}
