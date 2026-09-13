package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.{LRUCache, ModifiersCache}
import org.ergoplatform.validation.{MalformedModifierError, ParentHeaderNotFoundError}
import scorex.util.ScorexLogging

import scala.collection.mutable
import scala.util.Failure

class ErgoModifiersCache(override val maxSize: Int) extends ModifiersCache with LRUCache with ScorexLogging {

  private val missingParentByChild = mutable.Map.empty[K, K]
  private val childrenByMissingParent = mutable.LinkedHashMap.empty[K, mutable.LinkedHashSet[K]]
  private val newMissingParents = mutable.LinkedHashSet.empty[K]
  private val retryMissingParents = mutable.LinkedHashSet.empty[K]

  override def put(key: K, value: V): Unit = {
    val isNewHeader = !cache.contains(key) && value.isInstanceOf[Header]
    super.put(key, value)
    if (isNewHeader) {
      // A parent already present in the cache is not an external recovery frontier.
      newMissingParents -= key
      retryMissingParents -= key
    }
  }

  override def remove(key: K): Option[V] = {
    val removed = super.remove(key)
    removed.foreach {
      case _: Header =>
        forgetChild(key)
        // If this header was shielding cached children, its removal exposes their
        // parent frontier again. Selection later filters parents already in history.
        childrenByMissingParent.get(key).filter(_.nonEmpty).foreach { _ =>
          retryMissingParents -= key
          newMissingParents += key
        }
      case _ =>
        ()
    }
    removed
  }

  override def findCandidateKey(history: ErgoHistory): Option[K] = {
    def tryToApply(k: K, v: BlockSection): Boolean = {
      history.applicableTry(v) match {
        case Failure(e) if e.isInstanceOf[MalformedModifierError] =>
          log.warn(s"Modifier ${v.encodedId} is permanently invalid and will be removed from cache", e)
          remove(k)
          false
        case m => m.isSuccess
      }
    }

    val headersHeight = history.headersHeight

    {
      // try to apply block sections from height next to best fullBlock
      history
        .headerIdsAtHeight(history.fullBlockHeight + 1)
        .flatMap(id => history.typedModifierById[Header](id))
        .flatMap(_.sectionIds)
        .map(_._2)
        .flatMap(id => cache.get(id).map(v => id -> v))
        .find(p => tryToApply(p._1, p._2)).map(_._1)
    } orElse {
      // do exhaustive search between modifiers, that are possibly may be applied (exclude headers far from best header)
      cache.find { case (k, v) =>
        v match {
          case h: Header if h.height > headersHeight + 1 => false
          case _ => tryToApply(k, v)
        }
      }.map(_._1)
    }
  }

  private def forgetChild(childId: K): Unit = {
    missingParentByChild.remove(childId).foreach { parentId =>
      childrenByMissingParent.get(parentId).foreach { childIds =>
        childIds -= childId
        if (childIds.isEmpty) {
          childrenByMissingParent -= parentId
          newMissingParents -= parentId
          retryMissingParents -= parentId
        }
      }
    }
  }

  private def registerMissingParent(childId: K, parentId: K): Unit = {
    val wasTracked = childrenByMissingParent.contains(parentId)
    if (!missingParentByChild.get(childId).contains(parentId)) {
      forgetChild(childId)
      missingParentByChild.put(childId, parentId)
      childrenByMissingParent.getOrElseUpdate(parentId, mutable.LinkedHashSet.empty) += childId
    }

    if (cache.contains(parentId)) {
      newMissingParents -= parentId
      retryMissingParents -= parentId
    } else if (!wasTracked || (!newMissingParents.contains(parentId) && !retryMissingParents.contains(parentId))) {
      // A newly exposed frontier gets one turn before older retry candidates.
      retryMissingParents -= parentId
      newMissingParents += parentId
    } else {
      ()
    }
  }

  /**
    * Registers newly received orphan headers and returns a bounded, rotating
    * page of parent ids absent from both history and this cache.
    *
    * Work is proportional to `candidates.size + limit`; the full cache is never
    * scanned. Only one request frontier is kept for siblings sharing a parent.
    */
  def findMissingParentIds(history: ErgoHistory,
                           candidates: Seq[Header],
                           limit: Int): Seq[K] = {
    require(limit > 0)

    val inspectedCandidates = mutable.HashSet.empty[K]
    candidates.foreach { header =>
      if (inspectedCandidates.add(header.id)) {
        if (cache.contains(header.id)) {
          history.applicableTry(header) match {
            case Failure(error: ParentHeaderNotFoundError) =>
              registerMissingParent(header.id, error.parentId)
            case _ =>
              forgetChild(header.id)
          }
        } else {
          forgetChild(header.id)
        }
      }
    }

    val selected = mutable.ArrayBuffer.empty[K]
    val parentsToInspect = Math.min(newMissingParents.size + retryMissingParents.size, limit)
    var inspectedParents = 0

    while (selected.size < limit && inspectedParents < parentsToInspect &&
      (newMissingParents.nonEmpty || retryMissingParents.nonEmpty)) {
      val parentId = newMissingParents.headOption.getOrElse(retryMissingParents.head)
      newMissingParents -= parentId
      retryMissingParents -= parentId
      inspectedParents += 1

      childrenByMissingParent.get(parentId) match {
        case Some(childIds) if childIds.nonEmpty && !cache.contains(parentId) && !history.contains(parentId) =>
          selected += parentId
          retryMissingParents += parentId
        case Some(_) if cache.contains(parentId) =>
          // Keep the relation dormant. Removing the cached parent re-enables it.
          ()
        case Some(_) =>
          // The parent reached history; cached children will be consumed by the
          // normal application loop and remove their registrations.
          ()
        case None =>
          ()
      }
    }

    selected.toVector
  }

}
