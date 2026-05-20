package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.{LRUCache, ModifiersCache}
import org.ergoplatform.validation.{MalformedModifierError, RecoverableModifierError}
import scorex.util.ScorexLogging

import scala.util.Failure

class ErgoModifiersCache(override val maxSize: Int) extends ModifiersCache with LRUCache with ScorexLogging {

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

  /**
    * Find the lowest-height header in the cache that fails `applicableTry` with a
    * `RecoverableModifierError` (typically `ParentHeaderNotFoundError`).
    *
    * Used by `ErgoNodeViewHolder` to surface stuck cache entries via
    * `RecoverableFailedModification` events so the synchronizer's parent-chase
    * handler can request the missing ancestor and the chain switch can walk back
    * to the common ancestor.
    */
  def findStuckHeader(history: ErgoHistory): Option[(Header, RecoverableModifierError)] = {
    cache.values
      .collect { case h: Header => h }
      .toSeq
      .sortBy(_.height)
      .iterator
      .flatMap { h =>
        history.applicableTry(h) match {
          case Failure(e: RecoverableModifierError) => Some(h -> e)
          case _                                    => None
        }
      }
      .toStream
      .headOption
  }

}
