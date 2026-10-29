package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.BlockSection
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.{LRUCache, ModifiersCache}
import org.ergoplatform.validation.MalformedModifierError
import scorex.util.ScorexLogging

import scala.util.Failure

class ErgoModifiersCache(override val maxSize: Int) extends ModifiersCache with LRUCache with ScorexLogging {

  override def findCandidateKey(history: ErgoHistory): Option[K] = {
    def tryToApply(k: K, v: BlockSection): Boolean = {
      history.applicableTry(v) match {
        case Failure(e: MalformedModifierError) if history.contains(v.id) =>
          // already in history (e.g. stored from another source after the synchronizer passed this copy): a duplicate,
          // not an invalid modifier. Any fatal rule is matched on purpose, as in `validateAndSetStatus`
          log.info(s"Modifier ${v.encodedId} is already in history, removing its copy from cache: ${e.getMessage}")
          remove(k)
          false
        case Failure(e: MalformedModifierError) =>
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

}
