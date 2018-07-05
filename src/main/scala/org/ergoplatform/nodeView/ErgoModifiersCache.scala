package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.DefaultModifiersCache
import scorex.core.validation.MalformedModifierError

import scala.collection.mutable
import scala.util.Failure

class ErgoModifiersCache(override val maxSize: Int)
  extends DefaultModifiersCache[ErgoPersistentModifier, ErgoHistory](maxSize) {

  protected override def onPut(key: K): Unit =
    super.onPut(key)

  protected override def onRemove(key: K, rememberKey: Boolean): Unit =
    super.onRemove(key, rememberKey)

  override def findCandidateKey(history: ErgoHistory): Option[K] = {
    def tryToApply(k: K, v: ErgoPersistentModifier): Boolean = {
      history.applicableTry(v) match {
        case Failure(e) if e.isInstanceOf[MalformedModifierError] =>
          log.warn(s"Modifier ${v.encodedId} is permanently invalid and will be removed from cache", e)
          remove(k, rememberKey = true)
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
        .flatMap(_.sectionIds.map(id => mutable.WrappedArray.make[Byte](id)))
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