package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.DefaultModifiersCache
import scorex.core.validation.RecoverableModifierError

import scala.collection.mutable
import scala.util.{Failure, Success}

class ErgoModifiersCache(override val maxSize: Int)
  extends DefaultModifiersCache[ErgoPersistentModifier, ErgoHistory](maxSize) {

  /**
    * Defines a candidate to be applied.
    * Get candidate from short list if possible, do exhaustive search otherwise
    *
    * @param history - an interface to history which could be needed to define a candidate
    * @return - candidate if it is found
    */
  override def findCandidateKey(history: ErgoHistory): Option[K] = {
    history.missedModifiersForFullChain(maxSize, Seq())
      .find { m =>
        val key = new mutable.WrappedArray.ofByte(m._2)
        cache.get(key).exists { v =>
          history.applicableTry(v) match {
            case Failure(e) if e.isInstanceOf[RecoverableModifierError] =>
              // do nothing - modifier may be applied in future
              log.warn("Modifier from waitingIds list is not applicable now. Should avoid such situations")
              false
            case Failure(e) =>
              // non-recoverable error - remove modifier from cache
              // TODO blacklist peer who sent it
              log.warn(s"Modifier ${v.encodedId} is permanently invalid and will be removed from cache", e)
              remove(key, rememberKey = true)
              false
            case Success(_) =>
              true
          }
        }
      }.map(m => new mutable.WrappedArray.ofByte(m._2)) orElse super.findCandidateKey(history)
  }

}