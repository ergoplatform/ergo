package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.DefaultModifiersCache
import scorex.core.validation.RecoverableModifierError

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success}

class ErgoModifiersCache(override val maxSize: Int)
  extends DefaultModifiersCache[ErgoPersistentModifier, ErgoHistory](maxSize) {

  /**
    * Id's of block sections we are waiting to create block from full block
    */
  private val waitingIds: ArrayBuffer[K] = ArrayBuffer[K]()

  protected override def onRemove(key: K, rememberKey: Boolean): Unit = {
    waitingIds -= key
    super.onRemove(key, rememberKey)
  }

  /**
    * Defines a candidate to be applied.
    * Get candidate from short list if possible, do exhaustive search otherwise
    *
    * @param history - an interface to history which could be needed to define a candidate
    * @return - candidate if it is found
    */
  override def findCandidateKey(history: ErgoHistory): Option[K] = {
    waitingIds.flatMap(k => cache.get(k).map(v => k -> v)).find { case (k, v) =>
      history.applicableTry(v) match {
        case Failure(e) if e.isInstanceOf[RecoverableModifierError] =>
          // do nothing - modifier may be applied in future
          log.warn("Modifier from waitingIds list is not applicable now. Should avoid such situations")
          false
        case Failure(e) =>
          // non-recoverable error - remove modifier from cache
          // TODO blacklist peer who sent it
          log.warn(s"Modifier ${v.encodedId} is permanently invalid and will be removed from cache", e)
          remove(k, rememberKey = true)
          false
        case Success(_) =>
          true
      }
    }.map(_._1) orElse super.findCandidateKey(history)
  }.map { key =>
    cache.foreach { case (k: K, v: V) =>
      if (v.parentId sameElements key.array) waitingIds += k
    }
    cache.get(key) foreach {
      case h: Header =>
        waitingIds ++= history.requiredModifiersForHeader(h).map(k => new mutable.WrappedArray.ofByte(k._2))
      case _ =>
    }
    key
  }

}