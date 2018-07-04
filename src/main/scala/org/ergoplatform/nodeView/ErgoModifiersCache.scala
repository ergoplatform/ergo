package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.DefaultModifiersCache
import scorex.core.validation.RecoverableModifierError

import scala.collection.mutable
import scala.util.{Failure, Success}

class ErgoModifiersCache(override val maxSize: Int)
  extends DefaultModifiersCache[ErgoPersistentModifier, ErgoHistory](maxSize) {

  def keySet: scala.collection.Set[K] = cache.keySet

  /**
    * Defines a candidate to be applied.
    *
    * @param history - an interface to history which could be needed to define a candidate
    * @return - candidate if it is found
    */
  override def findCandidateKey(history: ErgoHistory): Option[K] = {
    def checkApplicability(key: K, v: ErgoPersistentModifier) = {
      history.applicableTry(v) match {
        case Failure(e) if e.isInstanceOf[RecoverableModifierError] =>
          // do nothing - modifier may be applied in future
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

    val required: scala.collection.Set[K] = history.missedModifiersKeySet
    val containing: scala.collection.Set[K] = cache.keySet
    val probableCandidateKeys = containing.intersect(required)

    probableCandidateKeys.find { key =>
      cache.get(key).exists { v =>
        checkApplicability(key, v)
      }
    } orElse {
      val curHeight = history.headersHeight
      cache.find { case (key, modifier) =>
        modifier match {
          case header: Header if header.height <= curHeight + 1 => checkApplicability(key, header)
          case _ => false
        }
      }.map(_._1)
    }
  }

}