package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.ErgoPersistentModifier
import org.ergoplatform.modifiers.history.Header
import org.ergoplatform.nodeView.history.ErgoHistory
import scorex.core.{LRUCache, ModifiersCache}
import scorex.core.utils.ScorexLogging
import scorex.core.validation.RecoverableModifierError

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Random, Success}

class ErgoModifiersCache(override val maxSize: Int)
  extends ModifiersCache[ErgoPersistentModifier, ErgoHistory]
      with LRUCache[ErgoPersistentModifier, ErgoHistory]
      with ScorexLogging  {

  protected override def onPut(key: K): Unit =
    super.onPut(key)

  protected override def onRemove(key: K, rememberKey: Boolean): Unit =
    super.onRemove(key, rememberKey)

  override def findCandidateKey(history: ErgoHistory): Option[K] = {
    val headersHeight = history.headersHeight

    val maxIterations = 10

    @tailrec
    def cacheIteration(called: Int): Option[K] = {
      val expectedFullBlockParts = history
        .headerIdsAtHeight(history.fullBlockHeight + 1)
        .flatMap(id => history.typedModifierById[Header](id))
        .flatMap(h => Seq(mutable.WrappedArray.make[Byte](h.transactionsId), mutable.WrappedArray.make[Byte](h.ADProofsId)))
        .find(id => contains(id))
        .flatMap(id => cache.get(id))
        .toSeq

      val headerToApply =
        cache
          .find { case (_, p) => p.isInstanceOf[Header] && p.asInstanceOf[Header].height <= headersHeight + 1 }
          .map(_._2)
          .toSeq

      val mods = expectedFullBlockParts ++ headerToApply

      if(mods.isEmpty) None else {
        val mod = mods(Random.nextInt(mods.size))
        history.applicableTry(mod) match {
          case Failure(e) =>
            if (!e.isInstanceOf[RecoverableModifierError]) remove(mod.id, rememberKey = true)
            if (called == maxIterations - 1) None else cacheIteration(called + 1)
          case Success(_) => Some(mod.id)
        }
      }
    }

    cacheIteration(0)
  }
}