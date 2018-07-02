package org.ergoplatform.nodeView

import org.ergoplatform.modifiers.history.{ADProofs, BlockTransactions, Header}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.utils.{ErgoPropertyTest, HistorySpecification}
import scorex.crypto.hash.Blake2b256

import scala.collection.mutable

class ErgoModifiersCacheSpecification extends ErgoPropertyTest with HistorySpecification {

  private def genKey(i: Int): mutable.WrappedArray[Byte] = mutable.WrappedArray.make[Byte](Blake2b256(s"$i"))

  private def genCachePair(i: Int): (mutable.WrappedArray[Byte], Header) = {
    val header = invalidHeaderGen.sample.get
    val k = genKey(i)
    k -> header
  }

  property("cache size is within limits") {
    val limit = 3
    val modifiersCache = new ErgoModifiersCache(limit)

    modifiersCache.maxSize shouldBe limit

    (1 to limit).foreach { i =>
      val (k, h) = genCachePair(i)
      modifiersCache.put(k, h)
    }

    modifiersCache.size shouldBe limit

    val above = genCachePair(limit + 1)

    modifiersCache.put(above._1, above._2)
    modifiersCache.size shouldBe limit

    modifiersCache.remove(genKey(1)).isEmpty shouldBe true
  }

  property("cache is remembering removed modifier ids if rememberKey = true") {
    val limit = 3
    val modifiersCache = new ErgoModifiersCache(limit)

    (1 to limit).foreach { i =>
      val (k, h) = genCachePair(i)
      modifiersCache.put(k, h)
    }

    val k1 = genKey(1)
    modifiersCache.contains(k1) shouldBe true
    modifiersCache.remove(k1, rememberKey = true)
    modifiersCache.contains(k1) shouldBe true

    val k2 = genKey(2)
    modifiersCache.contains(k2) shouldBe true
    modifiersCache.remove(k2, rememberKey = false)
    modifiersCache.contains(k2) shouldBe false

    val k3 = genKey(3)
    modifiersCache.contains(k3) shouldBe true
    modifiersCache.remove(k3)
    modifiersCache.contains(k3) shouldBe false
  }

  property("cache is proposing a reasonable candidate to enhance history") {
    val limit = 25
    val modifiersCache = new ErgoModifiersCache(limit)

    val history0 = generateHistory(verifyTransactions = true, StateType.Utxo, PoPoWBootstrap = false, BlocksToKeep)

    val chain = genChain(5, history0)

    chain.foreach { fb =>
      modifiersCache.put(fb.header.id, fb.header)
      modifiersCache.put(fb.header.transactionsId, fb.blockTransactions)
      modifiersCache.put(fb.header.ADProofsId, fb.aDProofs.get)
    }

    //The history is empty - we can apply only a header at height == 0 at this moment.
    //Out of 15 elements in the cache, the cache should propose a proper candidate
    val c1 = modifiersCache.popCandidate(history0).get
    c1.isInstanceOf[Header] shouldBe true
    val h1 = c1.asInstanceOf[Header]
    h1.height shouldBe 0

    val history1 = history0.append(c1).get._1

    //We have only header of height == 0 in the history, so cache should return whether a header of height == 1
    //or a non-header part of the full block at height == 0
    val c2 = modifiersCache.popCandidate(history1).get
    val properCandidate = c2 match {
      case h: Header => h.height == 1
      case bt: BlockTransactions => bt.id sameElements h1.transactionsId
      case ap: ADProofs => ap.id sameElements h1.ADProofsId
    }
    properCandidate shouldBe true
  }
}