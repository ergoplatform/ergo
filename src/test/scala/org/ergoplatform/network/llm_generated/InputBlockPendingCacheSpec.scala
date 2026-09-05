package org.ergoplatform.network.llm_generated

import org.ergoplatform.network.InputBlockPendingCache
import org.ergoplatform.network.ErgoNodeViewSynchronizer.InputBlockDiffData
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.util.ModifierId

class InputBlockPendingCacheSpec extends AnyPropSpec with Matchers {
  import org.ergoplatform.utils.generators.ErgoNodeTransactionGenerators._
  private def id(n: Int): ModifierId = ModifierId @@ n.toString
  private def data(time: Long = 0L): InputBlockDiffData =
    InputBlockDiffData(time, Seq(Array.fill(6)(1.toByte)), Seq.empty)
  private val weight: Long = InputBlockPendingCache.weightOf(data())

  property("defaults accommodate two ordinary chains and ID-array accounting for a transport envelope") {
    InputBlockPendingCache.MaxPeerEntries should be >= 2 * org.ergoplatform.settings.Parameters.SubsPerBlockDefault
    val idCount = org.ergoplatform.network.message.MessageConstants.MaxMessageSize.toLong / 6L
    InputBlockPendingCache.MaxPeerWeight should be >= 256L + idCount * 38L
  }

  property("retained transaction bytes and repeated references are charged") {
    val tx = validErgoTransactionGenTemplate(0, 0).sample.get._2
    val retained = data().copy(txs = Seq(tx, tx))
    InputBlockPendingCache.weightOf(retained) shouldBe weight + 2L * (64L + tx.size)
    val cache = new InputBlockPendingCache(10, 10, weight, weight, 100, () => 0L)
    cache.put(id(1), retained, "a", "a") shouldBe false
    cache.size shouldBe 0
    cache.retainedWeight shouldBe 0L
  }

  property("global and peer entry limits preserve admitted entries") {
    val cache = new InputBlockPendingCache(3, 2, weight * 10, weight * 10, 100, () => 0L)
    cache.put(id(1), data(), "peer-a", "peer-a") shouldBe true
    cache.put(id(2), data(), "peer-a", "peer-a") shouldBe true
    cache.put(id(3), data(), "peer-a", "peer-a") shouldBe false
    cache.put(id(3), data(), "peer-b", "peer-b") shouldBe true
    cache.put(id(4), data(), "peer-c", "peer-c") shouldBe false
    cache.size shouldBe 3
    cache.get(id(1)).isDefined shouldBe true
  }

  property("global retained weight is inclusive and released on completion") {
    val cache = new InputBlockPendingCache(10, 10, weight * 2, weight * 10, 100, () => 0L)
    cache.put(id(1), data(), "a", "a") shouldBe true
    cache.put(id(2), data(), "b", "b") shouldBe true
    cache.put(id(3), data(), "c", "c") shouldBe false
    cache.retainedWeight shouldBe weight * 2
    cache.remove(id(1))
    cache.retainedWeight shouldBe weight
    cache.put(id(3), data(), "c", "c") shouldBe true
  }

  property("peer retained weight is independent of the global budget") {
    val cache = new InputBlockPendingCache(10, 10, weight * 10, weight, 100, () => 0L)
    cache.put(id(1), data(), "a", "a") shouldBe true
    cache.put(id(2), data(), "a", "a") shouldBe false
    cache.put(id(2), data(), "b", "b") shouldBe true
    cache.removeConnection("a")
    cache.put(id(3), data(), "a", "a") shouldBe true
  }

  property("replacement charges only the difference and never steals another owner's entry") {
    val cache = new InputBlockPendingCache(1, 1, weight, weight, 100, () => 0L)
    cache.put(id(1), data(), "a", "a") shouldBe true
    cache.put(id(1), data(), "a", "a") shouldBe true
    cache.retainedWeight shouldBe weight
    cache.put(id(1), data(), "b", "b") shouldBe false
    val larger = data().copy(weakTxsIds = Seq(Array.fill(64)(1.toByte)))
    cache.put(id(1), larger, "a", "a") shouldBe false
    cache.retainedWeight shouldBe weight
    cache.get(id(1)).get.weakTxsIds.head.length shouldBe 6
  }

  property("expiry is enforced on access and admission without waiting for a cleanup tick") {
    var now = 0L
    val cache = new InputBlockPendingCache(1, 1, weight, weight, 100, () => now)
    cache.put(id(1), data(), "a", "a") shouldBe true
    now = 99
    cache.get(id(1)).isDefined shouldBe true
    cache.put(id(1), data(now), "a", "a") shouldBe true
    now = 100
    cache.get(id(1)) shouldBe None
    cache.retainedWeight shouldBe 0L
    cache.put(id(2), data(now), "b", "b") shouldBe true
    now = 200
    cache.put(id(3), data(now), "c", "c") shouldBe true
    cache.size shouldBe 1
  }

  property("abandonment and recovery clear all associated accounting") {
    val cache = new InputBlockPendingCache(3, 3, weight * 3, weight * 3, 100, () => 0L)
    cache.put(id(1), data(), "a", "a") shouldBe true
    cache.put(id(2), data(), "b", "b") shouldBe true
    cache.removeConnection("a")
    cache.get(id(1)) shouldBe None
    cache.retainedWeight shouldBe weight
    cache.clear()
    cache.size shouldBe 0
    cache.retainedWeight shouldBe 0L
    cache.put(id(3), data(), "a", "a") shouldBe true
  }

  property("disconnect releases only its connection while address quotas remain shared") {
    val cache = new InputBlockPendingCache(10, 2, weight * 10, weight * 2, 100, () => 0L)
    cache.put(id(1), data(), "shared-address", "connection-a") shouldBe true
    cache.put(id(2), data(), "shared-address", "connection-b") shouldBe true
    cache.put(id(1), data(), "shared-address", "connection-b") shouldBe false
    cache.put(id(3), data(), "shared-address", "connection-c") shouldBe false
    cache.removeConnection("connection-a")
    cache.get(id(1)) shouldBe None
    cache.get(id(2)).isDefined shouldBe true
    cache.retainedWeight shouldBe weight
    cache.put(id(3), data(), "shared-address", "connection-c") shouldBe true
    cache.put(id(4), data(), "shared-address", "connection-d") shouldBe false
    cache.retainedWeight shouldBe weight * 2
  }
}
