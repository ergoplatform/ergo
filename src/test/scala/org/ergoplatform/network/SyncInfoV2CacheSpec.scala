package org.ergoplatform.network

import org.ergoplatform.network.ErgoNodeViewSynchronizer.SyncInfoV2Cache
import org.ergoplatform.nodeView.history.ErgoSyncInfoV2
import org.ergoplatform.utils.generators.ChainGenerator.genHeaderChain
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

class SyncInfoV2CacheSpec extends AnyPropSpec with Matchers {
  private val headers = genHeaderChain(3, diffBitsOpt = None, useRealTs = false).headers
  private val tip = headers.last
  private val fullSummary = ErgoSyncInfoV2(Seq(tip, headers.head))
  private val reducedSummary = ErgoSyncInfoV2(Seq(tip))

  property("reuse a summary only for the same tip and requested mode") {
    Seq(false, true).foreach { full =>
      val cache = new SyncInfoV2Cache
      val expected = if (full) fullSummary else reducedSummary
      cache.getOrElseUpdate(Some(tip.id), full)(expected) shouldBe expected
      cache.getOrElseUpdate(Some(tip.id), full) {
        fail("An unchanged tip and mode should reuse the cached summary")
      } shouldBe expected
    }
  }

  property("alternating reduced and full requests preserves each requested summary") {
    val cache = new SyncInfoV2Cache
    Seq(false, true, false, true).foreach { full =>
      val expected = if (full) fullSummary else reducedSummary
      cache.getOrElseUpdate(Some(tip.id), full)(expected) shouldBe expected
    }
  }

  property("a different best header at the same height replaces the cached summary") {
    val otherTip = genHeaderChain(1, prefixOpt = Some(headers(1)),
      diffBitsOpt = None, useRealTs = false).last
    otherTip.height shouldBe tip.height
    otherTip.id should not be tip.id

    Seq(false, true).foreach { full =>
      val cache = new SyncInfoV2Cache
      val original = if (full) fullSummary else reducedSummary
      val replacement = ErgoSyncInfoV2(if (full) Seq(otherTip, headers.head) else Seq(otherTip))
      cache.getOrElseUpdate(Some(tip.id), full)(original) shouldBe original
      cache.getOrElseUpdate(Some(otherTip.id), full)(replacement) shouldBe replacement
      cache.getOrElseUpdate(Some(otherTip.id), full) {
        fail("The replacement tip should now be cached")
      } shouldBe replacement
    }
  }

  property("empty history and populated history do not share cached summaries") {
    val cache = new SyncInfoV2Cache
    val empty = ErgoSyncInfoV2(Nil)
    cache.getOrElseUpdate(None, full = true)(empty) shouldBe empty
    cache.getOrElseUpdate(None, full = true) {
      fail("An unchanged empty history should reuse its summary")
    } shouldBe empty
    cache.getOrElseUpdate(Some(tip.id), full = true)(fullSummary) shouldBe fullSummary
    cache.getOrElseUpdate(None, full = true)(empty) shouldBe empty
  }
}
