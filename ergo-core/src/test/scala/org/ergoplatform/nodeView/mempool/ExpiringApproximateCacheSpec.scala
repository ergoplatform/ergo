package org.ergoplatform.nodeView.mempool

import org.scalactic.TripleEqualsSupport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.UUID
import scala.concurrent.duration._

class ExpiringApproximateCacheSpec
  extends AnyFlatSpec
  with Matchers
  with TripleEqualsSupport {

  it should "behave as fixed sized FIFO collection of bloom filters" in {
    val cache = ExpiringApproximateCache.empty(
      frontCacheSize            = 100,
      frontCacheExpiration      = 1.hour
    )
    cache.bloomFilterQueueSize shouldBe 4
    val fullCache =
      (1 to 500).map(_.toString).foldLeft(cache) { case (acc, n) => acc.put(n) }

    (1 to 400).foreach { n =>
      assert(fullCache.mightContain(n.toString), s"$n should be in bloom filter")
    }

    println("size: " + fullCache.bloomFilterQueue.size)

    fullCache.bloomFilterQueue.size shouldBe 4
    assert(
      fullCache.approximateElementCount > 430,
      "At least 430 elements must be present"
    )

    // add some more elements over limit
    val newCache =
      (501 to 1000).map(_.toString).foldLeft(fullCache) { case (acc, n) => acc.put(n) }
    newCache.bloomFilterQueue.size shouldBe 4
    assert(newCache.approximateElementCount < 570, "Max 600 elements must be present")

    (601 to 800).foreach { n =>
      assert(newCache.mightContain(n.toString), s"$n should be in bloom filter")
    }
  }

  it should "have a fixed size expiring cache before bloom filters" in {
    val cache = ExpiringApproximateCache.empty(
      frontCacheSize            = 100,
      frontCacheExpiration      = 50.millis
    )
    // let's add 100 elems directly to front cache
    val fullCache =
      (1 to 100).map(_.toString).foldLeft(cache) { case (acc, n) => acc.put(n) }

    (1 to 100).foreach { n =>
      assert(fullCache.mightContain(n.toString), s"$n should be in front cache")
    }
    fullCache.frontCache.size shouldBe 100
    // now let's add another element which won't with to front cache so it goes to bloom filters
    val updatedCache = fullCache.put("101")
    updatedCache.frontCache.size shouldBe 1
    updatedCache.frontCache.contains("101") shouldBe true
    assert(updatedCache.mightContain("100"), s"100 should be in bloom filter")

    val expiringCache = (102 to 200).map(_.toString).foldLeft(updatedCache) { case (acc, n) =>
      acc.put(n)
    }

    expiringCache.frontCache.size shouldBe 100

    // test that all elements in front cache expire
    Thread.sleep(200)
    val expriredCache = expiringCache.put("201")
    expriredCache.frontCache.size shouldBe 1
    // expired elements are not in the filters as well
    val fp = (102 to 200).map(_.toString).count(expriredCache.mightContain)
    (fp < 5) shouldBe true // no more than 5% false positives, with some extra buffer given
  }

  it should "overflow properly" in {
    val cache = ExpiringApproximateCache.empty(
      frontCacheSize            = 10000,
      frontCacheExpiration      = 1.hour
    )
    // let's add 2M of realistic elems to cache
    val elemCount = 2000000
    val uuids     = (1 to elemCount).map(_ => UUID.randomUUID().toString)
    val fullCache = uuids.foldLeft(cache) { case (acc, n) => acc.put(n) }

    uuids.forall(fullCache.mightContain) shouldBe false
    uuids.takeRight(10000).forall(fullCache.mightContain) shouldBe true // last elements there
    // first ids forgotten, except of false positives (with some buffer we add)
    val fp = uuids.take(10000).count(id => fullCache.mightContain(id))
    (fp < 100) shouldBe true // we allow up to 1% false positives
  }

}
