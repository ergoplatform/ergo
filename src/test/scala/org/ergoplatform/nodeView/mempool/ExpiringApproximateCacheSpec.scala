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
      bloomFilterCapacity       = 500,
      bloomFilterExpirationRate = 0.2,
      frontCacheSize            = 0,
      frontCacheExpiration      = 1.hour
    )
    cache.bloomFilterQueueSize shouldBe 5
    cache.bloomFilterApproxElemCount shouldBe 100
    val fullCache =
      (1 to 500).map(_.toString).foldLeft(cache) { case (acc, n) => acc.put(n) }

    (1 to 400).foreach { n =>
      assert(fullCache.mightContain(n.toString), s"$n should be in bloom filter")
    }

    fullCache.bloomFilterQueue.size shouldBe 5
    assert(
      fullCache.bloomFilterQueue.map(_._1) == Vector(4, 3, 2, 1, 0),
      "BF indexes must be 4-0"
    )
    assert(
      fullCache.approximateElementCount > 430,
      "At least 430 elements must be present"
    )

    // add some more elements over limit
    val newCache =
      (501 to 1000).map(_.toString).foldLeft(fullCache) { case (acc, n) => acc.put(n) }
    newCache.bloomFilterQueue.size shouldBe 5
    assert(
      newCache.bloomFilterQueue.map(_._1) == Vector(9, 8, 7, 6, 5),
      "BF indexes must be 9-5"
    )
    assert(newCache.approximateElementCount < 570, "Max 600 elements must be present")

    (601 to 800).foreach { n =>
      assert(newCache.mightContain(n.toString), s"$n should be in bloom filter")
    }
  }

  it should "have a fixed size expiring cache before bloom filters" in {
    val cache = ExpiringApproximateCache.empty(
      bloomFilterCapacity       = 500,
      bloomFilterExpirationRate = 0.2,
      frontCacheSize            = 100,
      frontCacheExpiration      = 500.millis
    )
    // let's add 100 elems directly to front cache
    val fullCache =
      (1 to 100).map(_.toString).foldLeft(cache) { case (acc, n) => acc.put(n) }

    (1 to 100).foreach { n =>
      assert(fullCache.mightContain(n.toString), s"$n should be in front cache")
    }
    fullCache.bloomFilterApproxElemCount shouldBe 100
    fullCache.frontCache.size shouldBe 100
    // now let's add another element which won't with to front cache so it goes to bloom filters
    val updatedCache = fullCache.put("101")
    updatedCache.frontCache.size shouldBe 100
    updatedCache.frontCache.contains("101") shouldBe false
    assert(updatedCache.mightContain("101"), s"101 should be in bloom filter")

    // test that all elements in front cache expire
    Thread.sleep(550)
    updatedCache.put("102").frontCache.size shouldBe 1
  }

  it should "handle millions of realistic elements" in {
    import org.scalactic._

    val cache = ExpiringApproximateCache.empty(
      bloomFilterCapacity       = 10000000,
      bloomFilterExpirationRate = 0.1,
      frontCacheSize            = 10000,
      frontCacheExpiration      = 1.hour
    )
    // let's add 2 millions of realistic elems to cache
    val elemCount = 2000000
    val uuids     = (1 to elemCount).map(_ => UUID.randomUUID().toString)
    val fullCache = uuids.foldLeft(cache) { case (acc, n) => acc.put(n) }

    val notIncludedUuids = uuids.filterNot(fullCache.mightContain)
    notIncludedUuids shouldBe empty

    implicit val approxEquality: Equality[Int] =
      TolerantNumerics.tolerantIntEquality(tolerance = 5000)

    fullCache.approximateElementCount.toInt === elemCount
  }

  it should "overflow properly" in {
    val cache = ExpiringApproximateCache.empty(
      bloomFilterCapacity       = 50000,
      bloomFilterExpirationRate = 0.25,
      frontCacheSize            = 10000,
      frontCacheExpiration      = 1.hour
    )
    // let's add 2 millions of realistic elems to cache
    val elemCount = 200000
    val uuids     = (1 to elemCount).map(_ => UUID.randomUUID().toString)
    val fullCache = uuids.foldLeft(cache) { case (acc, n) => acc.put(n) }

    uuids.forall(fullCache.mightContain) shouldBe false
    uuids.takeRight(10000).forall(fullCache.mightContain) shouldBe true // last elements there
  }
}
