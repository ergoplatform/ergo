package org.ergoplatform.nodeView.mempool

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration._

class ExpiringFifoBloomFilterSpec extends AnyFlatSpec with Matchers {

  it should "behave as fixed sized FIFO collection of bloom filters" in {
    val bf = ExpiringFifoBloomFilter.empty(
      bloomFilterCapacity       = 500,
      bloomFilterExpirationRate = 0.2,
      cacheSize                 = 0,
      cacheExpiration           = 1.hour
    )
    bf.bloomFilterQueueSize shouldBe 5
    bf.bloomFilterApproxElemCount shouldBe 100
    val fullBf = (1 to 500).map(_.toString).foldLeft(bf) { case (acc, n) => acc.put(n) }

    (1 to 400).foreach { n =>
      assert(fullBf.mightContain(n.toString), s"$n should be in bloom filter")
    }

    fullBf.bloomFilterQueue.size shouldBe 5
    assert(
      fullBf.bloomFilterQueue.map(_._1) == Vector(4, 3, 2, 1, 0),
      "BF indexes must be 4-0"
    )
    assert(fullBf.approximateElementCount > 430, "At least 430 elements must be present")

    // add some more elements over limit
    val newBf =
      (501 to 1000).map(_.toString).foldLeft(fullBf) { case (acc, n) => acc.put(n) }
    newBf.bloomFilterQueue.size shouldBe 5
    assert(
      newBf.bloomFilterQueue.map(_._1) == Vector(9, 8, 7, 6, 5),
      "BF indexes must be 9-5"
    )
    assert(newBf.approximateElementCount < 570, "Max 600 elements must be present")

    (601 to 800).foreach { n =>
      assert(newBf.mightContain(n.toString), s"$n should be in bloom filter")
    }
  }

  it should "have a fixed size expiring cache before bloom filters" in {
    val bf = ExpiringFifoBloomFilter.empty(
      bloomFilterCapacity       = 500,
      bloomFilterExpirationRate = 0.2,
      cacheSize                 = 100,
      cacheExpiration           = 500.millis
    )
    // let's add 100 elems directly to front cache
    val fullBf = (1 to 100).map(_.toString).foldLeft(bf) { case (acc, n) => acc.put(n) }

    (1 to 100).foreach { n =>
      assert(fullBf.mightContain(n.toString), s"$n should be in front cache")
    }
    fullBf.frontCache.size shouldBe 100
    // now let's add another element which won't with to front cache so it goes to bloom filters
    val updatedBf = fullBf.put("101")
    updatedBf.frontCache.size shouldBe 100
    updatedBf.frontCache.contains("101") shouldBe false
    assert(updatedBf.mightContain("101"), s"101 should be in bloom filter")

    // test that all elements in front cache expire
    Thread.sleep(500)
    updatedBf.put("102").frontCache.size shouldBe 1
  }
}
