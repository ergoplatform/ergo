package org.ergoplatform.network

import org.ergoplatform.network.FixedSizeApproximateCacheQueue.{ApproxCache, ConciseCache, elemCountApproxThreshold}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FixedSizeApproximateCacheQueueSpec extends AnyFlatSpec with Matchers {

  it should "create fixed size amount of caches" in {
    val queue = FixedSizeApproximateCacheQueue.empty(cacheQueueSize = 5)
    val queue1 =
      queue
        .putAll((1 to 100).map(_.toString))
        .putAll((101 to 200).map(_.toString))
        .putAll((201 to 300).map(_.toString))

    (1 to 300).foreach { n =>
      assert(queue1.mightContain(n.toString), s"$n should be in cache")
    }

    queue1.cacheQueue.size shouldBe 3

    val queue2 =
      queue1
        .putAll(Vector("301"))
        .putAll(Vector("302"))
        .putAll(Vector("303"))
        .putAll(Vector("304"))
        .putAll(Vector("305"))
    queue2.cacheQueue.size shouldBe 5

    (301 to 305).foreach { n =>
      assert(queue2.mightContain(n.toString), s"$n should be in cache")
    }
  }

  it should "create different types of caches based on reaching approx threshold" in {
    val queue = FixedSizeApproximateCacheQueue.empty(cacheQueueSize = 2)

    val fewElemQueue = queue.putAll((1 to 10).map(_.toString))
    assert(fewElemQueue.cacheQueue.head.isInstanceOf[ConciseCache], "Few elements should go to concise cache")

    val manyElemQueue = queue.putAll((1 to (elemCountApproxThreshold + 10)).map(_.toString))
    assert(manyElemQueue.cacheQueue.head.isInstanceOf[ApproxCache], "Many elements should go to approximate cache")

    val combinedQueue = manyElemQueue.putAll((1 to 10).map(_.toString))
    assert(combinedQueue.cacheQueue.head.isInstanceOf[ConciseCache], "Few elements should go to concise cache")
    assert(combinedQueue.cacheQueue.last.isInstanceOf[ApproxCache], "Many elements should go to approximate cache")
  }

}
