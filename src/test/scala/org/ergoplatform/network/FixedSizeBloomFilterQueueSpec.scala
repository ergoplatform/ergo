package org.ergoplatform.network

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FixedSizeBloomFilterQueueSpec extends AnyFlatSpec with Matchers {

  it should "create fixed size amount of bloom filters" in {
    val queue = FixedSizeBloomFilterQueue.empty(bloomFilterQueueSize = 5)
    val queue1 =
      queue
        .putAll((1 to 100).map(_.toString))
        .putAll((101 to 200).map(_.toString))
        .putAll((201 to 300).map(_.toString))

    (1 to 300).foreach { n =>
      assert(queue1.mightContain(n.toString), s"$n should be in bloom filter")
    }

    queue1.bloomFilterQueue.size shouldBe 3

    val queue2 =
      queue1
        .putAll(Vector("301"))
        .putAll(Vector("302"))
        .putAll(Vector("303"))
        .putAll(Vector("304"))
        .putAll(Vector("305"))
    queue2.bloomFilterQueue.size shouldBe 5

    (301 to 305).foreach { n =>
      assert(queue2.mightContain(n.toString), s"$n should be in bloom filter")
    }

  }

}
