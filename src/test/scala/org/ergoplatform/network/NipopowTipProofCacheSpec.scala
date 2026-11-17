package org.ergoplatform.network

import org.ergoplatform.network.NipopowTipProofCache.{Build, Serve, Unavailable, Wait}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.util.{ModifierId, bytesToId}

class NipopowTipProofCacheSpec extends AnyPropSpec with Matchers {

  private def id(b: Byte): ModifierId = bytesToId(Array.fill(32)(b))

  private val onBestChain: (ModifierId, Int) => Boolean = (_, _) => true
  private val proof = Array[Byte](1, 2, 3)

  property("a proof for the current tip is built once and kept for maxAgeBlocks blocks") {
    val cache = new NipopowTipProofCache(maxAgeBlocks = 10)
    cache.request(id(1), 100, onBestChain) shouldBe Build(id(1), 100)
    // a second request while the build is under way does not start another
    cache.request(id(1), 100, onBestChain) shouldBe Wait
    cache.request(id(2), 101, onBestChain) shouldBe Wait
    cache.built(Build(id(1), 100), id(1), 100, Some(proof)) shouldBe true
    cache.request(id(2), 101, onBestChain) shouldBe Serve(proof)
    cache.request(id(3), 109, onBestChain) shouldBe Serve(proof)
    // ten blocks after the header the proof was built for, a new one is built
    cache.request(id(4), 110, onBestChain) shouldBe Build(id(4), 110)
  }

  property("a failed build is not tried again before maxAgeBlocks blocks have passed") {
    val cache = new NipopowTipProofCache(maxAgeBlocks = 10)
    cache.request(id(1), 100, onBestChain) shouldBe Build(id(1), 100)
    cache.built(Build(id(1), 100), id(1), 100, None) shouldBe true
    cache.request(id(1), 100, onBestChain) shouldBe Unavailable
    cache.request(id(2), 109, onBestChain) shouldBe Unavailable
    cache.request(id(3), 110, onBestChain) shouldBe Build(id(3), 110)
  }

  property("a kept proof is not used when its header is not on the best chain any more") {
    val cache = new NipopowTipProofCache(maxAgeBlocks = 10)
    cache.request(id(1), 100, onBestChain) shouldBe Build(id(1), 100)
    cache.built(Build(id(1), 100), id(1), 100, Some(proof)) shouldBe true
    // the best chain was replaced by one which does not hold the header the proof was built for
    cache.request(id(5), 102, (_, _) => false) shouldBe Build(id(5), 102)
    cache.built(Build(id(5), 102), id(5), 102, Some(proof)) shouldBe true
    // the best header is below the one the proof was built for
    cache.request(id(6), 101, onBestChain) shouldBe Build(id(6), 101)
  }

  property("when a build has not ended after maxAgeBlocks blocks another is started, and the first one's result is not kept") {
    val cache = new NipopowTipProofCache(maxAgeBlocks = 10)
    cache.request(id(1), 100, onBestChain) shouldBe Build(id(1), 100)
    cache.request(id(2), 109, onBestChain) shouldBe Wait
    // the build started at height 100 has not ended: another is started
    cache.request(id(3), 110, onBestChain) shouldBe Build(id(3), 110)
    cache.request(id(3), 110, onBestChain) shouldBe Wait
    // the first build ends after all: its result is not kept, and the second is still waited for
    cache.built(Build(id(1), 100), id(1), 100, Some(proof)) shouldBe false
    cache.request(id(4), 111, onBestChain) shouldBe Wait
    val later = Array[Byte](4, 5, 6)
    cache.built(Build(id(3), 110), id(3), 110, Some(later)) shouldBe true
    cache.request(id(4), 111, onBestChain) shouldBe Serve(later)
  }

  property("a proof which ends at a later header than the request named is kept under the header it ends at") {
    val cache = new NipopowTipProofCache(maxAgeBlocks = 10)
    cache.request(id(1), 100, onBestChain) shouldBe Build(id(1), 100)
    // the best header moved on while the proof was built: the proof ends at height 101
    cache.built(Build(id(1), 100), id(2), 101, Some(proof)) shouldBe true
    cache.request(id(2), 101, onBestChain) shouldBe Serve(proof)
    // its age is counted from the header it ends at
    cache.request(id(3), 110, onBestChain) shouldBe Serve(proof)
    cache.request(id(4), 111, onBestChain) shouldBe Build(id(4), 111)
    cache.built(Build(id(4), 111), id(4), 111, Some(proof)) shouldBe true
    // the header a request named is still on the best chain, the one the proof ends at is not: not served
    cache.request(id(5), 112, (headerId, _) => headerId != id(4)) shouldBe Build(id(5), 112)
  }

}
