package org.ergoplatform.it.util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

class StallWatchSpec extends AnyFlatSpec with Matchers {

  private class Clock {
    var now: Deadline = Deadline.now
    def advance(by: FiniteDuration): Unit = now = now + by
  }

  "Stall watch" should "count quiet time from its first sample, not its creation" in {
    val clock = new Clock
    val watch = new StallWatch[Int](() => clock.now)
    clock.advance(2.minutes) // an earlier phase of the scenario
    watch.record(None) shouldBe Duration.Zero
    clock.advance(5.seconds)
    watch.record(None) shouldBe 5.seconds
    watch.longestQuietPeriod shouldBe Duration.Zero // no key yet, so no quiet period
  }

  it should "restart on a changed key only, not on a repeated or missing one" in {
    val clock = new Clock
    val watch = new StallWatch[Int](() => clock.now)
    clock.advance(5.seconds)
    watch.record(Some(1)) shouldBe Duration.Zero
    clock.advance(30.seconds)
    watch.record(Some(1)) shouldBe 30.seconds
    clock.advance(10.seconds)
    watch.record(None) shouldBe 40.seconds
    clock.advance(1.second)
    watch.record(Some(2)) shouldBe Duration.Zero
    clock.advance(3.seconds)
    watch.record(Some(1)) shouldBe Duration.Zero
    watch.longestQuietPeriod shouldBe 41.seconds
  }
}
