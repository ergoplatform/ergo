package org.ergoplatform.it

import org.ergoplatform.it.api.NodeApi.NodeInfo
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ForkResolutionDiagnosticsSpec extends AnyFlatSpec with Matchers {
  import ForkResolutionDiagnostics._

  "Fork observations" should "retain independently accepted results while later selections change" in {
    val first = latch(Vector.fill[Option[String]](4)(None), Vector(Some("a"), None, Some("a"), None))
    val later = latch(first, Vector(None, Some("a"), None, Some("a")))
    later shouldBe Vector.fill(4)(Some("a"))
    latch(later, Vector.fill(4)(Some("b"))) shouldBe later
  }

  it should "retain missing results until their own predicate succeeds" in {
    latch(Vector(Some("a"), None), Vector(None, None)) shouldBe Vector(Some("a"), None)
  }

  it should "report the fixed target separately from the current selected header" in {
    val target = "ab" * 32
    val current = "cd" * 32
    val snapshot = Snapshot(Right(NodeInfo(None, None, Some(25), Some(20), None, Some(false))),
      Right(3), Right(Seq(current, target)), Right(20), reachable = true)
    val text = describe("match-anchor", 2, Some(10), Some(target), snapshot)
    text should include(s"fixed=Some($target)")
    text should include(s"selected=$current")
    text should include("headerHeight=Some(25) fullHeight=Some(20) mining=Some(false)")
    text should include("peerCount=3")
  }

  it should "exclude response text, addresses and paths from observed fields" in {
    val payload = "http://example.invalid/private/location"
    val snapshot = Snapshot(Right(NodeInfo(Some(payload), Some(payload), Some(1), Some(1), Some(payload), None)),
      Left(payload), Right(Seq(payload)), Right(1), reachable = true)
    val text = describe("match-anchor", 0, Some(1), Some(payload), snapshot)
    text should not include payload
    text should include("peerErrorClass=ObservationError")
    text should include("selected=invalid-header-id")
    text should include("fixed=Some(invalid-header-id)")
    describe("initial-height", 0, None, None,
      Snapshot(Left("TimeoutException"), Left("IOException"), Left("ParsingFailure"),
        Left("TimeoutException"), reachable = false)) should
      include("statusErrorClass=TimeoutException peerErrorClass=IOException headerErrorClass=ParsingFailure")
  }

  it should "keep supplementary failures out of the startup and height acceptance gates" in {
    val snapshot = Snapshot(Left("ParsingFailure"), Left("TimeoutException"), Left("IOException"),
      Right(20), reachable = true)
    startupReached(snapshot) shouldBe Some(())
    heightReached(20)(snapshot) shouldBe Some(20)
    heightReached(21)(snapshot) shouldBe None
    heightReached(20)(snapshot.copy(fullHeight = Left("ParsingFailure"))) shouldBe None
    startupReached(snapshot.copy(reachable = false)) shouldBe None
  }
}
