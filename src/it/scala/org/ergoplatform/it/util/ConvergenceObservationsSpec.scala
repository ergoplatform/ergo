package org.ergoplatform.it.util

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.ergoplatform.it.api.NodeApi.NodeInfo

import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration._

class ConvergenceObservationsSpec extends AnyFlatSpec with Matchers {
  implicit private val ec: ExecutionContext = ExecutionContext.global

  private def withObserver(test: ConvergenceObservations => Unit): Unit = {
    val observer = new ConvergenceObservations
    try test(observer)
    finally observer.close()
  }

  "Selected header agreement" should "accept retained alternatives only when every first ID agrees" in {
    ConvergenceObservations.selectedHeadersAgree(Seq(Seq("a", "b"), Seq("a", "c"))) shouldBe true
    ConvergenceObservations.selectedHeadersAgree(Seq(Seq("a", "b"), Seq("b", "a"))) shouldBe false
    ConvergenceObservations.selectedHeadersAgree(Seq(Seq("a"), Seq.empty)) shouldBe false
    ConvergenceObservations.selectedHeadersAgree(Seq(Seq(""), Seq(""))) shouldBe false
    ConvergenceObservations.selectedHeadersAgree(Seq.empty) shouldBe false
  }

  "Full block agreement" should "require both heights and IDs and the original minimum height" in {
    val info = NodeInfo(Some("header"), Some("block"), Some(60), Some(50), None, None)
    ConvergenceObservations.sameBestBlock(info, info, 50) shouldBe true
    ConvergenceObservations.sameBestBlock(info, info, 51) shouldBe false
    ConvergenceObservations.sameBestBlock(info, info.copy(bestBlockHeightOpt = Some(51)), 50) shouldBe false
    ConvergenceObservations.sameBestBlock(info, info.copy(bestBlockIdOpt = Some("other")), 50) shouldBe false
    ConvergenceObservations.sameBestBlock(info.copy(bestBlockHeightOpt = None), info, 50) shouldBe false
    ConvergenceObservations.sameBestBlock(info, info.copy(bestBlockHeightOpt = None), 50) shouldBe false
    ConvergenceObservations.sameBestBlock(info.copy(bestBlockIdOpt = None), info, 50) shouldBe false
    ConvergenceObservations.sameBestBlock(info, info.copy(bestBlockIdOpt = None), 50) shouldBe false
  }

  it should "resample the entire group until its current selections agree" in withObserver { observer =>
    val samples = new AtomicInteger()
    val result = observer.until(2.seconds.fromNow, 1.millis, 100.millis) { _ =>
      val headers = if (samples.incrementAndGet() == 1) Seq(Seq("a", "b"), Seq("b", "a"))
                    else Seq(Seq("b", "a"), Seq("b"))
      Future.successful(headers)
    }(ConvergenceObservations.selectedHeadersAgree)("selected headers disagree")
    Await.result(result, 3.seconds).map(_.head) shouldBe Seq("b", "b")
    samples.get() shouldBe 2
  }

  it should "fail persistent disagreement within the original deadline with recent evidence" in withObserver { observer =>
    var recent = "none"
    val result = observer.until(100.millis.fromNow, 1.millis, 20.millis) { _ =>
      recent = "node0=a node1=b"
      Future.successful(Seq(Seq("a"), Seq("b")))
    }(ConvergenceObservations.selectedHeadersAgree)(s"last: $recent")
    intercept[TimeoutException](Await.result(result, 2.seconds)).getMessage should include("node0=a node1=b")
  }

  "Observation probes" should "bound a stalled endpoint and not start overlapping requests" in withObserver { observer =>
    val calls = new AtomicInteger()
    val never = Promise[Int]()
    val probe = observer.probe { calls.incrementAndGet(); never.future }
    Await.result(probe.sample(20.millis), 2.seconds) shouldBe Left("TimeoutException")
    Await.result(probe.sample(20.millis), 2.seconds) shouldBe Left("TimeoutException")
    calls.get() shouldBe 1
    never.success(3)
    Await.result(never.future, 2.seconds) shouldBe 3
    Await.result(probe.sample(100.millis), 2.seconds) shouldBe Right(3)
    calls.get() shouldBe 2
  }

  it should "keep successful status available while the peer sample times out" in withObserver { observer =>
    val status = observer.probe(Future.successful(42)).sample(100.millis)
    val peers = observer.probe(Promise[Int]().future).sample(30.millis)
    Await.result(status, 2.seconds) shouldBe Right(42)
    Await.result(status.zip(peers), 2.seconds) shouldBe (Right(42) -> Left("TimeoutException"))
  }

  it should "retain only an error class for endpoint failures" in withObserver { observer =>
    val result = observer.probe(Future.failed[Int](new IllegalArgumentException("private diagnostic payload")))
    Await.result(result.sample(100.millis), 2.seconds) shouldBe Left("IllegalArgumentException")
  }

  it should "enforce the convergence deadline even when observation never returns" in withObserver { observer =>
    val result = observer.until(30.millis.fromNow, 1.millis, 10.millis)(_ => Promise[Boolean]().future)(identity)("recent status")
    intercept[TimeoutException](Await.result(result, 2.seconds)).getMessage shouldBe "recent status"
  }

  "Observation identifiers" should "exclude arbitrary response text" in {
    ConvergenceObservations.headerId("ab" * 32) shouldBe "ab" * 32
    ConvergenceObservations.headerId("unexpected response text") shouldBe "invalid-header-id"
  }
}
