package org.ergoplatform.it.util

import io.circe.Json
import io.circe.parser.parse
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.IOException
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration._

class UtxoSyncFailureDiagnosticsSpec extends AnyFlatSpec with Matchers {
  private implicit val ec: ExecutionContext = ExecutionContext.global
  private val marker = "untrusted-extra-value"
  private val header = "ab" * 32

  "Failure diagnostics" should "preserve only typed diagnostic fields and exact cumulative scores" in {
    val score = BigInt("123456789012345678901234567890")
    val body = Json.obj(
      "headersHeight" -> Json.fromInt(13), "fullHeight" -> Json.fromInt(9),
      "headersScore" -> Json.fromBigInt(score), "fullBlocksScore" -> Json.fromBigInt(score - 4),
      "bestHeaderId" -> Json.fromString(header), "bestFullHeaderId" -> Json.fromString(header),
      "genesisBlockId" -> Json.fromString(header), "isMining" -> Json.True,
      "peersCount" -> Json.fromInt(3), "name" -> Json.fromString(marker),
      "address" -> Json.fromString(marker), "sampledAt" -> Json.fromString(marker))
    val result = UtxoSyncFailureDiagnostics.project(2, 123L, Right(body))
    result.hcursor.get[BigInt]("headersScore") shouldBe Right(score)
    result.hcursor.get[BigInt]("fullBlocksScore") shouldBe Right(score - 4)
    result.hcursor.get[String]("bestHeaderId") shouldBe Right(header)
    result.hcursor.get[Int]("headersHeight") shouldBe Right(13)
    result.hcursor.get[Int]("fullHeight") shouldBe Right(9)
    result.hcursor.get[Boolean]("isMining") shouldBe Right(true)
    result.hcursor.get[Int]("peersCount") shouldBe Right(3)
    result.hcursor.get[Int]("node") shouldBe Right(2)
    result.hcursor.get[Long]("sampledAt") shouldBe Right(123L)
    result.asObject.get.keys.toSet shouldBe Set("node", "sampledAt", "headersHeight", "fullHeight",
      "headersScore", "fullBlocksScore", "bestHeaderId", "bestFullHeaderId", "genesisBlockId", "isMining", "peersCount")
    result.noSpaces should not include marker
  }

  it should "mark malformed fields and preserve missing fields without copying their contents" in {
    val body = Json.obj("headersHeight" -> Json.fromString(marker), "fullHeight" -> Json.fromInt(-1),
      "headersScore" -> Json.arr(Json.fromString(marker)), "isMining" -> Json.fromString(marker),
      "bestHeaderId" -> Json.fromString(marker), "genesisBlockId" -> Json.obj("secret" -> Json.fromString(marker)))
    val result = UtxoSyncFailureDiagnostics.project(0, 1L, Right(body))
    Seq("headersHeight", "fullHeight", "headersScore", "isMining", "genesisBlockId").foreach { key =>
      result.hcursor.get[String](key) shouldBe Right("invalid")
    }
    result.hcursor.get[String]("bestHeaderId") shouldBe Right("invalid-header-id")
    Seq("fullBlocksScore", "peersCount", "bestFullHeaderId").foreach { key =>
      result.hcursor.downField(key).focus shouldBe Some(Json.Null)
    }
    result.noSpaces should not include marker
    UtxoSyncFailureDiagnostics.project(0, 1L, Right(Json.fromString(marker)))
      .hcursor.get[String]("errorClass") shouldBe Right("InvalidInfoResponse")
    UtxoSyncFailureDiagnostics.project(0, 1L, Left("bad/class: " + marker)).noSpaces should not include marker
  }

  it should "retain the original timeout when transport or request creation fails" in {
    val original = new TimeoutException("original assertion")
    var captured = ""
    val requests = Seq[() => Future[Json]](
      () => Future.successful(Json.obj("name" -> Json.fromString(marker))),
      () => Future.failed(new IOException(marker)),
      () => throw new IllegalStateException(marker),
      () => Future.successful(Json.fromString(marker)))
    val thrown = intercept[TimeoutException] {
      UtxoSyncFailureDiagnostics.rethrowAfterCapture(original, requests)(value => captured = value)
    }
    (thrown eq original) shouldBe true
    val snapshots = parse(captured).toOption.get.asArray.get
    snapshots.size shouldBe 4
    snapshots.map(_.hcursor.get[Int]("node").toOption.get) shouldBe Vector(0, 1, 2, 3)
    snapshots(1).hcursor.get[String]("errorClass") shouldBe Right("IOException")
    snapshots(2).hcursor.get[String]("errorClass") shouldBe Right("IllegalStateException")
    snapshots(3).hcursor.get[String]("errorClass") shouldBe Right("InvalidInfoResponse")
    captured should not include marker
  }

  it should "retain the original timeout if emitting diagnostics fails" in {
    val original = new TimeoutException("original assertion")
    val thrown = intercept[TimeoutException] {
      UtxoSyncFailureDiagnostics.rethrowAfterCapture(original, Seq(() => Future.successful(Json.obj()))) { _ =>
        throw new IllegalArgumentException(marker)
      }
    }
    (thrown eq original) shouldBe true
  }

  it should "start all pending requests and finish collection under its separate bound" in {
    val original = new TimeoutException("original assertion")
    val started = new AtomicInteger(0)
    val pending = Seq.fill(4)(Promise[Json]())
    var captured = ""
    val thrown = intercept[TimeoutException] {
      Await.result(Future {
        UtxoSyncFailureDiagnostics.rethrowAfterCapture(original, pending.map { promise => () =>
          started.incrementAndGet()
          promise.future
        })(value => captured = value)
      }, 4.seconds)
    }
    (thrown eq original) shouldBe true
    started.get() shouldBe 4
    parse(captured).toOption.get.asArray.get.foreach { snapshot =>
      snapshot.hcursor.get[String]("errorClass") shouldBe Right("TimeoutException")
    }
  }
}
