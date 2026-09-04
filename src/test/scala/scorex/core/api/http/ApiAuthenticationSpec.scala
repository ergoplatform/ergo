package scorex.core.api.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.{Directives, Route}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.ergoplatform.utils.ErgoNodeTestConstants
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base16

class ApiAuthenticationSpec extends AnyFlatSpec with Matchers with ScalatestRouteTest {
  private val apiKey = "api-authentication-test"
  private val keyHash = Base16.encode(Blake2b256(apiKey))

  private def routes(hash: Option[String]): Route = {
    val api = new ApiDirectives {
      override val settings = ErgoNodeTestConstants.settings.scorexSettings.restApi.copy(apiKeyHash = hash)
      override val apiKeyHeaderName = "api_key"
    }
    import Directives._
    Route.seal(
      path("protected") { api.withAuth { complete(StatusCodes.OK) } } ~
        path("public") { complete(StatusCodes.OK) }
    )
  }

  "Protected routes" should "reject requests when no API key is configured" in {
    Get("/protected") ~> routes(None) ~> check { status shouldBe StatusCodes.Forbidden }
    Get("/protected").withHeaders(RawHeader("api_key", apiKey)) ~> routes(None) ~> check {
      status shouldBe StatusCodes.Forbidden
    }
  }

  it should "reject empty and malformed configured hashes" in {
    Seq("", "not-a-hash", "00").foreach { hash =>
      Get("/protected").withHeaders(RawHeader("api_key", apiKey)) ~> routes(Some(hash)) ~> check {
        status shouldBe StatusCodes.Forbidden
      }
    }
  }

  it should "reject the former public default credential" in {
    Get("/protected").withHeaders(RawHeader("api_key", "hello")) ~>
      routes(Some(Base16.encode(Blake2b256("hello")))) ~> check {
      status shouldBe StatusCodes.Forbidden
    }
  }

  it should "reject missing and incorrect credentials for a configured key" in {
    Get("/protected") ~> routes(Some(keyHash)) ~> check { status shouldBe StatusCodes.Forbidden }
    Get("/protected").withHeaders(RawHeader("api_key", "incorrect")) ~> routes(Some(keyHash)) ~> check {
      status shouldBe StatusCodes.Forbidden
    }
  }

  it should "accept the configured credential" in {
    Get("/protected").withHeaders(RawHeader("api_key", apiKey)) ~> routes(Some(keyHash)) ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  "Public routes" should "remain available without an API key" in {
    Get("/public") ~> routes(None) ~> check { status shouldBe StatusCodes.OK }
  }
}
