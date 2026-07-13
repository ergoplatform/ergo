package scorex.core.api.http

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.{Directives, Route}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.ergoplatform.tools.ApiKeyHash
import org.ergoplatform.utils.ErgoNodeTestConstants
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.core.api.http.ApiDirectives.{DisabledApiKeyHash, LegacyDefaultApiKeyHash}

class ApiDirectivesSpec extends AnyFlatSpec with Matchers with ScalatestRouteTest {

  private def protectedRoute(apiKeyHash: String): Route = {
    val directives: ApiDirectives = new ApiDirectives {
      override val settings = ErgoNodeTestConstants.settings.scorexSettings.restApi.copy(
        apiKeyHash = Some(apiKeyHash)
      )
      override val apiKeyHeaderName: String = "api_key"
    }
    Route.seal(directives.withAuth {
      Directives.complete(StatusCodes.OK)
    })
  }

  "The disabled API key hash" should "reject protected routes with or without a key" in {
    Get() ~> protectedRoute(DisabledApiKeyHash) ~> check {
      status shouldBe StatusCodes.Forbidden
    }

    Get().addHeader(RawHeader("api_key", "anything")) ~> protectedRoute(
      DisabledApiKeyHash
    ) ~> check {
      status shouldBe StatusCodes.Forbidden
    }
  }

  it should "reject the legacy public default key" in {
    Get().addHeader(RawHeader("api_key", "hello")) ~>
    protectedRoute(LegacyDefaultApiKeyHash) ~> check {
      status shouldBe StatusCodes.Forbidden
    }
  }

  it should "allow an explicitly configured operator key" in {
    val operatorKey = "operator-test-key"

    Get().addHeader(RawHeader("api_key", operatorKey)) ~>
    protectedRoute(ApiKeyHash.hash(operatorKey)) ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "reject an incorrect key for an explicitly configured operator hash" in {
    val operatorKeyHash = ApiKeyHash.hash("operator-test-key")

    Get().addHeader(RawHeader("api_key", "wrong-key")) ~>
    protectedRoute(operatorKeyHash) ~> check {
      status shouldBe StatusCodes.Forbidden
    }
  }
}
