package org.ergoplatform.http

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.ergoplatform.settings.RESTApiSettings
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.ergoplatform.utils.ScorexEncoder
import scorex.core.api.http.ApiRoute
import scorex.crypto.hash.Blake2b256

import java.net.InetSocketAddress
import scala.concurrent.duration._

class ErgoHttpServiceSpec extends AnyFlatSpec with Matchers with ScalatestRouteTest {

  private val endpoint = "/api-docs/swagger.conf"

  private def serviceRoute(corsAllowedOrigin: Option[String]): Route = {
    val settings = RESTApiSettings(
      bindAddress = new InetSocketAddress("127.0.0.1", 9053),
      apiKeyHash = None,
      corsAllowedOrigin = corsAllowedOrigin,
      timeout = 5.seconds,
      publicUrl = None
    )
    ErgoHttpService(
      apiRoutes = Seq.empty,
      swaggerRoute = SwaggerRoute(settings, "{}"),
      panelRoute = NodePanelRoute()
    ).compositeRoute
  }

  private def headerValue(response: HttpResponse, name: String): Option[String] =
    response.headers.find(_.is(name)).map(_.value())

  "ErgoHttpService" should "honor the configured CORS origin" in {
    val configuredOrigin = "https://trusted.example"

    Get(endpoint)
      .addHeader(RawHeader("Origin", configuredOrigin)) ~>
      serviceRoute(Some(configuredOrigin)) ~> check {
      status shouldBe StatusCodes.OK
      headerValue(response, "access-control-allow-origin") shouldBe Some(configuredOrigin)
      headerValue(response, "access-control-allow-credentials") shouldBe Some("true")
    }
  }

  it should "omit credentials for the wildcard CORS origin" in {
    Get(endpoint)
      .addHeader(RawHeader("Origin", "https://example.org")) ~>
      serviceRoute(Some("*")) ~> check {
      status shouldBe StatusCodes.OK
      headerValue(response, "access-control-allow-origin") shouldBe Some("*")
      headerValue(response, "access-control-allow-credentials") shouldBe None
    }
  }

  it should "not reflect an untrusted request origin" in {
    val configuredOrigin = "https://trusted.example"

    Get(endpoint)
      .addHeader(RawHeader("Origin", "https://evil.example")) ~>
      serviceRoute(Some(configuredOrigin)) ~> check {
      status shouldBe StatusCodes.OK
      headerValue(response, "access-control-allow-origin") shouldBe Some(configuredOrigin)
    }
  }

  it should "disable CORS handling when no origin is configured" in {
    Get(endpoint)
      .addHeader(RawHeader("Origin", "https://example.org")) ~>
      serviceRoute(None) ~> check {
      status shouldBe StatusCodes.OK
      headerValue(response, "access-control-allow-origin") shouldBe None
      headerValue(response, "access-control-allow-headers") shouldBe None
    }

    Options(endpoint)
      .addHeader(RawHeader("Origin", "https://example.org"))
      .addHeader(RawHeader("Access-Control-Request-Method", "GET")) ~>
      serviceRoute(None) ~> check {
      status.isFailure() shouldBe true
      headerValue(response, "access-control-allow-origin") shouldBe None
    }
  }

  it should "preserve the supported preflight methods and headers" in {
    val configuredOrigin = "https://trusted.example"

    Options(endpoint)
      .addHeader(RawHeader("Origin", configuredOrigin))
      .addHeader(RawHeader("Access-Control-Request-Method", "POST"))
      .addHeader(RawHeader("Access-Control-Request-Headers", "api_key, sentry-trace")) ~>
      serviceRoute(Some(configuredOrigin)) ~> check {
      status shouldBe StatusCodes.OK
      headerValue(response, "access-control-allow-origin") shouldBe Some(configuredOrigin)

      val allowedMethods = headerValue(response, "access-control-allow-methods")
        .toSeq.flatMap(_.split(",")).map(_.trim).toSet
      allowedMethods shouldBe Set("OPTIONS", "POST", "PUT", "GET", "DELETE")

      val allowedHeaders = headerValue(response, "access-control-allow-headers")
        .toSeq.flatMap(_.split(",")).map(_.trim).toSet
      allowedHeaders shouldBe Set(
        "Authorization",
        "Content-Type",
        "X-Requested-With",
        "api_key",
        "openai-conversation-id",
        "openai-ephemeral-user-id",
        "baggage",
        "sentry-trace"
      )
    }
  }

  Seq(Some("https://trusted.example"), Some("*"), None).foreach { origin =>
    it should s"preserve API key authentication with CORS origin $origin" in {
      val key = "cors-test-key"
      val protectedSettings = RESTApiSettings(
        bindAddress = new InetSocketAddress("127.0.0.1", 9053),
        apiKeyHash = Some(ScorexEncoder.default.encode(Blake2b256(key))),
        corsAllowedOrigin = origin,
        timeout = 5.seconds,
        publicUrl = None
      )
      var executions = 0
      val protectedApi = new ApiRoute {
        override val context = system
        override val settings: RESTApiSettings = protectedSettings
        override val route: Route = path("protected") {
          get {
            withAuth {
              executions += 1
              complete(StatusCodes.OK)
            }
          }
        }
      }
      val route = ErgoHttpService(
        Seq(protectedApi), SwaggerRoute(protectedSettings, "{}"), NodePanelRoute()
      ).compositeRoute

      Seq(None, Some("incorrect-key"), Some(key)).foreach { supplied =>
        val request = Get("/protected").addHeader(RawHeader("Origin", "https://trusted.example"))
        val authenticated = supplied.fold(request)(value => request.addHeader(RawHeader("api_key", value)))
        authenticated ~> route ~> check {
          status shouldBe (if (supplied.contains(key)) StatusCodes.OK else StatusCodes.Forbidden)
          executions shouldBe (if (supplied.contains(key)) 1 else 0)
        }
      }
    }
  }
}
