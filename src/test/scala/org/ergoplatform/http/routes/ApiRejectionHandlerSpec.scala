package org.ergoplatform.http.routes

import akka.http.scaladsl.model.{ContentTypes, FormData, HttpEntity, MediaTypes, StatusCodes}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.circe.parser.parse
import org.ergoplatform.http.api.{BlocksApiRoute, ScanApiRoute, TransactionsApiRoute}
import org.ergoplatform.http.{ErgoHttpService, NodePanelRoute, SwaggerRoute}
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.hash.Blake2b256

class ApiRejectionHandlerSpec
  extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val restApiSettings = settings.scorexSettings.restApi
  private val testApiKey = "test-api-key-for-rejection-handler-spec"

  private val compositeRoute: Route = ErgoHttpService(
    apiRoutes = Seq(
      TransactionsApiRoute(utxoReadersRef, nodeViewRef, settings),
      BlocksApiRoute(nodeViewRef, digestReadersRef, settings),
      ScanApiRoute(utxoReadersRef, settings)
    ),
    swaggerRoute = SwaggerRoute(restApiSettings, swaggerConfig = ""),
    panelRoute = NodePanelRoute()
  ).compositeRoute

  private val settingsWithAuth = settings.copy(
    scorexSettings = settings.scorexSettings.copy(
      restApi = restApiSettings.copy(
        apiKeyHash = Some(encoder.encode(Blake2b256(testApiKey)))
      )
    )
  )

  private val authCompositeRoute: Route = ErgoHttpService(
    apiRoutes = Seq(
      ScanApiRoute(utxoReadersRef, settingsWithAuth)
    ),
    swaggerRoute = SwaggerRoute(settingsWithAuth.scorexSettings.restApi, swaggerConfig = ""),
    panelRoute = NodePanelRoute()
  ).compositeRoute

  it should "return 415 UnsupportedMediaType for POST /transactions with application/x-www-form-urlencoded" in {
    val formEntity = FormData("tx" -> "{}").toEntity
    Post("/transactions", formEntity) ~> compositeRoute ~> check {
      status shouldBe StatusCodes.UnsupportedMediaType
      contentType shouldBe ContentTypes.`application/json`
      val json = parse(responseAs[String]).getOrElse(fail("Expected JSON error response"))
      json.hcursor.downField("error").as[Int] shouldBe Right(415)
      json.hcursor.downField("reason").as[String] shouldBe Right("unsupported.media.type")
      val detail = json.hcursor.downField("detail").as[String].getOrElse("")
      detail should include(MediaTypes.`application/x-www-form-urlencoded`.value)
      detail should include("application/json")
    }
  }

  it should "return 415 UnsupportedMediaType for POST /transactions with text/plain" in {
    val textEntity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, "{}")
    Post("/transactions", textEntity) ~> compositeRoute ~> check {
      status shouldBe StatusCodes.UnsupportedMediaType
      contentType shouldBe ContentTypes.`application/json`
      val json = parse(responseAs[String]).getOrElse(fail("Expected JSON error response"))
      json.hcursor.downField("error").as[Int] shouldBe Right(415)
      json.hcursor.downField("reason").as[String] shouldBe Right("unsupported.media.type")
      val detail = json.hcursor.downField("detail").as[String].getOrElse("")
      detail should include("text/plain")
      detail should include("application/json")
    }
  }

  it should "return 400 BadRequest for GET /blocks with malformed query parameter" in {
    Get("/blocks?limit=abc") ~> compositeRoute ~> check {
      status shouldBe StatusCodes.BadRequest
      contentType shouldBe ContentTypes.`application/json`
      val json = parse(responseAs[String]).getOrElse(fail("Expected JSON error response"))
      json.hcursor.downField("error").as[Int] shouldBe Right(400)
      json.hcursor.downField("reason").as[String] shouldBe Right("bad.request")
      val detail = json.hcursor.downField("detail").as[String].getOrElse("")
      detail should include("The query parameter 'limit' was malformed")
    }
  }

  it should "keep 403 Forbidden ahead of 415 UnsupportedMediaType when authentication fails" in {
    val textEntity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, "{}")
    Post("/scan/register", textEntity) ~> authCompositeRoute ~> check {
      status shouldBe StatusCodes.Forbidden
    }

    Post("/scan/register", textEntity).addHeader(RawHeader("api_key", testApiKey)) ~> authCompositeRoute ~> check {
      status shouldBe StatusCodes.UnsupportedMediaType
    }
  }

}
