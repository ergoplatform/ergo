package org.ergoplatform.http.routes

import java.util.concurrent.atomic.AtomicInteger

import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.model.StatusCode
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.UniversalEntity
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.stream.scaladsl.Source
import akka.testkit.TestProbe
import akka.util.ByteString
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger => LogbackLogger}
import ch.qos.logback.core.read.ListAppender
import org.ergoplatform.http.api.BlocksApiRoute
import org.ergoplatform.http.api.EmissionApiRoute
import org.ergoplatform.http.api.MiningApiRoute
import org.ergoplatform.http.api.ScanApiRoute
import org.ergoplatform.http.{ErgoHttpService, NodePanelRoute, SwaggerRoute}
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory
import scorex.crypto.hash.Blake2b256

import scala.collection.JavaConverters._

class ErgoHttpServiceSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val restApiSettings = settings.scorexSettings.restApi
  private val testApiKey = "test-api-key-for-http-service-spec"

  private val service = ErgoHttpService(
    apiRoutes = Seq(EmissionApiRoute(settings)),
    swaggerRoute = SwaggerRoute(restApiSettings, swaggerConfig = ""),
    panelRoute = NodePanelRoute()
  )

  private val route: Route = service.compositeRoute

  private val settingsWithAuth = settings.copy(
    scorexSettings = settings.scorexSettings.copy(
      restApi = restApiSettings.copy(
        apiKeyHash = Some(encoder.encode(Blake2b256(testApiKey)))
      )
    )
  )

  private val authRoute: Route = ErgoHttpService(
    apiRoutes = Seq(
      ScanApiRoute(utxoReadersRef, settingsWithAuth),
      MiningApiRoute(minerRef, settingsWithAuth),
      BlocksApiRoute(nodeViewRef, digestReadersRef, settingsWithAuth)
    ),
    swaggerRoute = SwaggerRoute(
      settingsWithAuth.scorexSettings.restApi,
      swaggerConfig = ""
    ),
    panelRoute = NodePanelRoute()
  ).compositeRoute

  private val serviceLogger: LogbackLogger =
    LoggerFactory.getLogger(classOf[ErgoHttpService]).asInstanceOf[LogbackLogger]

  /** Runs `body` while capturing what the service logs at `level` */
  private def capturingLogs[T](level: Level)(body: => T): (T, Seq[String]) = {
    val appender = new ListAppender[ILoggingEvent]
    appender.start()
    val previousLevel = serviceLogger.getLevel
    serviceLogger.setLevel(level)
    serviceLogger.addAppender(appender)
    try {
      val result = body
      (result, appender.list.asScala.map(_.getFormattedMessage).toList)
    } finally {
      serviceLogger.detachAppender(appender)
      serviceLogger.setLevel(previousLevel)
      appender.stop()
    }
  }

  it should "log served queries at DEBUG level" in {
    val (_, messages) = capturingLogs(Level.DEBUG) {
      Get("/emission/at/100") ~> route ~> check {
        status shouldBe StatusCodes.OK
      }
    }

    val logged = messages.filter(_.startsWith("GET /emission/at/100"))
    logged.size shouldBe 1
    // method, uri, response status and elapsed time, and nothing else
    logged.head should fullyMatch regex """GET /emission/at/100 - 200 in \d+ ms"""
  }

  it should "log the query string, and log unmatched paths with the status they were rejected with" in {
    val (rejectedStatus, messages) = capturingLogs(Level.DEBUG) {
      Get("/emission/at/100?foo=bar") ~> route ~> check {
        status shouldBe StatusCodes.OK
      }
      Get("/no/such/route") ~> route ~> check {
        status.isSuccess() shouldBe false
        status.intValue()
      }
    }

    messages.exists(_.startsWith("GET /emission/at/100?foo=bar - 200 in ")) shouldBe true
    // rejections are turned into responses by the rejection handler, so they are logged too
    messages.exists(_.startsWith(s"GET /no/such/route - $rejectedStatus in ")) shouldBe true
  }

  it should "log nothing when the logger is not at DEBUG level" in {
    val (_, messages) = capturingLogs(Level.INFO) {
      Get("/emission/at/100") ~> route ~> check {
        status shouldBe StatusCodes.OK
      }
    }

    messages shouldBe empty
  }

  it should "not change the response when logging is enabled" in {
    val body = capturingLogs(Level.DEBUG) {
      Get("/emission/at/100") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[String]
      }
    }._1

    val bodyWithoutLogging = capturingLogs(Level.OFF) {
      Get("/emission/at/100") ~> route ~> check {
        status shouldBe StatusCodes.OK
        responseAs[String]
      }
    }._1

    body shouldBe bodyWithoutLogging
  }

  it should "prefer authorization failure to sibling method rejections" in {
    Post("/scan/register", "{}") ~> authRoute ~> check {
      status shouldBe StatusCodes.Forbidden
    }
  }

  private def checkStreamedRequest(path: String,
                                   body: String,
                                   expectedStatus: StatusCode,
                                   apiKey: Option[String] = None): Unit = {
    val bytes = ByteString(body)
    val pulled = TestProbe()
    val materializations = new AtomicInteger
    val source = Source.single(bytes).map { chunk =>
      pulled.ref ! chunk
      chunk
    }.mapMaterializedValue { materialized =>
      if (materializations.incrementAndGet() != 1) {
        throw new IllegalStateException("request body materialized twice")
      }
      materialized
    }
    val entity: UniversalEntity =
      HttpEntity.Default(ContentTypes.`application/json`, bytes.length.toLong, source)

    val request = Post(path, entity)
    val requestWithKey = apiKey.fold(request) { key =>
      request.addHeader(RawHeader("api_key", key))
    }
    requestWithKey ~> authRoute ~> check {
      status shouldBe expectedStatus
    }
    pulled.expectMsg(bytes)
    materializations.get() shouldBe 1
  }

  it should "discard a streamed unauthorized scan body exactly once" in {
    checkStreamedRequest("/scan/register", "{}", StatusCodes.Forbidden)
  }

  it should "reject and discard a malformed streamed mining body before decoding" in {
    checkStreamedRequest("/mining/candidateWithTxs", "{", StatusCodes.Forbidden)
  }

  it should "reject and discard a malformed streamed custom-key mining body" in {
    checkStreamedRequest("/mining/candidateWithTxsAndPk", "{", StatusCodes.Forbidden)
  }

  it should "reject and discard a malformed streamed block body before decoding" in {
    checkStreamedRequest("/blocks", "{", StatusCodes.Forbidden)
  }

  it should "decode a malformed streamed mining body after valid authentication" in {
    checkStreamedRequest("/mining/candidateWithTxs", "{", StatusCodes.BadRequest,
      Some(testApiKey))
  }

}
