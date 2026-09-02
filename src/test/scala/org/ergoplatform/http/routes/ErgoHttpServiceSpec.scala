package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{Level, Logger => LogbackLogger}
import ch.qos.logback.core.read.ListAppender
import org.ergoplatform.http.api.EmissionApiRoute
import org.ergoplatform.http.{ErgoHttpService, NodePanelRoute, SwaggerRoute}
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.LoggerFactory

import scala.collection.JavaConverters._

class ErgoHttpServiceSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  import org.ergoplatform.utils.ErgoNodeTestConstants._

  private val restApiSettings = settings.scorexSettings.restApi

  private val service = ErgoHttpService(
    apiRoutes = Seq(EmissionApiRoute(settings)),
    swaggerRoute = SwaggerRoute(restApiSettings, swaggerConfig = ""),
    panelRoute = NodePanelRoute()
  )

  private val route: Route = service.compositeRoute

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

}
