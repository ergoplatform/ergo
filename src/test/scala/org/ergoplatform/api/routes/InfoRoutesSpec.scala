package org.ergoplatform.api.routes

import java.net.InetSocketAddress

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import io.circe.syntax._
import org.ergoplatform.api.routes.InfoRoute.Info
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.settings.RESTApiSettings

import scala.concurrent.duration._

class InfoRoutesSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds)
  val route = InfoRoute(readersRef, minerRef, pmRef, digest = true, restApiSettings, nodeId).route
  implicit val timeout = RouteTestTimeout(15.seconds dilated)

  it should "return info" in {
    Get("/info") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val resp = responseAs[String]
      Info(nodeId, minerInfo, connectedPeers.length, readers, "digest").asJson.toString shouldEqual resp
    }
  }
}
