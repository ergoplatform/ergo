package org.ergoplatform.api.routes

import java.net.InetSocketAddress

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import org.scalatest.{FlatSpec, Matchers}
import scorex.core.settings.RESTApiSettings

import scala.concurrent.duration._

class InfoRoutesSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  val restApiSettings = RESTApiSettings(new InetSocketAddress("localhost", 8080), None, corsAllowed = false, 10.seconds)
  val route = InfoRoute(readersRef, minerRef, pmRef, digest = true, restApiSettings, nodeId).route
  implicit val timeout = RouteTestTimeout(15.seconds dilated)

  it should "return info" in {
    Get("/info") ~> route ~> check {
      status shouldBe StatusCodes.OK
      InfoRoute.makeInfoJson(nodeId, minerInfo, connectedPeers.length, readers, "digest").toString shouldEqual responseAs[String]
    }
  }
}
