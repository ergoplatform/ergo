package org.ergoplatform.api.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class InfoRoutesSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  val route = InfoRoute(readersRef, minerRef, pmRef, settings, nodeId).route
  implicit val timeout = RouteTestTimeout(15.seconds dilated)

  it should "return info" in {
    Get("/info") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val stateType = settings.nodeSettings.stateType
      InfoRoute.makeInfoJson(nodeId, minerInfo, connectedPeers.length, readers, stateType,
        settings.scorexSettings.network.nodeName).toString shouldEqual responseAs[String]
    }
  }
}
