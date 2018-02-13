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
      val state = if (settings.nodeSettings.ADState) "digest" else "utxo"
      InfoRoute.makeInfoJson(nodeId, minerInfo, connectedPeers.length, readers, state,
        settings.scorexSettings.network.nodeName).toString shouldEqual responseAs[String]
    }
  }
}
