package org.ergoplatform.api.routes

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import akka.testkit.TestDuration
import org.ergoplatform.Version
import org.ergoplatform.local.ErgoStatsCollector.NodeInfo
import org.ergoplatform.local.ErgoStatsCollectorRef
import org.ergoplatform.settings.Algos
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class InfoRoutesSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  val localInterface: ActorRef = ErgoStatsCollectorRef(nodeViewRef, settings)
  val route = InfoRoute(localInterface, settings.scorexSettings.restApi).route
  implicit val timeout = RouteTestTimeout(15.seconds dilated)
  private val votes = Algos.encode(Algos.hash(settings.scorexSettings.network.nodeName).take(5))
  val nodeInfo = NodeInfo(settings.scorexSettings.network.nodeName, Version.VersionString, 0, 0, "null",
    settings.nodeSettings.stateType, "null", isMining = settings.nodeSettings.mining, votes, None, None)

  it should "return info" in {

    Get("/info") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val stateType = settings.nodeSettings.stateType
      nodeInfo.json.spaces2 shouldEqual responseAs[String]
    }
  }
}
