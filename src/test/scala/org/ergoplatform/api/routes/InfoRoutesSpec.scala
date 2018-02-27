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
import scorex.core.utils.NetworkTime.Time
import scorex.core.utils.NetworkTimeProvider

import scala.concurrent.duration._

class InfoRoutesSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  val fakeTimeProvider: NetworkTimeProvider = new NetworkTimeProvider(settings.scorexSettings.ntp) {
    override def time(): Time = 123
  }

  val localInterface: ActorRef = ErgoStatsCollectorRef(nodeViewRef, settings, fakeTimeProvider)
  val route = InfoRoute(localInterface, settings.scorexSettings.restApi).route
  implicit val timeout = RouteTestTimeout(15.seconds dilated)
  private val votes = Algos.encode(Algos.hash(settings.scorexSettings.network.nodeName).take(5))
  val nodeInfo = NodeInfo(settings.scorexSettings.network.nodeName, Version.VersionString, 0, 0, "null",
    settings.nodeSettings.stateType, "null", isMining = settings.nodeSettings.mining, votes, None, None,
    fakeTimeProvider.time())

  it should "return info" in {

    Get("/info") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val stateType = settings.nodeSettings.stateType
      nodeInfo.json.spaces2 shouldEqual responseAs[String]
    }
  }
}
