package org.ergoplatform.http.routes

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.ergoplatform.http.api.ErgoBaseApiRoute
import org.ergoplatform.settings.RESTApiSettings
import org.ergoplatform.settings.Constants.TrueTree
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.InetSocketAddress
import scala.concurrent.duration._

class ErgoBaseApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest {

  private val restApiSettings =
    RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds, None)

  private class TestRoute(implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
    override val settings: RESTApiSettings = restApiSettings

    override val route: Route = Route.seal {
      path("tree") {
        post {
          ergoTree { _ =>
            complete(StatusCodes.OK)
          }
        }
      }
    }
  }

  private val route = new TestRoute().route

  it should "parse valid ErgoTree bodies" in {
    Post("/tree", TrueTree.bytesHex) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "reject hex bodies that are not valid ErgoTrees" in {
    Post("/tree", "00") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }
}
