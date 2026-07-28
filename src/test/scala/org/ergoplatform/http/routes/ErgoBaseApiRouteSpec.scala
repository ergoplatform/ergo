package org.ergoplatform.http.routes

import akka.actor.ActorRefFactory
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.syntax._
import org.ergoplatform.http.api.ErgoBaseApiRoute
import org.ergoplatform.settings.RESTApiSettings
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.InetSocketAddress
import scala.concurrent.duration._

class ErgoBaseApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with FailFastCirceSupport {

  private val restApiSettings =
    RESTApiSettings(new InetSocketAddress("localhost", 8080), None, None, 10.seconds, None)

  private class TestRoute(implicit val context: ActorRefFactory) extends ErgoBaseApiRoute {
    override val settings: RESTApiSettings = restApiSettings

    override val route: Route = Route.seal {
      pathPrefix("modifier") {
        get {
          modifierId { _ =>
            complete(StatusCodes.OK)
          }
        }
      } ~
      path("modifiers") {
        post {
          modifierIds { _ =>
            complete(StatusCodes.OK)
          }
        }
      }
    }
  }

  private val route = new TestRoute().route

  it should "reject modifier ids with invalid byte length" in {
    val validModifierId = "00" * 32

    Get(s"/modifier/$validModifierId") ~> route ~> check {
      status shouldBe StatusCodes.OK
    }

    Get("/modifier/00") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }

    Post("/modifiers", Seq(validModifierId).asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }

    Post("/modifiers", Seq("00").asJson) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }
}
