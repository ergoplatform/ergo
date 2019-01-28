package org.ergoplatform.api.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.api.MiningApiRoute
import org.ergoplatform.mining.external.ExternalAutolykosSolution
import org.ergoplatform.settings.ErgoSettings
import org.ergoplatform.utils.Stubs
import org.ergoplatform.utils.generators.ErgoGenerators
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class MiningApiRouteSpec
  extends FlatSpec
    with ErgoGenerators
    with Matchers
    with ScalatestRouteTest
    with Stubs
    with FailFastCirceSupport {

  val prefix = "/mining"

  val ergoSettings: ErgoSettings = ErgoSettings.read(Some("src/test/resources/application.conf"))
  val route: Route = MiningApiRoute(minerRef, settings).route

  val externalSolution = ExternalAutolykosSolution(genECPoint.sample.get, Array.fill(32)(9: Byte), BigInt(0))

  it should "return requested candidate" in {
    Get(prefix + "/candidate") ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[Json]) shouldBe 'success
    }
  }

  it should "process external solution" in {
    Post(prefix + "/solution", externalSolution.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

}
