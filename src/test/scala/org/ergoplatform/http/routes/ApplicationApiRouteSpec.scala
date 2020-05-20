package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.{Decoder, Encoder}
import org.ergoplatform.ErgoBox
import org.ergoplatform.http.api.{ApiCodecs, ApplicationApiRoute}
import org.ergoplatform.nodeView.wallet.scanning.{ContainsScanningPredicate, ExternalAppRequest, ExternalApplicationJsonCodecs}
import org.ergoplatform.utils.Stubs
import org.scalatest.{FlatSpec, Matchers}
import io.circe.syntax._
import org.ergoplatform.http.api.ApplicationEntities.ApplicationIdWrapper
import org.ergoplatform.settings.{Args, ErgoSettings}

import scala.util.Try
import scala.concurrent.duration._

class ApplicationApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport
  with ApiCodecs {

  implicit val timeout: RouteTestTimeout = RouteTestTimeout(145.seconds)

  implicit val appReqEncoder: Encoder[ExternalAppRequest] = ExternalApplicationJsonCodecs.appReqEncoder
  implicit val appReqDecoder: Decoder[ExternalAppRequest] = ExternalApplicationJsonCodecs.appReqDecoder

  implicit val appIdWrapperEncoder: Encoder[ApplicationIdWrapper] = ApplicationIdWrapper.applicationIdWrapperEncoder
  implicit val appIdWrapperDecoder: Decoder[ApplicationIdWrapper] = ApplicationIdWrapper.applicationIdWrapperDecoder

  val prefix = "/application"

  val ergoSettings: ErgoSettings = ErgoSettings.read(
    Args(userConfigPathOpt = Some("src/test/resources/application.conf"), networkTypeOpt = None))
  val route: Route = ApplicationApiRoute(utxoReadersRef, ergoSettings).route

  val appRequest = ExternalAppRequest("demo", ContainsScanningPredicate(ErgoBox.R4, Array(0: Byte, 1: Byte)))

  it should "register an application" in {
    Post(prefix + "/register", appRequest.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[ApplicationIdWrapper]) shouldBe 'success
    }
  }

  it should "deregister an application" in {

  }

  it should "list registered applications" in {

  }

  it should "list unspent boxes for an application" in {

  }

  it should "stop tracking a box" in {

  }

}
