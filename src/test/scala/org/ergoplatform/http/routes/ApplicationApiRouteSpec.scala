package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.{Decoder, Encoder, Json}
import org.ergoplatform.ErgoBox
import org.ergoplatform.http.api.{ApiCodecs, ApplicationApiRoute}
import org.ergoplatform.nodeView.wallet.scanning.{ContainsScanningPredicate, ExternalAppRequest, ExternalApplication, ExternalApplicationJsonCodecs}
import org.ergoplatform.utils.Stubs
import org.scalatest.{FlatSpec, Matchers}
import io.circe.syntax._
import org.ergoplatform.http.api.ApplicationEntities.{ApplicationIdBoxId, ApplicationIdWrapper}
import org.ergoplatform.settings.{Args, ErgoSettings}
import org.ergoplatform.wallet.Constants.ApplicationId
import scorex.crypto.authds.ADKey
import scorex.utils.Random

import scala.util.Try
import scala.concurrent.duration._

class ApplicationApiRouteSpec extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport
  with ApiCodecs {

  implicit val timeout: RouteTestTimeout = RouteTestTimeout(145.seconds)

  implicit val appEncoder: Encoder[ExternalApplication] = ExternalApplicationJsonCodecs.appEncoder
  implicit val appDecoder: Decoder[ExternalApplication] = ExternalApplicationJsonCodecs.appDecoder

  implicit val appReqEncoder: Encoder[ExternalAppRequest] = ExternalApplicationJsonCodecs.appReqEncoder
  implicit val appReqDecoder: Decoder[ExternalAppRequest] = ExternalApplicationJsonCodecs.appReqDecoder

  implicit val appIdWrapperEncoder: Encoder[ApplicationIdWrapper] = ApplicationIdWrapper.applicationIdWrapperEncoder
  implicit val appIdWrapperDecoder: Decoder[ApplicationIdWrapper] = ApplicationIdWrapper.applicationIdWrapperDecoder

  val prefix = "/application"

  val ergoSettings: ErgoSettings = ErgoSettings.read(
    Args(userConfigPathOpt = Some("src/test/resources/application.conf"), networkTypeOpt = None))
  val route: Route = ApplicationApiRoute(utxoReadersRef, ergoSettings).route

  val appRequest = ExternalAppRequest("demo", ContainsScanningPredicate(ErgoBox.R4, Array(0: Byte, 1: Byte)))

  val appRequest2 = ExternalAppRequest("demo2", ContainsScanningPredicate(ErgoBox.R4, Array(1: Byte, 1: Byte)))

  it should "register an application" in {
    Post(prefix + "/register", appRequest.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[ApplicationIdWrapper]) shouldBe 'success
    }
  }

  it should "deregister an application" in {
    var appId: ApplicationIdWrapper = ApplicationIdWrapper(ApplicationId @@ (-1000: Short)) // improper value

    // first, register an app
    Post(prefix + "/register", appRequest.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[ApplicationIdWrapper])
      response shouldBe 'success
      appId = response.get
    }

    // then remove it
    Post(prefix + "/deregister", appId.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[ApplicationIdWrapper]) shouldBe 'success
    }

    // second time it should be not successful
    Post(prefix + "/deregister", appId.asJson) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "list registered applications" in {
    // register two apps
    Post(prefix + "/register", appRequest.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[ApplicationIdWrapper])
      response shouldBe 'success
    }

    Post(prefix + "/register", appRequest2.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[ApplicationIdWrapper])
      response shouldBe 'success
    }

    Get(prefix + "/listAll") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[Seq[ExternalApplication]])
      response shouldBe 'success
      val apps = response.get

      apps.map(_.appName).contains(appRequest.appName) shouldBe true
      apps.map(_.appName).contains(appRequest2.appName) shouldBe true
    }
  }

  it should "list unspent boxes for an application" in {
    val minConfirmations = 15
    val minInclusionHeight = 20

    val suffix = s"/unspentBoxes/101?minConfirmations=$minConfirmations&minInclusionHeight=$minInclusionHeight"

    Get(prefix + suffix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[List[Json]])
      response shouldBe 'success
      response.get.nonEmpty shouldBe true
      response.get.foreach { json =>
        json.hcursor.downField("confirmationsNum").as[Int].forall(_ >= minConfirmations) shouldBe true
        json.hcursor.downField("inclusionHeight").as[Int].forall(_ >= minInclusionHeight) shouldBe true
      }
    }
  }

  it should "stop tracking a box" in {
    val appIdBoxId = ApplicationIdBoxId(ApplicationId @@ (51: Short), ADKey @@ Random.randomBytes(32))

    Post(prefix + "/stopTracking", appIdBoxId.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

}
