package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.ErgoBox
import org.ergoplatform.http.api.ScanEntities.{ScanIdBoxId, ScanIdWrapper}
import org.ergoplatform.http.api.{ApiCodecs, ScanApiRoute}
import org.ergoplatform.nodeView.wallet.scanning._
import org.ergoplatform.settings.{Args, ErgoSettings}
import org.ergoplatform.utils.Stubs
import org.ergoplatform.wallet.Constants.ScanId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.ADKey
import scorex.utils.Random
import sigmastate.Values.ByteArrayConstant

import scala.concurrent.duration._
import scala.util.Try

class ScanApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport
  with ApiCodecs {

  import ScanJsonCodecs.{scanDecoder, scanReqEncoder}
  import ScanIdWrapper.{scanIdWrapperEncoder, scanIdWrapperDecoder}

  implicit val timeout: RouteTestTimeout = RouteTestTimeout(145.seconds)

  val prefix = "/scan"

  val ergoSettings: ErgoSettings = ErgoSettings.read(
    Args(userConfigPathOpt = Some("src/test/resources/application.conf"), networkTypeOpt = None))
  val route: Route = ScanApiRoute(utxoReadersRef, ergoSettings).route

  private val predicate0 = ContainsScanningPredicate(ErgoBox.R4, ByteArrayConstant(Array(0: Byte, 1: Byte)))
  private val predicate1 = ContainsScanningPredicate(ErgoBox.R4, ByteArrayConstant(Array(1: Byte, 1: Byte)))

  val appRequest = ScanRequest("demo", predicate0, Some(ScanWalletInteraction.Off), Some(false))
  val appRequest2 = ScanRequest("demo2", predicate1, Some(ScanWalletInteraction.Off), None)

  it should "register a scan" in {
    Post(prefix + "/register", appRequest.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[ScanIdWrapper]) shouldBe 'success
    }
  }

  it should "deregister a scan" in {
    var scanId: ScanIdWrapper = ScanIdWrapper(ScanId @@ (-1000: Short)) // improper value

    // first, register an app
    Post(prefix + "/register", appRequest.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[ScanIdWrapper])
      response shouldBe 'success
      scanId = response.get
    }

    // then remove it
    Post(prefix + "/deregister", scanId.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[ScanIdWrapper]) shouldBe 'success
    }

    // second time it should be not successful
    Post(prefix + "/deregister", scanId.asJson) ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

  it should "list registered scans" in {
    // register two apps
    Post(prefix + "/register", appRequest.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[ScanIdWrapper])
      response shouldBe 'success
    }

    Post(prefix + "/register", appRequest2.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[ScanIdWrapper])
      response shouldBe 'success
    }

    Get(prefix + "/listAll") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[Seq[Scan]])
      response shouldBe 'success
      val apps = response.get

      apps.map(_.scanName).contains(appRequest.scanName) shouldBe true
      apps.map(_.scanName).contains(appRequest2.scanName) shouldBe true
    }
  }

  it should "list unspent boxes for a scan with lower constraint" in {
    val minConfirmations = 15
    val minInclusionHeight = 20

    val suffix = s"/unspentBoxes/101?minConfirmations=$minConfirmations&minInclusionHeight=$minInclusionHeight"

    Get(prefix + suffix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[List[Json]])
      response shouldBe 'success
      response.get.nonEmpty shouldBe true // there are boxes that has confirmations > 15 and inclusionHeight > 20
      response.get.foreach { json =>
        json.hcursor.downField("confirmationsNum").as[Int].forall(_ >= minConfirmations) shouldBe true
        json.hcursor.downField("inclusionHeight").as[Int].forall(_ >= minInclusionHeight) shouldBe true
      }

      // unconfirmed box not returned
      response.get.flatMap(_.hcursor.downField("confirmationsNum").as[Option[Int]].toOption)
        .exists(_.isDefined == false) shouldBe false
    }
  }


  it should "list unspent boxes for a scan with upper constraint" in {
    val maxConfirmations = 15
    val maxInclusionHeight = 20

    val suffix = s"/unspentBoxes/101?maxConfirmations=$maxConfirmations&maxInclusionHeight=$maxInclusionHeight"

    Get(prefix + suffix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[List[Json]])
      response shouldBe 'success
      response.get.nonEmpty shouldBe true // there are boxes that has confirmations < 15 and inclusionHeight < 20
      response.get.foreach { json =>
        json.hcursor.downField("confirmationsNum").as[Int].forall(_ <= maxConfirmations) shouldBe true
        json.hcursor.downField("inclusionHeight").as[Int].forall(_ <= maxInclusionHeight) shouldBe true
      }
      // unconfirmed box not returned
      response.get.flatMap(_.hcursor.downField("confirmationsNum").as[Option[Int]].toOption)
        .exists(_.isDefined == false) shouldBe false
    }
  }


  it should "list unspent boxes for a scan with upper and lower constraints" in {
    val confirmations = 15
    val inclusionHeight = 20

    val suffix = s"/unspentBoxes/101?minConfirmations=$confirmations&minInclusionHeight=$inclusionHeight&maxConfirmations=$confirmations&maxInclusionHeight=$inclusionHeight"

    Get(prefix + suffix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[List[Json]])
      response shouldBe 'success
      response.get.nonEmpty shouldBe false // there are no boxes with confirmations and inclusionHeight within range
    }
  }

  it should "list unspent and unconfirmed boxes for a scan with lower constraint" in {
    val minConfirmations = -1
    val minInclusionHeight = 0

    val suffix = s"/unspentBoxes/101?minConfirmations=$minConfirmations&minInclusionHeight=$minInclusionHeight"

    Get(prefix + suffix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[List[Json]])
      response shouldBe 'success
      response.get.nonEmpty shouldBe true

      // unconfirmed box returned
      response.get.flatMap(_.hcursor.downField("confirmationsNum").as[Option[Int]].toOption)
        .exists(_.isDefined == false) shouldBe true
    }
  }

  it should "list spent boxes for a scan with lower constraint" in {
    val minConfirmations = 15
    val minInclusionHeight = 20

    val suffix = s"/spentBoxes/101?minConfirmations=$minConfirmations&minInclusionHeight=$minInclusionHeight"

    Get(prefix + suffix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[List[Json]])
      response shouldBe 'success
      response.get.nonEmpty shouldBe true
      response.get.foreach { json =>
        json.hcursor.downField("inclusionHeight").as[Int].forall(_ >= minInclusionHeight) shouldBe true
      }
      response.get.foreach { json =>
        json.hcursor.downField("spent").as[Boolean].forall(_ == true) shouldBe true
      }

      // unconfirmed box not returned
      response.get.flatMap(_.hcursor.downField("confirmationsNum").as[Option[Int]].toOption)
        .exists(_.isDefined == false) shouldBe false
    }
  }

  it should "list spent boxes for a scan with upper constraint" in {
    val maxConfirmations = 15
    val maxInclusionHeight = 20

    val suffix = s"/spentBoxes/101?maxConfirmations=$maxConfirmations&maxInclusionHeight=$maxInclusionHeight"

    Get(prefix + suffix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[List[Json]])
      response shouldBe 'success
      response.get.nonEmpty shouldBe true
      response.get.foreach { json =>
        json.hcursor.downField("confirmationsNum").as[Int].forall(_ <= maxConfirmations) shouldBe true
        json.hcursor.downField("inclusionHeight").as[Int].forall(_ <= maxInclusionHeight) shouldBe true
      }
      response.get.foreach { json =>
        json.hcursor.downField("spent").as[Boolean].forall(_ == true) shouldBe true
      }

      // unconfirmed box not returned
      response.get.flatMap(_.hcursor.downField("confirmationsNum").as[Option[Int]].toOption)
        .exists(_.isDefined == false) shouldBe false
    }
  }

  it should "list spent boxes boxes for a scan with upper and lower constraints" in {
    val confirmations = 15
    val inclusionHeight = 20

    val suffix = s"/spentBoxes/101?minConfirmations=$confirmations&minInclusionHeight=$inclusionHeight&maxConfirmations=$confirmations&maxInclusionHeight=$inclusionHeight"

    Get(prefix + suffix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[List[Json]])
      response shouldBe 'success
      response.get.nonEmpty shouldBe false // there are no spent boxes with confirmations and inclusionHeight within range
    }
  }

  it should "fail when maxInclusionHeight is specified and we consider unconfirmed" in {
    val minConfirmations = -1
    val maxInclusionHeight = 50

    val suffix = s"/unspentBoxes/101?minConfirmations=$minConfirmations&maxInclusionHeight=$maxInclusionHeight"

    Get(prefix + suffix) ~> route ~> check {
      rejection shouldEqual ValidationRejection("maxInclusionHeight cannot be specified when we consider unconfirmed")
    }
  }

  it should "stop tracking a box" in {
    val scanIdBoxId = ScanIdBoxId(ScanId @@ (51: Short), ADKey @@ Random.randomBytes(32))

    Post(prefix + "/stopTracking", scanIdBoxId.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "generate scan for p2s rule" in {
    Post(prefix + "/p2sRule", "Ms7smJmdbakqfwNo") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val res = responseAs[Json]
      res.hcursor.downField("scanId").as[Int].toOption.isDefined shouldBe true
    }

    Post(prefix + "/p2sRule", "s7smJmdbakqfwNo") ~> route ~> check {
      status shouldBe StatusCodes.BadRequest
    }
  }

}
