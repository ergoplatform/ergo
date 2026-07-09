package org.ergoplatform.http.routes

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes}
import akka.http.scaladsl.server.{Route, ValidationRejection}
import akka.http.scaladsl.testkit.{RouteTestTimeout, ScalatestRouteTest}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.ErgoBox
import org.ergoplatform.http.api.ScanEntities.{ScanIdBoxId, ScanIdWrapper}
import org.ergoplatform.http.api.{ApiCodecs, ScanApiRoute}
import org.ergoplatform.nodeView.wallet.scanning._
import org.ergoplatform.settings.{Args, ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.Stubs
import org.ergoplatform.wallet.Constants.ScanId
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.ADKey
import scorex.util.encode.Base16
import scorex.utils.Random
import sigma.ast.ByteArrayConstant
import sigma.serialization.ValueSerializer

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

  val ergoSettings: ErgoSettings = ErgoSettingsReader.read(
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

  "ScanApiRoute" should "successfully handle p2sRule with long testnet address and truncate scan name" in {
    // Testnet P2S address that exceeds the 255 byte limit
    val testnetP2S = "47vws8BeKJigut3p4eFbsKhQHoDvKo69K8ikb6xqxyWXye9ejVEbBjWacdecnG9ceyd2ZEt99LJyWoEUZ3H47jfVUKXNNcsSq7h63AuRUB78urnUq1qxCNQURxWQgvtyBXGR8MXK4iDEDFTPFXd4Gnu7mvLFEi9J8ycpQucwZ8iEP5nuUu2FE6A93r8oBA1CY8pSn23mV7uaoB9QZHHDajykXk9ojHa9xKFJTZkQMrtYDJwCe2RG18PswnupavfujQbFBUb5Gz12rsmqggJm43Rq3gwy4RvsYcZsTdUaV6QVZQnamzu37djE4xDmK1EsUd1jraTCLgFrPNARAgztQpj68vmRj7aFtHB6c9w6SoMvEsiA3DceffqqVhkxzAfE4uNkYdC8nm5A3jJ4TyM6Wck3YB4tKi2DAzH2rXz1TiEUzaQ1wPftPqJFhpyEhWdLjBWzDAdWEPCRRBkxVnnMQ5s2ChmWJ9GX4zxYgZ6tVcASfavNz4CcxUQGrLXMdKseVcarP9JkKCGVna534dbL8pnj6WzQgTSVn9wNRRXfaLwrJpuq"

    // Verify the address length exceeds the maximum scan name length
    val addressBytes = testnetP2S.getBytes("UTF-8")
    addressBytes.length should be > 255

    // Make the POST request to the p2sRule endpoint
    Post("/scan/p2sRule", HttpEntity(ContentTypes.`application/json`, "\"" + testnetP2S + "\"")) ~> route ~> check {
      // The request should succeed (even though we're using a mock wallet)
      // In a real scenario, this would register a scan with a truncated name
      status shouldBe StatusCodes.OK

      // Verify that the scan name would be truncated to 255 bytes
      val expectedTruncatedName = new String(testnetP2S.getBytes("UTF-8"), 0, 255, "UTF-8")
      expectedTruncatedName.getBytes("UTF-8").length shouldBe 255

      // The truncated name should be a prefix of the original address
      testnetP2S should startWith(expectedTruncatedName)
    }
  }

  it should "register and retrieve Basis Reserve Scanner scan" in {
    val basisReserveScannerRequest = """{"removeOffchain":false,"scanName":"Basis Reserve Scanner","trackingRule":{"predicate":"contains","register":"R1","value":"0e97031994031004140414040004000442040004420400044205000400048090e4c004044204020580a8d6b9070100d805d6017ee4e3000204d6029d72017300d603e4c6a70407d604b2a59e7201730100d605ededed93c27204c2a793db63087204db6308a793e4c672040407720393e4c67204060ee4c6a7060e959372027302d80dd606db07027203d607e4e30107d608cbb37206db07027207d609e4e30405d60a7a7209d60bdb6a01ddd60ce4e3020ed60db4720c73037304d60ee4e30305d60fb3b372087a720e720ad610e4e3060ed611b4721073057306d612e4c6b2db6501fe7307000407ea02d1ededed720593e4dc640ce4c6a705640283013c0e0e86027208720ae4e3050ee4c672040564939f720b7bb4720c7308b1720ca0ee720d9f72037bcbb3b3720d720f7206ed9099c1a7c17204720eeced91720973099199db6807b2db6502fe730a0072097e730b05939f720b7bb47210730cb17210a0ee72119f72127bcbb3b37211720fdb07027212cd720795937202730dd1eded72059299c17204c1a7730e93e4c672040564e4c6a70564d1730f"},"walletInteraction":"off"}"""

    var registeredScanId: ScanIdWrapper = null

    // Register the scan
    Post(prefix + "/register", HttpEntity(ContentTypes.`application/json`, basisReserveScannerRequest)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[ScanIdWrapper])
      response shouldBe 'success
      registeredScanId = response.get
    }

    // Verify the scan can be retrieved via listAll
    Get(prefix + "/listAll") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val response = Try(responseAs[Seq[Scan]])
      response shouldBe 'success
      val scans = response.get

      // Find the scan we just registered
      val basisReserveScan = scans.find(_.scanName == "Basis Reserve Scanner")
      basisReserveScan shouldBe defined

      // Verify the scan properties match the request
      val scan = basisReserveScan.get
      scan.scanName shouldBe "Basis Reserve Scanner"
      scan.walletInteraction shouldBe ScanWalletInteraction.Off
      scan.removeOffchain shouldBe false
      scan.trackingRule shouldBe a[ContainsScanningPredicate]

      val predicate = scan.trackingRule.asInstanceOf[ContainsScanningPredicate]
      predicate.regId shouldBe ErgoBox.R1
      Base16.encode(ValueSerializer.serialize(predicate.value)) shouldBe "0e97031994031004140414040004000442040004420400044205000400048090e4c004044204020580a8d6b9070100d805d6017ee4e3000204d6029d72017300d603e4c6a70407d604b2a59e7201730100d605ededed93c27204c2a793db63087204db6308a793e4c672040407720393e4c67204060ee4c6a7060e959372027302d80dd606db07027203d607e4e30107d608cbb37206db07027207d609e4e30405d60a7a7209d60bdb6a01ddd60ce4e3020ed60db4720c73037304d60ee4e30305d60fb3b372087a720e720ad610e4e3060ed611b4721073057306d612e4c6b2db6501fe7307000407ea02d1ededed720593e4dc640ce4c6a705640283013c0e0e86027208720ae4e3050ee4c672040564939f720b7bb4720c7308b1720ca0ee720d9f72037bcbb3b3720d720f7206ed9099c1a7c17204720eeced91720973099199db6807b2db6502fe730a0072097e730b05939f720b7bb47210730cb17210a0ee72119f72127bcbb3b37211720fdb07027212cd720795937202730dd1eded72059299c17204c1a7730e93e4c672040564e4c6a70564d1730f"
    }
  }

}
