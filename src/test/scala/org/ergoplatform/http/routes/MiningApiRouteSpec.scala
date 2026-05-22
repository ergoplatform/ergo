package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import io.circe.syntax._
import org.bouncycastle.util.encoders.Hex
import org.ergoplatform.http.api.MiningApiRoute
import org.ergoplatform.mining.AutolykosSolution
import org.ergoplatform.modifiers.history.header.HeaderWithoutPow
import org.ergoplatform.settings.{Algos, ErgoSettings}
import org.ergoplatform.utils.Stubs
import org.ergoplatform.utils.generators.ErgoCoreGenerators.genECPoint
import org.ergoplatform.{ErgoTreePredef, Pay2SAddress}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.bytesToId

import scala.util.Try

class MiningApiRouteSpec
  extends AnyFlatSpec
    with Matchers
    with ScalatestRouteTest
    with Stubs
    with FailFastCirceSupport {

  import org.ergoplatform.utils.ErgoNodeTestConstants._
  import org.ergoplatform.utils.generators.ErgoCoreGenerators._

  val prefix = "/mining"

  val localSetting: ErgoSettings = settings.copy(nodeSettings = settings.nodeSettings.copy(useExternalMiner = true))
  val route: Route = MiningApiRoute(minerRef, localSetting).route

  val solution = AutolykosSolution(genECPoint.sample.get, genECPoint.sample.get, Array.fill(32)(9: Byte), BigInt(0))

  it should "return requested candidate" in {
    Get(prefix + "/candidate") ~> route ~> check {
      status shouldBe StatusCodes.OK
      Try(responseAs[Json]) shouldBe 'success
    }
  }

  it should "process external solution" in {
    Post(prefix + "/solution", solution.asJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "display miner pk" in {
    Get(prefix + "/rewardAddress") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val script = ErgoTreePredef.rewardOutputScript(settings.chainSettings.monetary.minerRewardDelay, pk)
      val addressStr = Pay2SAddress(script)(settings.addressEncoder).toString()
      responseAs[Json].hcursor.downField("rewardAddress").as[String] shouldEqual Right(addressStr)
    }
  }

  it should "return a self-consistent block template" in {
    Get(prefix + "/blockTemplate") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val json = responseAs[Json]
      val cur  = json.hcursor

      // top-level fields are all present
      cur.downField("header").focus       shouldBe defined
      cur.downField("transactions").focus shouldBe defined
      cur.downField("extension").focus    shouldBe defined
      cur.downField("adProofBytes").focus shouldBe defined
      cur.downField("pk").focus           shouldBe defined
      cur.downField("b").focus            shouldBe defined
      cur.downField("msg").focus          shouldBe defined

      // header roots are populated (CandidateBlock JSON wouldn't have these directly)
      val header = cur.downField("header")
      header.downField("transactionsRoot").as[String].toOption.get should have length 64
      header.downField("adProofsRoot").as[String].toOption.get    should have length 64
      header.downField("extensionRoot").as[String].toOption.get   should have length 64

      // reconstructing HeaderWithoutPow from the JSON and hashing must reproduce `msg`
      val reconstructed = HeaderWithoutPow(
        version          = header.downField("version").as[Byte].toOption.get,
        parentId         = bytesToId(Algos.decode(header.downField("parentId").as[String].toOption.get).get),
        ADProofsRoot     = Digest32 @@ Algos.decode(header.downField("adProofsRoot").as[String].toOption.get).get,
        stateRoot        = ADDigest @@ Algos.decode(header.downField("stateRoot").as[String].toOption.get).get,
        transactionsRoot = Digest32 @@ Algos.decode(header.downField("transactionsRoot").as[String].toOption.get).get,
        timestamp        = header.downField("timestamp").as[Long].toOption.get,
        nBits            = header.downField("nBits").as[Long].toOption.get,
        height           = header.downField("height").as[Int].toOption.get,
        extensionRoot    = Digest32 @@ Algos.decode(header.downField("extensionRoot").as[String].toOption.get).get,
        votes            = Algos.decode(header.downField("votes").as[String].toOption.get).get,
        unparsedBytes    = Algos.decode(header.downField("unparsedBytes").as[String].toOption.get).get
      )
      val expectedMsg = settings.chainSettings.powScheme.msgByHeader(reconstructed)
      val returnedMsg = Algos.decode(cur.downField("msg").as[String].toOption.get).get
      Hex.toHexString(returnedMsg) shouldEqual Hex.toHexString(expectedMsg)
    }
  }

}
