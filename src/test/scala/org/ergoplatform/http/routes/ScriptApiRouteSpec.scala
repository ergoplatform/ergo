package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.{Pay2SAddress, Pay2SHAddress}
import org.ergoplatform.settings.{Args, ErgoSettings}
import org.ergoplatform.utils.Stubs
import org.scalatest.{FlatSpec, Matchers}
import io.circe.syntax._
import org.ergoplatform.http.api.ScriptApiRoute
import scorex.util.encode.Base16
import sigmastate.Values.{ErgoTree, TrueLeaf}
import sigmastate.serialization.ErgoTreeSerializer

class ScriptApiRouteSpec  extends FlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport {

  val prefix = "/script"

  val ergoSettings: ErgoSettings = ErgoSettings.read(
    Args(userConfigPathOpt = Some("src/test/resources/application.conf"), networkTypeOpt = None))
  val route: Route = ScriptApiRoute(readersRef, settings).route

  val scriptSource: String =
    """
      |{
      |    val myPk = PK("3WwUerNahQR1YXyq8AKi5UkKsYeJ99zxrqNqt3BCG4xSGeTERHiQ")
      |    HEIGHT < 9197 && myPk.isProven
      |}
      |""".stripMargin

  val scriptSourceSigProp: String =
    """
      |{
      |    PK("3WwUerNahQR1YXyq8AKi5UkKsYeJ99zxrqNqt3BCG4xSGeTERHiQ")
      |}
      |""".stripMargin


  it should "generate valid P2SAddress form source" in {
    val suffix = "/p2sAddress"
    val assertion = (json: Json) => {
      status shouldBe StatusCodes.OK
      val addressStr = json.hcursor.downField("address").as[String].right.get
      ergoAddressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SAddress.addressTypePrefix
    }
    Post(prefix + suffix, Json.obj("source" -> scriptSource.asJson)) ~> route ~> check(assertion(responseAs[Json]))
    Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson)) ~> route ~>
      check(assertion(responseAs[Json]))
  }

  //todo: temporarily switched off due to https://github.com/ergoplatform/ergo/issues/936
//  it should "generate valid P2SHAddress form source" in {
//    val suffix = "/p2shAddress"
//    val assertion = (json: Json) => {
//      status shouldBe StatusCodes.OK
//      val addressStr = json.hcursor.downField("address").as[String].right.get
//      ergoAddressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SHAddress.addressTypePrefix
//    }
//    Post(prefix + suffix, Json.obj("source" -> scriptSource.asJson)) ~> route ~> check(assertion(responseAs[Json]))
//    Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson)) ~> route ~>
//      check(assertion(responseAs[Json]))
//  }


  it should "get through address <-> ergoTree round-trip" in {
    val suffix = "addressToTree"

    val assertion = (json: Json, address: String) => {
      status shouldBe StatusCodes.OK
      val treeStr = json.hcursor.downField("tree").as[String].right.get

      val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(treeStr).get)

      val addr = ergoAddressEncoder.fromProposition(tree).get

      ergoAddressEncoder.toString(addr) shouldBe address
    }

    val p2pk = "3WvsT2Gm4EpsM9Pg18PdY6XyhNNMqXDsvJTbbf6ihLvAmSb7u5RN"
    Get(s"$prefix/$suffix/$p2pk") ~> route ~> check(assertion(responseAs[Json], p2pk))

    //todo: temporarily switched off due to https://github.com/ergoplatform/ergo/issues/936
//    val p2sh = "8UmyuJuQ3FS9ts7j72fn3fKChXSGzbL9WC"
//    Get(s"$prefix/$suffix/$p2sh") ~> route ~> check(assertion(responseAs[Json], p2sh))

    val script = TrueLeaf
    val tree = ErgoTree.fromProposition(script)
    val p2s = ergoAddressEncoder.toString(ergoAddressEncoder.fromProposition(tree).get)
    p2s shouldBe "Ms7smJwLGbUAjuWQ"
    Get(s"$prefix/$suffix/$p2s") ~> route ~> check(assertion(responseAs[Json], p2s))
  }
}
