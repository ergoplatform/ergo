package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport
import io.circe.Json
import org.ergoplatform.{ErgoAddressEncoder, Pay2SAddress, Pay2SHAddress}
import org.ergoplatform.settings.{Args, ErgoSettings, ErgoSettingsReader}
import org.ergoplatform.utils.Stubs
import io.circe.syntax._
import org.ergoplatform.http.api.ScriptApiRoute
import org.ergoplatform.settings.Constants.TrueTree
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.encode.Base16
import sigma.ast.SByte
import sigma.ast.syntax.CollectionConstant
import sigma.serialization.{ErgoTreeSerializer, ValueSerializer}

class ScriptApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs
  with FailFastCirceSupport {

  import org.ergoplatform.utils.ErgoNodeTestConstants._
  implicit val addressEncoder: ErgoAddressEncoder = settings.addressEncoder

  val prefix = "/script"

  val ergoSettings: ErgoSettings = ErgoSettingsReader.read(
    Args(userConfigPathOpt = Some("src/test/resources/application.conf"), networkTypeOpt = None))
  val route: Route = ScriptApiRoute(digestReadersRef, settings).route

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

  it should "execute script with context" in {
    val suffix = "/executeWithContext"
    val stream = ClassLoader.getSystemClassLoader.getResourceAsStream("execute-script.json")
    val req = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val assertion = (json: Json) => {
      status shouldBe StatusCodes.OK
      val value = json.hcursor.downField("value").downField("op").as[Int].right.get
      val condition = json.hcursor.downField("value").downField("condition").as[Boolean].right.get
      val cost = json.hcursor.downField("cost").as[Int].right.get
      value shouldEqual -45
      condition shouldEqual true
      cost shouldEqual 6
    }
    val json = io.circe.parser.parse(req)
    Post(prefix + suffix, json) ~> route ~> check(assertion(responseAs[Json]))
  }

  it should "generate valid P2SAddress form source" in {
    val suffix = "/p2sAddress"
    val assertion = (json: Json) => {
      status shouldBe StatusCodes.OK
      val addressStr = json.hcursor.downField("address").as[String].right.get
      addressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SAddress.addressTypePrefix
    }
    Post(prefix + suffix, Json.obj("source" -> scriptSource.asJson, "treeVersion" -> 0.asJson)) ~> route ~> check(assertion(responseAs[Json]))
    Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson, "treeVersion" -> 0.asJson)) ~> route ~>
      check(assertion(responseAs[Json]))
  }

  it should "generate valid P2SHAddress form source" in {
    val suffix = "/p2shAddress"
    val assertion = (json: Json) => {
      status shouldBe StatusCodes.OK
      val addressStr = json.hcursor.downField("address").as[String].right.get
      addressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SHAddress.addressTypePrefix
    }
    Post(prefix + suffix, Json.obj("source" -> scriptSource.asJson, "treeVersion" -> 0.asJson)) ~> route ~> check(assertion(responseAs[Json]))
    Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson, "treeVersion" -> 0.asJson)) ~> route ~>
      check(assertion(responseAs[Json]))
  }

  it should "get through address <-> ergoTree round-trip" in {
    val suffix = "addressToTree"

    val assertion = (json: Json, address: String) => {
      status shouldBe StatusCodes.OK
      val treeStr = json.hcursor.downField("tree").as[String].right.get

      val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(Base16.decode(treeStr).get)

      val addr = addressEncoder.fromProposition(tree).get

      addressEncoder.toString(addr) shouldBe address
    }

    val p2pk = "3WvsT2Gm4EpsM9Pg18PdY6XyhNNMqXDsvJTbbf6ihLvAmSb7u5RN"
    Get(s"$prefix/$suffix/$p2pk") ~> route ~> check(assertion(responseAs[Json], p2pk))

    val tree = TrueTree

    val p2sh = Pay2SHAddress.apply(tree).toString()
    p2sh shouldBe "rbcrmKEYduUvADj9Ts3dSVSG27h54pgrq5fPuwB"
    Get(s"$prefix/$suffix/$p2sh") ~> route ~> check(assertion(responseAs[Json], p2sh))

    val p2s = addressEncoder.toString(addressEncoder.fromProposition(tree).get)
    p2s shouldBe "Ms7smJwLGbUAjuWQ"
    Get(s"$prefix/$suffix/$p2s") ~> route ~> check(assertion(responseAs[Json], p2s))
  }

  it should "address <-> bytes roundtrip via addressToBytes" in {
    val suffix = "addressToBytes"

    val assertion = (json: Json, address: String) => {
      status shouldBe StatusCodes.OK
      val vs = json.hcursor.downField("bytes").as[String].right.get
      val vbs = Base16.decode(vs).get

      val bac = ValueSerializer.deserialize(vbs).asInstanceOf[CollectionConstant[SByte.type]]

      val bs = bac.value.toArray.map(b => b.byteValue())

      val tree = ErgoTreeSerializer.DefaultSerializer.deserializeErgoTree(bs)

      val addr = addressEncoder.fromProposition(tree).get

      addressEncoder.toString(addr) shouldBe address
    }

    val p2pk = "3WvsT2Gm4EpsM9Pg18PdY6XyhNNMqXDsvJTbbf6ihLvAmSb7u5RN"
    Get(s"$prefix/$suffix/$p2pk") ~> route ~> check(assertion(responseAs[Json], p2pk))

    val p2sh = "rbcrmKEYduUvADj9Ts3dSVSG27h54pgrq5fPuwB"
    Get(s"$prefix/$suffix/$p2sh") ~> route ~> check(assertion(responseAs[Json], p2sh))

    val tree = TrueTree
    val p2s = addressEncoder.toString(addressEncoder.fromProposition(tree).get)
    p2s shouldBe "Ms7smJwLGbUAjuWQ"
    Get(s"$prefix/$suffix/$p2s") ~> route ~> check(assertion(responseAs[Json], p2s))
  }

}
