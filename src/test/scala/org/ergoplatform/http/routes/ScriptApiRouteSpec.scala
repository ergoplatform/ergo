package org.ergoplatform.http.routes

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.RawHeader
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
import scorex.crypto.hash.Blake2b256
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

  private val apiKey = "test-api-key"
  private val apiKeyHeader = RawHeader("api_key", apiKey)
  private val wrongApiKeyHeader = RawHeader("api_key", "wrong-api-key")
  val settingsWithAuth: ErgoSettings = settings.copy(
    scorexSettings = settings.scorexSettings.copy(
      restApi = settings.scorexSettings.restApi.copy(
        apiKeyHash = Some(Base16.encode(Blake2b256(apiKey)))
      )
    )
  )
  val routeWithAuth: Route = ScriptApiRoute(digestReadersRef, settingsWithAuth).route
  private val sealedRouteWithAuth: Route = Route.seal(routeWithAuth)

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

  it should "reject script execution without an API key when auth is enabled" in {
    val stream = ClassLoader.getSystemClassLoader.getResourceAsStream("execute-script.json")
    val req = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    Post(prefix + "/executeWithContext", io.circe.parser.parse(req)) ~> sealedRouteWithAuth ~> check {
      status shouldBe StatusCodes.Forbidden
    }
  }

  it should "reject script execution with the wrong API key" in {
    val stream = ClassLoader.getSystemClassLoader.getResourceAsStream("execute-script.json")
    val req = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    Post(prefix + "/executeWithContext", io.circe.parser.parse(req)).withHeaders(wrongApiKeyHeader) ~>
      sealedRouteWithAuth ~> check {
        status shouldBe StatusCodes.Forbidden
      }
  }

  it should "execute script with context with the correct API key" in {
    val stream = ClassLoader.getSystemClassLoader.getResourceAsStream("execute-script.json")
    val req = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    Post(prefix + "/executeWithContext", io.circe.parser.parse(req)).withHeaders(apiKeyHeader) ~>
      sealedRouteWithAuth ~> check {
        status shouldBe StatusCodes.OK
        responseAs[Json].hcursor.downField("value").downField("op").as[Int] shouldBe Right(-45)
        responseAs[Json].hcursor.downField("value").downField("condition").as[Boolean] shouldBe Right(true)
        responseAs[Json].hcursor.downField("cost").as[Int] shouldBe Right(6)
      }
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

  it should "generate addresses with different tree versions" in {
    val p2sSuffix = "/p2sAddress"
    val p2shSuffix = "/p2shAddress"

    var p2sAddressV0: String = ""
    var p2shAddressV0: String = ""
    var p2sAddressV1: String = ""
    var p2shAddressV1: String = ""

    // Test with tree version 0
    Post(prefix + p2sSuffix, Json.obj("source" -> scriptSource.asJson, "treeVersion" -> 0.asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val addressStr = responseAs[Json].hcursor.downField("address").as[String].right.get
      addressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SAddress.addressTypePrefix
      p2sAddressV0 = addressStr
    }

    Post(prefix + p2shSuffix, Json.obj("source" -> scriptSource.asJson, "treeVersion" -> 0.asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val addressStr = responseAs[Json].hcursor.downField("address").as[String].right.get
      addressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SHAddress.addressTypePrefix
      p2shAddressV0 = addressStr
    }

    // Test with tree version 1
    Post(prefix + p2sSuffix, Json.obj("source" -> scriptSource.asJson, "treeVersion" -> 1.asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val addressStr = responseAs[Json].hcursor.downField("address").as[String].right.get
      addressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SAddress.addressTypePrefix
      p2sAddressV1 = addressStr
    }

    Post(prefix + p2shSuffix, Json.obj("source" -> scriptSource.asJson, "treeVersion" -> 1.asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val addressStr = responseAs[Json].hcursor.downField("address").as[String].right.get
      addressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SHAddress.addressTypePrefix
      p2shAddressV1 = addressStr
    }

    // Get the actual Ergo trees and verify they have different version bytes
    val p2sTreeV0 = addressEncoder.fromString(p2sAddressV0).get.script
    val p2sTreeV1 = addressEncoder.fromString(p2sAddressV1).get.script
    val p2shTreeV0 = addressEncoder.fromString(p2shAddressV0).get.script
    val p2shTreeV1 = addressEncoder.fromString(p2shAddressV1).get.script

    // Check that the trees have different version bytes
    p2sTreeV0.bytes should not equal p2sTreeV1.bytes
    p2shTreeV0.bytes shouldBe p2shTreeV1.bytes

    // Specifically check the version byte (first byte of ErgoTree)
    p2sTreeV0.bytes.head should not equal p2sTreeV1.bytes.head
    p2shTreeV0.bytes.head shouldBe p2shTreeV1.bytes.head

    // Verify the actual version bytes match what we requested
    p2sTreeV0.bytes.head shouldEqual 16
    p2sTreeV1.bytes.head shouldEqual 25
    p2shTreeV0.bytes.head shouldEqual 0
    p2shTreeV1.bytes.head shouldEqual 0
  }

  it should "handle tree version 2 for P2SH address" in {
    val suffix = "/p2shAddress"
    Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson, "treeVersion" -> 2.asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val addressStr = responseAs[Json].hcursor.downField("address").as[String].right.get
      addressEncoder.fromString(addressStr).get.addressTypePrefix shouldEqual Pay2SHAddress.addressTypePrefix

      // P2SH should always have version 0 regardless of treeVersion parameter
      val tree = addressEncoder.fromString(addressStr).get.script
      tree.bytes.head shouldEqual 0
    }
  }

  it should "generate consistent addresses for same script and version" in {
    val suffix = "/p2sAddress"

    Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson, "treeVersion" -> 1.asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val addressStr1 = responseAs[Json].hcursor.downField("address").as[String].right.get

      Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson, "treeVersion" -> 1.asJson)) ~> route ~> check {
        status shouldBe StatusCodes.OK
        val addressStr2 = responseAs[Json].hcursor.downField("address").as[String].right.get
        addressStr1 shouldEqual addressStr2
      }
    }
  }

  it should "generate different addresses for different tree versions" in {
    val suffix = "/p2sAddress"

    Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson, "treeVersion" -> 0.asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val addressStr0 = responseAs[Json].hcursor.downField("address").as[String].right.get

      Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson, "treeVersion" -> 1.asJson)) ~> route ~> check {
        status shouldBe StatusCodes.OK
        val addressStr1 = responseAs[Json].hcursor.downField("address").as[String].right.get
        addressStr0 should not equal addressStr1
      }
    }
  }

  it should "handle P2SH with tree version 1 (should still use version 0)" in {
    val suffix = "/p2shAddress"
    Post(prefix + suffix, Json.obj("source" -> scriptSourceSigProp.asJson, "treeVersion" -> 1.asJson)) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val addressStr = responseAs[Json].hcursor.downField("address").as[String].right.get
      val tree = addressEncoder.fromString(addressStr).get.script
      // P2SH always uses version 0
      tree.bytes.head shouldEqual 0
    }
  }

  it should "generate p2sAddress without api_key when auth is enabled" in {
    Post(prefix + "/p2sAddress", Json.obj("source" -> scriptSource.asJson, "treeVersion" -> 0.asJson)) ~> routeWithAuth ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "generate p2shAddress without api_key when auth is enabled" in {
    Post(prefix + "/p2shAddress", Json.obj("source" -> scriptSource.asJson, "treeVersion" -> 0.asJson)) ~> routeWithAuth ~> check {
      status shouldBe StatusCodes.OK
    }
  }

}
