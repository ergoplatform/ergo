package org.ergoplatform.mining.llm_generated

import io.circe.Json
import org.ergoplatform.AutolykosSolution.pkForV2
import org.ergoplatform.mining.{WeakAutolykosSolution, groupElemToBytes}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sigma.crypto.CryptoConstants

class WeakAutolykosSolutionCodecsSpec extends AnyFlatSpec with Matchers {
  private val publicKeyHex = "0279be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
  private val nonce = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7)
  private val nonceJson = Json.fromString("0001020304050607")

  it should "preserve the exact public key and nonce JSON fields" in {
    val solution = WeakAutolykosSolution(CryptoConstants.dlogGroup.generator, nonce)
    val expected = Json.obj("pk" -> Json.fromString(publicKeyHex), "n" -> nonceJson)

    WeakAutolykosSolution.jsonEncoder(solution) shouldBe expected
    val decoded = WeakAutolykosSolution.jsonDecoder.decodeJson(expected).right.get
    decoded.encodedPk.toSeq shouldBe solution.encodedPk.toSeq
    decoded.n.toSeq shouldBe nonce.toSeq
  }

  it should "retain the default public key when pk is absent" in {
    val decoded = WeakAutolykosSolution.jsonDecoder.decodeJson(Json.obj("n" -> nonceJson)).right.get

    decoded.encodedPk.toSeq shouldBe groupElemToBytes(pkForV2).toSeq
    decoded.n.toSeq shouldBe nonce.toSeq
  }

  it should "reject a missing or malformed nonce" in {
    Seq(
      Json.obj("pk" -> Json.fromString(publicKeyHex)),
      Json.obj("n" -> Json.fromString("not-hex")),
      Json.obj("n" -> Json.fromInt(1))
    ).foreach { json =>
      WeakAutolykosSolution.jsonDecoder.decodeJson(json).isLeft shouldBe true
    }
  }
}
