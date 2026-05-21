package org.ergoplatform.nodeView.wallet.requests

import io.circe.parser.decode
import io.circe.syntax._
import org.ergoplatform.Pay2SAddress
import org.ergoplatform.http.api.ApiCodecs
import org.ergoplatform.settings.Constants.FalseTree
import org.ergoplatform.utils.ErgoNodeTestConstants
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sigma.ast.{EvaluatedValue, IntConstant, SType}
import sigma.interpreter.ContextExtension

class RequestsHolderSpec extends AnyFlatSpec with Matchers with ApiCodecs {

  private val settings = ErgoNodeTestConstants.settings
  override implicit val ergoAddressEncoder = settings.addressEncoder
  private implicit val encoder: RequestsHolderEncoder = new RequestsHolderEncoder(settings)
  private implicit val decoder: RequestsHolderDecoder = new RequestsHolderDecoder(settings)

  private val paymentRequest = PaymentRequest(
    Pay2SAddress(FalseTree)(ergoAddressEncoder), 100L, Array.empty, Map.empty
  )

  private def ext(pairs: (Byte, EvaluatedValue[SType])*): ContextExtension =
    ContextExtension(pairs.toMap)

  "RequestsHolder" should "round-trip without context extensions" in {
    val holder = RequestsHolder(
      Seq(paymentRequest), Some(1000L),
      inputsRaw = Seq("1a2b"),
      dataInputsRaw = Seq.empty,
      extensions = Seq.empty,
      minerRewardDelay = 720
    )
    val decoded = decode[RequestsHolder](holder.asJson.noSpaces)
    decoded shouldBe 'right
    decoded.right.get.extensions shouldBe Seq.empty
    decoded.right.get.inputsRaw shouldBe Seq("1a2b")
  }

  it should "round-trip a context.extension array matching inputsRaw" in {
    val e0 = ext(1.toByte -> IntConstant(5).asInstanceOf[EvaluatedValue[SType]])
    val e1 = ext(
      2.toByte -> IntConstant(7).asInstanceOf[EvaluatedValue[SType]],
      3.toByte -> IntConstant(11).asInstanceOf[EvaluatedValue[SType]]
    )
    val holder = RequestsHolder(
      Seq(paymentRequest), Some(1000L),
      inputsRaw = Seq("1a2b", "2b3c"),
      dataInputsRaw = Seq.empty,
      extensions = Seq(e0, e1),
      minerRewardDelay = 720
    )

    val json = holder.asJson
    json.hcursor.downField("context").downField("extension").as[Seq[ContextExtension]] shouldBe Right(Seq(e0, e1))

    val decoded = decode[RequestsHolder](json.noSpaces)
    decoded shouldBe 'right
    decoded.right.get.extensions shouldBe Seq(e0, e1)
  }

  it should "reject a request where context.extension length != inputsRaw length" in {
    val bad =
      """{
        |  "requests": [],
        |  "fee": 1000,
        |  "inputsRaw": ["1a2b", "2b3c"],
        |  "context": { "extension": [ { "1": "0402" } ] }
        |}""".stripMargin
    decode[RequestsHolder](bad) shouldBe 'left
  }

  it should "accept a request that omits the context object entirely" in {
    val plain =
      """{
        |  "requests": [],
        |  "fee": 1000,
        |  "inputsRaw": ["1a2b"]
        |}""".stripMargin
    val decoded = decode[RequestsHolder](plain)
    decoded shouldBe 'right
    decoded.right.get.extensions shouldBe Seq.empty
  }
}
