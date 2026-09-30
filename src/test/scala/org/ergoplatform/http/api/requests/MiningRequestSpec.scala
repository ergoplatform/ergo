package org.ergoplatform.http.api.requests

import io.circe.Json
import io.circe.parser.decode
import io.circe.syntax._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MiningRequestSpec extends AnyFlatSpec with Matchers {

  "MiningRequest" should "decode valid JSON with empty transactions" in {
    val json = Json.obj(
      "txs" -> Json.arr(),
      "pk" -> Json.fromString("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
    )

    val result = decode[MiningRequest](json.noSpaces)

    result shouldBe 'right
    result.right.get.txs shouldBe empty
    result.right.get.pk shouldBe "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
  }

  it should "fail decoding when pk is missing" in {
    val json = Json.obj("txs" -> Json.arr())

    val result = decode[MiningRequest](json.noSpaces)

    result shouldBe 'left
  }

  it should "fail decoding when txs is missing" in {
    val json = Json.obj("pk" -> Json.fromString("0123456789abcdef"))

    val result = decode[MiningRequest](json.noSpaces)

    result shouldBe 'left
  }

  it should "fail decoding when both fields are missing" in {
    val json = Json.obj()

    val result = decode[MiningRequest](json.noSpaces)

    result shouldBe 'left
  }

  it should "fail decoding with invalid pk type" in {
    val json = Json.obj(
      "txs" -> Json.arr(),
      "pk" -> Json.fromInt(12345)
    )

    val result = decode[MiningRequest](json.noSpaces)

    result shouldBe 'left
  }

  it should "fail decoding with invalid txs type" in {
    val json = Json.obj(
      "txs" -> Json.fromString("not_an_array"),
      "pk" -> Json.fromString("0123456789abcdef")
    )

    val result = decode[MiningRequest](json.noSpaces)

    result shouldBe 'left
  }

  it should "encode to JSON correctly" in {
    val request = MiningRequest(Seq.empty, "abcdef0123456789")

    val json = request.asJson

    json.hcursor.downField("txs").as[Seq[Json]] shouldBe 'right
    json.hcursor.downField("pk").as[String] shouldBe Right("abcdef0123456789")
  }

  it should "preserve transaction order when encoding/decoding" in {
    // Use simple valid transaction JSON structure with all required fields
    val tx1 = Json.obj(
      "id" -> Json.fromString("tx1"),
      "inputs" -> Json.arr(),
      "dataInputs" -> Json.arr(),
      "outputCandidates" -> Json.arr(),
      "outputs" -> Json.arr()
    )
    val tx2 = Json.obj(
      "id" -> Json.fromString("tx2"),
      "inputs" -> Json.arr(),
      "dataInputs" -> Json.arr(),
      "outputCandidates" -> Json.arr(),
      "outputs" -> Json.arr()
    )
    val tx3 = Json.obj(
      "id" -> Json.fromString("tx3"),
      "inputs" -> Json.arr(),
      "dataInputs" -> Json.arr(),
      "outputCandidates" -> Json.arr(),
      "outputs" -> Json.arr()
    )

    val json = Json.obj(
      "txs" -> Json.arr(tx1, tx2, tx3),
      "pk" -> Json.fromString("fedcba9876543210")
    )
    val decoded = decode[MiningRequest](json.noSpaces)

    decoded shouldBe 'right
    val decodedRequest = decoded.right.get
    decodedRequest.txs should have size 3
    decodedRequest.pk shouldBe "fedcba9876543210"
  }

}
