package org.ergoplatform.it

import java.io.File

import com.atlassian.oai.validator.OpenApiInteractionValidator
import com.atlassian.oai.validator.model.{Request, SimpleRequest, SimpleResponse}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.JavaConverters._

/** Proves the swagger-request-validator catches the class of bug that openapi-checker
  * (per ergoplatform/ergo#555) silently passed: a response field declared as `string`
  * in the spec but returned as an object/array/number.
  *
  * Pure validator behavior — no node, no Docker.
  */
class OpenApiValidatorSelfTestSpec extends AnyFlatSpec with Matchers {

  private val specPath: String =
    new File("src/main/resources/api/openapi.yaml").getAbsolutePath

  private val validator: OpenApiInteractionValidator =
    OpenApiInteractionValidator.createFor(specPath).build()

  // Realistic header (matches BlockHeader schema). `id` is a base16 string per ModifierId.
  private val validHeaderBody: String =
    """{
      |  "id": "00d2a8d21113598ea924329f9520905693e914bac6235255b74fd3b8016171aa",
      |  "timestamp": 1538572701768,
      |  "version": 1,
      |  "adProofsRoot": "a84f62a669fb3684308ea609af6cd831b939b70210307e143a05321ce8efeda2",
      |  "stateRoot": "c2d3cfb7482c9edc4b1214b830032b93556af6a4a9224c7154cbf185bc00c15316",
      |  "transactionsRoot": "cc32add0ada11b6a81f07b61a6c606b2277af9999b453538d32b3c409630bce1",
      |  "nBits": 33628928,
      |  "extensionHash": "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8",
      |  "powSolutions": {
      |    "pk": "0350e25cee8562697d55275c96bb01b34228f9bd68fd9933f2a25ff195526864f5",
      |    "w": "032e3f5edb88f3cc7384bfedc892fc8dcb10d7a3bf3741d08a62cc701848d0932c",
      |    "n": "0000000000000000",
      |    "d": 1
      |  },
      |  "height": 32693,
      |  "difficulty": "291",
      |  "parentId": "002390f165396f855f53b928e469ba89a2107784479423a2db66b3acccef78e9",
      |  "votes": "000000",
      |  "size": 607
      |}""".stripMargin

  // Same header but with `id` as an object instead of a base16 string.
  // openapi-core 0.5.0 (the abandoned openapi-checker) silently passed this kind of mismatch.
  private val badHeaderBody: String = validHeaderBody.replace(
    """"id": "00d2a8d21113598ea924329f9520905693e914bac6235255b74fd3b8016171aa"""",
    """"id": {"oops": "this should be a string"}"""
  )

  private val headerPath = "/blocks/abc/header"

  private def request: Request = SimpleRequest.Builder.get(headerPath).build()

  private def response(body: String) = SimpleResponse.Builder
    .status(200)
    .withHeader("Content-Type", "application/json")
    .withBody(body)
    .build()

  it should "report no errors for a well-formed BlockHeader response" in {
    val report = validator.validate(request, response(validHeaderBody))
    withClue(
      report.getMessages.asScala
        .map(m => s"[${m.getLevel}] ${m.getKey}: ${m.getMessage}")
        .mkString("\n")
    ) {
      report.hasErrors shouldBe false
    }
  }

  it should "report an error when a string field is returned as an object (regression for ergo#555)" in {
    val report   = validator.validate(request, response(badHeaderBody))
    val messages = report.getMessages.asScala.map(_.getMessage).mkString("\n")
    report.hasErrors shouldBe true
    messages.toLowerCase should include("id")
  }
}
