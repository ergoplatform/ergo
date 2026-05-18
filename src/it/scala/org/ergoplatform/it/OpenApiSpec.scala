package org.ergoplatform.it

import java.io.File

import com.atlassian.oai.validator.OpenApiInteractionValidator
import com.atlassian.oai.validator.model.{SimpleRequest, SimpleResponse}
import com.atlassian.oai.validator.report.ValidationReport
import com.typesafe.config.Config
import org.asynchttpclient.Response
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source

class OpenApiSpec extends AnyFlatSpec with IntegrationSuite {

  private val expectedHeight: Int = 2

  private val offlineGeneratingPeer: Config = offlineGeneratingPeerConfig
    .withFallback(nodeSeedConfigs.head)
    .withFallback(localOnlyConfig)

  private val node: Node = docker.startDevNetNode(offlineGeneratingPeer).get

  private val specPath: String =
    new File("src/main/resources/api/openapi.yaml").getAbsolutePath

  private val validator: OpenApiInteractionValidator =
    OpenApiInteractionValidator.createFor(specPath).build()

  private def loadResource(name: String): String = {
    val src = Source.fromInputStream(getClass.getResourceAsStream(name))
    try src.mkString
    finally src.close()
  }

  private val sampleBlock: String = loadResource("/openapi/sample-block.json")
  private val sampleTx: String    = loadResource("/openapi/sample-tx.json")

  private case class ApiCall(method: String, path: String, body: Option[String] = None)

  private def callApi(c: ApiCall): Future[(ApiCall, Response)] = {
    val responseFuture: Future[Response] = c.method match {
      case "GET"  => node.singleGet(c.path)
      case "POST" => node.post(c.path, c.body.getOrElse(""))
      case other  => Future.failed(new IllegalArgumentException(s"Unsupported method $other"))
    }
    responseFuture.map(c -> _)
  }

  private def toReport(c: ApiCall, resp: Response): ValidationReport = {
    val request = c.method match {
      case "GET" =>
        SimpleRequest.Builder.get(c.path).build()
      case "POST" =>
        val builder = SimpleRequest.Builder
          .post(c.path)
          .withHeader("Content-Type", "application/json")
        c.body.fold(builder)(builder.withBody).build()
    }
    val response = SimpleResponse.Builder
      .status(resp.getStatusCode)
      .withHeader(
        "Content-Type",
        Option(resp.getContentType).getOrElse("application/json")
      )
      .withBody(resp.getResponseBody)
      .build()
    validator.validate(request, response)
  }

  // Only public read-only endpoints + the POST shapes the previous checker exercised.
  // Wallet/mining/scan endpoints require a configured wallet and are out of scope here.
  private def buildCalls(headerId: String): Seq[ApiCall] = Seq(
    ApiCall("GET", "/info"),
    ApiCall("GET", "/peers/connected"),
    ApiCall("GET", "/peers/all"),
    ApiCall("GET", s"/blocks/lastHeaders/$expectedHeight"),
    ApiCall("GET", s"/blocks/at/$expectedHeight"),
    ApiCall("GET", s"/blocks/$headerId"),
    ApiCall("GET", s"/blocks/$headerId/header"),
    ApiCall("GET", s"/blocks/$headerId/transactions"),
    ApiCall("GET", "/utils/seed/123"),
    ApiCall("POST", "/blocks", Some(sampleBlock)),
    ApiCall("POST", "/transactions", Some(sampleTx)),
    ApiCall("POST", "/utils/hash/blake2b", Some("\"123qwe\"")),
    ApiCall("POST", "/utils/hash/blake2b", Some("\"\"")),
    ApiCall("POST", "/utils/hash/blake2b", Some("\"aaaaaaaaaa\""))
  )

  it should "OpenApi specification check" in {
    val result = node
      .waitForHeight(expectedHeight)
      .flatMap(_ => node.headerIdsByHeight(expectedHeight))
      .flatMap { headerIds =>
        Future.traverse(buildCalls(headerIds.head))(callApi)
      }
      .map { results =>
        val report = results.foldLeft(ValidationReport.empty()) {
          case (acc, (call, resp)) => acc.merge(toReport(call, resp))
        }
        val messages = report.getMessages.asScala
          .map(m => s"[${m.getLevel}] ${m.getKey}: ${m.getMessage}")
          .mkString("\n")
        withClue(s"OpenAPI conformance failures:\n$messages\n") {
          report.hasErrors shouldBe false
        }
      }

    Await.result(result, 2.minutes)
  }
}
