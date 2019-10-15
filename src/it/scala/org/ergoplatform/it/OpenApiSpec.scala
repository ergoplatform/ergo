package org.ergoplatform.it

import java.io.{File, PrintWriter}

import com.typesafe.config.Config
import org.ergoplatform.it.container.{ApiChecker, ApiCheckerConfig, IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source

class OpenApiSpec extends FreeSpec with IntegrationSuite {

  val expectedHeight: Int = 2
  val paramsFilePath: String = "/tmp/parameters.yaml"
  val paramsTemplatePath: String = "src/it/resources/parameters-template.txt"

  val offlineGeneratingPeer: Config = offlineGeneratingPeerConfig.withFallback(nodeSeedConfigs.head)

  val node: Node = docker.startDevNetNode(offlineGeneratingPeer).get

  def renderTemplate(template: String, varMapping: Map[String, String]): String = varMapping
    .foldLeft(template) { case (s, (k, v)) => s.replaceAll(s"@$k", v) }

  def createParamsFile(params: Map[String, String]): Unit = {
    val template: String = Source.fromFile(paramsTemplatePath).getLines.map(_ + "\n").mkString
    val writer: PrintWriter = new PrintWriter(new File(paramsFilePath))
    writer.write(renderTemplate(template, params))
    writer.close()
  }

  "OpenApi specification check" in {
    val result: Future[Unit] = node.waitForHeight(expectedHeight)
      .flatMap { _ => node.headerIdsByHeight(expectedHeight) }
      .map { headerIds =>
        createParamsFile(
          Map(
            "blockHeight" -> expectedHeight.toString,
            "lastHeadersCount" -> expectedHeight.toString,
            "headerId" -> headerIds.head)
        )

        val apiAddressToCheck: String = s"${node.nodeInfo.networkIpAddress}:${node.nodeInfo.containerApiPort}"
        val specFilePath: String = new File("src/main/resources/api/openapi.yaml").getAbsolutePath
        val checker: ApiChecker = docker.startOpenApiChecker(
          ApiCheckerConfig(apiAddressToCheck, specFilePath, paramsFilePath)
        ).get

        docker.waitContainer(checker.containerId).statusCode shouldBe 0
      }

    Await.result(result, 2.minutes)
  }
}
