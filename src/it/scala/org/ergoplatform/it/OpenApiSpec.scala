package org.ergoplatform.it

import java.io.{File, PrintWriter}

import com.spotify.docker.client.messages.ContainerInfo
import com.typesafe.config.Config
import org.ergoplatform.it.container.{ApiChecker, ApiCheckerConfig, IntegrationSuite, Node}
import org.scalatest.{FreeSpec, TryValues}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source

class OpenApiSpec extends FreeSpec with IntegrationSuite with TryValues {

  val expectedHeight: Int = 2
  val specFilePath: String = "/tmp/openapi.yaml"
  val paramsFilePath: String = "/tmp/parameters.yaml"
  val paramsTemplatePath: String = "src/it/resources/parameters-template.txt"

  val offlineGeneratingPeer: Config = offlineGeneratingPeerConfig.withFallback(nodeSeedConfigs.head)

  val node: Node = docker.startNode(offlineGeneratingPeer).success.value

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
          Map("blockHeight" -> expectedHeight.toString,
              "lastHeadersCount" -> expectedHeight.toString,
              "headerId" -> headerIds.head)
        )
        val checker: ApiChecker = docker.startOpenApiChecker(
          ApiCheckerConfig(s"${node.restAddress}:${node.nodeRestPort}", specFilePath, paramsFilePath)
        ).get

        val containerInfo: ContainerInfo = docker.inspectContainer(checker.containerId)

        docker.saveLogs(checker.containerId, "openapi-checker")

        containerInfo.state.exitCode shouldBe 0
      }

    Await.result(result, 2.minutes)
  }
}
