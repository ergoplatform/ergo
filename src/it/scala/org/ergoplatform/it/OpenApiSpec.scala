package org.ergoplatform.it

import java.io.{File, PrintWriter}

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.{FreeSpec, TryValues}

import scala.io.Source

class OpenApiSpec extends FreeSpec with IntegrationSuite with TryValues {

  val expectedHeight: Int = 3
  val paramsFilePath: String = "tmp/parameters.yaml"
  val paramsTemplatePath: String = "/src/it/resources/parameters-template.txt"

  val offlineGeneratingNode: Config = onlineGeneratingPeerConfig.withFallback(nodeSeedConfigs.head)

  val node: Node = docker.startNode(offlineGeneratingNode).success.value

  def renderTemplate(template: String, varMapping: Map[String, String]): String = varMapping
    .foldLeft(template) { case (s, (k, v)) => s.replaceAll(s"@$k", v) }

  "OpenApi specification check" in {

    val r = node.waitForHeight(expectedHeight)
      .flatMap { _ => node.headerIdsByHeight(expectedHeight) }
      .map { headerIds =>
        val template: String = Source.fromFile(paramsTemplatePath).getLines.mkString
        val writer: PrintWriter = new PrintWriter(new File(paramsFilePath))
        writer.write(renderTemplate(template,
          Map("blockHeight" -> expectedHeight.toString,
              "lastHeadersCount" -> expectedHeight.toString,
              "headerId" -> headerIds.head)
        ))
        writer.close()
      }
  }
}
