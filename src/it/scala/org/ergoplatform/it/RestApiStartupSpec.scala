package org.ergoplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.concurrent.Await
import scala.concurrent.duration._

class RestApiStartupSpec extends AnyFlatSpec with IntegrationSuite {

  private val noKeyTemplate = new File("src/it/resources/devnetNoApiKeyTemplate.conf").getCanonicalFile
  require(noKeyTemplate.isFile)
  require(ConfigFactory.parseFile(noKeyTemplate).getIsNull("scorex.restApi.apiKeyHash"))

  private val noApiKeyConfig: Config = ConfigFactory.parseString("scorex.restApi.apiKeyHash = null")

  private val nodeConfig: Config = noApiKeyConfig
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs.head)
    .withFallback(allowLocalConfig)
  require(nodeConfig.getIsNull("scorex.restApi.apiKeyHash"))

  private val node: Node =
    docker.startDevNetNodeWithoutApiKey(nodeConfig, sequentialTopologyConfig,
      noKeyTemplate.getAbsolutePath -> "/opt/ergo/devnetTemplate.conf").get

  it should "start with no REST API key while rejecting protected wallet status" in {
    val result = for {
      started <- node.waitForStartup
      info <- started.singleGet("/info")
      missingKey <- started.singleGet("/wallet/status")
      dummyKey <- started.singleGet("/wallet/status", _.setHeader("api_key", "dummy"))
    } yield {
      info.getStatusCode shouldBe 200
      missingKey.getStatusCode shouldBe 403
      dummyKey.getStatusCode shouldBe 403
      docker.containerLogContains(started.containerId, "-Dscorex.restApi.apiKeyHash=") shouldBe false
      docker.containerLogContains(started.containerId,
        "scorex.restApi.apiKeyHash is not configured. Protected REST routes are disabled.") shouldBe true
    }

    Await.result(result, 90.seconds)
  }
}
