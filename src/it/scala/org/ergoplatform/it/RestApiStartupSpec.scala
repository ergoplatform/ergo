package org.ergoplatform.it

import com.typesafe.config.{Config, ConfigFactory}
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class RestApiStartupSpec extends AnyFlatSpec with IntegrationSuite {

  private val noApiKeyConfig: Config = ConfigFactory.parseString(
    """
      |scorex.restApi.apiKeyHash = null
    """.stripMargin
  )

  private val nodeConfig: Config = noApiKeyConfig
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs.head)
    .withFallback(allowLocalConfig)

  private val node: Node =
    docker.startDevNetNode(nodeConfig, sequentialTopologyConfig).get

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
    }

    Await.result(result, 90.seconds)
  }
}
