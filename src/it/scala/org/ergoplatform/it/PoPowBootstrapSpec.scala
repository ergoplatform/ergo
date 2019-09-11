package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.async.Async
import scala.concurrent.Await
import scala.concurrent.duration._

class PoPowBootstrapSpec extends FreeSpec with IntegrationSuite {

  val targetHeight = 200

  val minerConfig: Config = shortMiningDelayConfig
    .withFallback(nodeSeedConfigs.head)
  val lightNodeConfig: Config = digestStateNodeConfig
    .withFallback(poPowBootstrap(true))
    .withFallback(verifyTransactions(false))
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs.last)

  // Testing scenario:
  // 1. Start mining node and let it mine chain of length ~ {targetHeight};
  // 2. Start light node and wait until it gets synced up to {targetHeight};
  "PoPow bootstrapping" in {
    val minerNode: Node = docker.startNode(minerConfig).get

    val result = Async.async {
      Async.await(minerNode.waitForFullHeight(targetHeight, 1.second))

      val lightNode = docker.startNode(lightNodeConfig).get

      Async.await(lightNode.waitFor[Int](_.headersHeight, _ >= targetHeight, 1.second))
    }

    Await.result(result, 10.minutes)
  }

}
