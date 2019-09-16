package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.async.Async
import scala.concurrent.Await
import scala.concurrent.duration._

class PoPowBootstrapSpec extends FreeSpec with IntegrationSuite {

  val targetHeight: Int = 200

  val minerConfig: Config = shortMiningDelayConfig
    .withFallback(nodeSeedConfigs(0))
  val lightNodeConfig: Config = digestStateNodeConfig
    .withFallback(poPowBootstrap)
    .withFallback(verifyTransactions(false))
    .withFallback(prunedHistoryConfig(blocksToKeep = 0))
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs(1))
  val lightFullNodeConfig: Config = digestStateNodeConfig
    .withFallback(poPowBootstrap)
    .withFallback(verifyTransactions(true))
    .withFallback(prunedHistoryConfig(blocksToKeep = 0))
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs(2))

  // Testing scenario:
  // 1. Start mining node and let it mine chain of length ~ {targetHeight};
  // 2. Start light node and wait until it gets synced up to {targetHeight};
  "PoPow bootstrapping" - {
    val minerNode: Node = docker.startNode(minerConfig).get

    "LightSpv mode" in {
      val result = Async.async {
        Async.await(minerNode.waitForFullHeight(targetHeight, 1.second))

        val lightNode = docker.startNode(lightNodeConfig).get

        Async.await(lightNode.waitFor[Int](_.headersHeight, _ >= targetHeight, 1.second))
      }

      Await.result(result, 10.minutes)
    }

//    "LightFullPoPow mode" in {
//      val result = Async.async {
//        Async.await(minerNode.waitForFullHeight(targetHeight, 1.second))
//
//        val fullNode = docker.startNode(lightFullNodeConfig).get
//
//        Async.await(fullNode.waitFor[Int](_.fullHeight, _ >= targetHeight, 1.second))
//      }
//
//      Await.result(result, 10.minutes)
//    }
  }

}
