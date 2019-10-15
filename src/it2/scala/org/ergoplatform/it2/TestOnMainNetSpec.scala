package org.ergoplatform.it2

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.{FreeSpec, OptionValues}

import scala.async.Async
import scala.concurrent.Await
import scala.concurrent.duration._

class TestOnMainNetSpec
  extends FreeSpec
    with IntegrationSuite
    with OptionValues {

  val nodeConfig: Config = nodeSeedConfigs.head.withFallback(nonGeneratingPeerConfig)
  // TODO: switch to mainnet
  val node: Node = docker.startTestNetNode(nodeConfig).get

  "Start a node on mainnet and wait for a full sync" in {

    val result = Async.async {
      // height is null until the bootstrap sync is complete
      Async.await(node.waitForHeight(1, 1.second))
      // TODO check the node's health
      docker.forceStopNode(node.containerId)
    }

    Await.result(result, 60.minutes)
  }

}
