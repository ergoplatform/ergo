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

  // TODO: get the current height (from explorer?)
  val targetHeight: Int = 80000

  val nodeConfig: Config = nodeSeedConfigs.head.withFallback(nonGeneratingPeerConfig)
  val node: Node = docker.startMainNetNode(nodeConfig).get

  "Start a node on mainnet and wait for a full sync" in {

    val result = Async.async {
      Async.await(node.waitForHeight(targetHeight, 1.second))
      // TODO check the node's health
      docker.forceStopNode(node.containerId)
    }

    Await.result(result, 4.minutes)
  }

}
