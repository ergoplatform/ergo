package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class ForkResolutionSpec extends FreeSpec with IntegrationSuite {

  val initialCommonChainLength: Int = 4
  val forkLength: Int = 5
  val syncLength: Int = 5

  val minerConfig: Config = nodeSeedConfigs.head
  val onlineMiningNodesConfig: List[Config] = nodeSeedConfigs.slice(1, 4).map(_.withFallback(onlineGeneratingPeerConfig))
  val offlineMiningNodesConfig: List[Config] = nodeSeedConfigs.slice(1, 4)

  "Fork resolution after isolated mining" in {

    val nodes: List[Node] = docker.startNodes(minerConfig +: onlineMiningNodesConfig).get

    val result = for {
      b <- Future.traverse(nodes)(_.height).map(_.max)
      _ <- Future.traverse(nodes)(_.waitForHeight(b + initialCommonChainLength))
      isolatedNodes <- {
        nodes.foreach(node => docker.stopNode(node.containerId))
        Future.successful(docker.startNodes(minerConfig +: offlineMiningNodesConfig, isolatedPeersConfig).get)
      }
      _ <- Future.traverse(isolatedNodes)(_.waitForHeight(b + initialCommonChainLength + forkLength))
      regularNodes <- {
        isolatedNodes.foreach(node => docker.stopNode(node.containerId))
        Future.successful(docker.startNodes(minerConfig +: offlineMiningNodesConfig).get)
      }
      _ <- Future.traverse(regularNodes)(_.waitForHeight(b + initialCommonChainLength + forkLength + syncLength))
      headers <- Future.traverse(regularNodes)(_.headerIdsByHeight(b + initialCommonChainLength + forkLength))
    } yield {
      log.debug(s"Headers at height $b: ${headers.mkString(",")}")
      val headerIdsAtSameHeight = headers.flatten
      val sample = headerIdsAtSameHeight.head
      headerIdsAtSameHeight should contain only sample
    }
    Await.result(result, 4.minutes)
  }
}
