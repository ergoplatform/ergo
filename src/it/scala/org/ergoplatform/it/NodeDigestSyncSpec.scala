package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class NodeDigestSyncSpec extends FreeSpec with IntegrationSuite {

  val blocksQty = 5

  val minerConfig: Config = nodeSeedConfigs.head

  val digestStateNodeConfig: Config = nonGeneratingPeerConfig.withFallback(nodeSeedConfigs(1))
  val onlineGeneratingConfigs: List[Config] = nodeSeedConfigs.slice(2, 4).map(onlineGeneratingPeerConfig.withFallback)
  val nodeConfigs: List[Config] = minerConfig +: onlineGeneratingConfigs :+ digestStateNodeConfig

  val nodes: List[Node] = docker.startNodes(nodeConfigs).get

  s"Node with digest state sync ($blocksQty blocks)" in {
    val result = for {
      initHeight <- Future.traverse(nodes)(_.height).map(_.max)
      _ <- Future.traverse(nodes)(_.waitForHeight(initHeight + blocksQty))
      otherHeaders <- Future.traverse(nodes.init)(_.headerIdsByHeight(initHeight + blocksQty))
      digestNodeHeaders <- nodes.last.headerIdsByHeight(initHeight + blocksQty)
    } yield {
      val headerIdsAtSameHeight = otherHeaders.flatten
      val sample = headerIdsAtSameHeight.head
      headerIdsAtSameHeight should contain only sample
      sample shouldEqual digestNodeHeaders.head
    }
    Await.result(result, 10.minutes)
  }

}
