package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class DigestStateNodeSyncSpec extends FreeSpec with IntegrationSuite {

  val blocksQty = 10

  val minerConfig: Config = nodeSeedConfigs.head
  val digestConfig: Config = digestStatePeerConfig
    .withFallback(prunedHistoryPeerConfig(blocksQty / 2))
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs(1))
  val onlineGeneratingConfigs: List[Config] = nodeSeedConfigs.slice(2, 4).map(onlineGeneratingPeerConfig.withFallback)
  val nodeConfigs: List[Config] = minerConfig +: digestConfig +: onlineGeneratingConfigs

  val nodes: List[Node] = docker.startNodes(nodeConfigs).get

  s"Digest mode synchronization ($blocksQty blocks)" in {
    val result = for {
      initHeight <- Future.traverse(nodes)(_.height).map(_.max)
      _ <- Future.traverse(nodes)(_.waitForHeight(initHeight + blocksQty))
      headers <- Future.traverse(nodes)(_.headerIdsByHeight(initHeight + blocksQty))
    } yield {
      val headerIdsAtSameHeight = headers.map(_.head)
      val sample = headerIdsAtSameHeight.head
      headerIdsAtSameHeight should contain only sample
    }
    Await.result(result, 10.minutes)
  }

}
