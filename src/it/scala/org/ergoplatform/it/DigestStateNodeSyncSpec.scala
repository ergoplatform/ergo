package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.async.Async.{async, await}
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class DigestStateNodeSyncSpec extends FreeSpec with IntegrationSuite {

  val blocksQty = 10
  val syncDelta = 5

  val minerConfig: Config = nodeSeedConfigs.head
  val digestConfig: Config = digestStatePeerConfig
    .withFallback(prunedHistoryPeerConfig(blocksQty / 2))
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs(1))
  val onlineGeneratingConfigs: List[Config] = nodeSeedConfigs.slice(2, 4).map(onlineGeneratingPeerConfig.withFallback)
  val nodeConfigs: List[Config] = minerConfig +: onlineGeneratingConfigs

  // Testing scenario:
  // 1. Start up few full nodes and let them mine {targetHeight + syncDelta} blocks;
  // 2. Fetch their headers at {targetHeight} and make sure their chains is synced (all nodes have same best headers);
  // 3. Fetch `stateRoot` of best header at {targetHeight};
  // 4. Start up digest node and wait until it gets synced with the network up to {targetHeight};
  // 5. Fetch its `stateRoot` of best header at {targetHeight} and make sure it matches full nodes' one;
  s"Digest mode synchronization ($blocksQty blocks)" in {

    val fullNodes: List[Node] = docker.startNodes(nodeConfigs).get

    val result = async {
      val initHeight = await(Future.traverse(fullNodes)(_.height).map(_.max))
      val targetHeight = initHeight + blocksQty
      await(Future.traverse(fullNodes)(_.waitForHeight(targetHeight + syncDelta)))
      val fullNodesHeaders = await(Future.traverse(fullNodes)(_.headerIdsByHeight(targetHeight))).map(_.head)
      fullNodesHeaders should contain only fullNodesHeaders.head
      val fullNodesStateRoot = await(fullNodes.head.headerById(fullNodesHeaders.head)).stateRoot
      val digestNode = docker.startNode(digestConfig).get
      await(digestNode.waitForHeight(targetHeight))
      val digestNodeHeader = await(digestNode.headerIdsByHeight(targetHeight)).head
      digestNodeHeader shouldEqual fullNodesHeaders.head
      val digestNodeStateRoot = await(digestNode.headerById(digestNodeHeader)).stateRoot
      java.util.Arrays.equals(fullNodesStateRoot, digestNodeStateRoot) shouldBe true
    }

    Await.result(result, 10.minutes)
  }

}
