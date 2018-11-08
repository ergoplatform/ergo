package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.async.Async
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class PrunedFullNodeSyncSpec extends FreeSpec with IntegrationSuite {

  val blocksQty = 10
  val syncDelta = 5

  val minerConfig: Config = nodeSeedConfigs.head
  val prunedConfig: Config = prunedHistoryPeerConfig(blocksQty / 2)
    .withFallback(nonGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs(1))
  val onlineGeneratingConfigs: List[Config] = nodeSeedConfigs.slice(2, 4).map(onlineGeneratingPeerConfig.withFallback)
  val nodeConfigs: List[Config] = minerConfig +: onlineGeneratingConfigs

  // Testing scenario:
  // 1. Start up few full nodes and let them mine {targetHeight + syncDelta} blocks;
  // 2. Fetch their headers at {targetHeight} and make sure chains are synced (all nodes have same best headers);
  // 3. Fetch `stateRoot` of best header at {targetHeight};
  // 4. Start up pruned full node and wait until it gets synced with the network up to {targetHeight};
  // 5. Fetch its `stateRoot` of best header at {targetHeight} and make sure it matches full nodes' one;
  s"Pruned full node synchronization ($blocksQty blocks)" in {

    val fullNodes: List[Node] = docker.startNodes(nodeConfigs).get

    val result = Async.async {
      val initHeight = Async.await(Future.traverse(fullNodes)(_.height).map(_.max))
      val targetHeight = initHeight + blocksQty
      Async.await(Future.traverse(fullNodes)(_.waitForHeight(targetHeight + syncDelta)))
      val fullNodesHeaders = Async.await(Future.traverse(fullNodes)(_.headerIdsByHeight(targetHeight))).map(_.head)
      fullNodesHeaders should contain only fullNodesHeaders.head
      val fullNodesStateRoot = Async.await(fullNodes.head.headerById(fullNodesHeaders.head)).stateRoot
      val prunedNode = docker.startNode(prunedConfig).get
      Async.await(prunedNode.waitForHeight(targetHeight))
      val prunedNodeHeader = Async.await(prunedNode.headerIdsByHeight(targetHeight)).head
      prunedNodeHeader shouldEqual fullNodesHeaders.head
      val prunedNodeStateRoot = Async.await(prunedNode.headerById(prunedNodeHeader)).stateRoot
      java.util.Arrays.equals(fullNodesStateRoot, prunedNodeStateRoot) shouldBe true
    }

    Await.result(result, 10.minutes)
  }

}
