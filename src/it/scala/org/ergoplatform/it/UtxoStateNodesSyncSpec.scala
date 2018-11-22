package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class UtxoStateNodesSyncSpec extends FreeSpec with IntegrationSuite {

  val blocksQty = 5

  val forkDepth: Int = blocksQty
  val minerConfig: Config = nodeSeedConfigs.head

  val nonGeneratingConfig: Config = nonGeneratingPeerConfig.withFallback(nodeSeedConfigs(1))
  val onlineGeneratingConfigs: List[Config] = nodeSeedConfigs.slice(2, 4).map(onlineGeneratingPeerConfig.withFallback)
  val nodeConfigs: List[Config] = minerConfig +: nonGeneratingConfig +: onlineGeneratingConfigs

  val nodes: List[Node] = docker.startNodes(nodeConfigs).get

  s"Utxo state nodes synchronisation ($blocksQty blocks)" in {
    val result = for {
      initHeight <- Future.traverse(nodes)(_.height).map(_.max)
      reachedHeight <- Future.traverse(nodes)(_.waitForHeight(initHeight + blocksQty)).map(_.max)
      headers <- Future.traverse(nodes)(_.headerIdsByHeight(reachedHeight + blocksQty - forkDepth))
    } yield {
      log.debug(s"Headers at height ${reachedHeight + blocksQty - forkDepth}: ${headers.mkString(",")}")
      val headerIdsAtSameHeight = headers.map(_.head)
      val sample = headerIdsAtSameHeight.head
      headerIdsAtSameHeight should contain only sample
    }
    Await.result(result, 10.minutes)
  }

}

