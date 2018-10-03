package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class FourNodeSyncSpec extends FreeSpec with IntegrationSuite {

  val blocksCount = 5

  val forkDepth: Int = blocksCount
  val minerConfig: Config = nodeSeedConfigs.head

  val nonGeneratingConfig: Config = nonGeneratingPeerConfig.withFallback(nodeSeedConfigs(1))
  val onlineGeneratingConfigs: List[Config] = nodeSeedConfigs.slice(2, 4).map(onlineGeneratingPeerConfig.withFallback)
  val nodeConfigs: List[Config] = minerConfig +: nonGeneratingConfig +: onlineGeneratingConfigs

  val nodes: List[Node] = docker.startNodes(nodeConfigs).success.value

  s"Generate $blocksCount blocks" in {
    val result = for {
      b <- Future.traverse(nodes)(_.height).map(_.max)
      _ <- Future.traverse(nodes)(_.waitForHeight(b + blocksCount))
      headers <- Future.traverse(nodes)(_.headerIdsByHeight(b + blocksCount - forkDepth))
    } yield {
      log.debug(s"Headers at height ${b + blocksCount - forkDepth}: ${headers.mkString(",")}")
      val headerIdsAtSameHeight = headers.flatten
      val sample = headerIdsAtSameHeight.head
      headerIdsAtSameHeight should contain only sample
    }
    Await.result(result, 10.minutes)
  }

}

