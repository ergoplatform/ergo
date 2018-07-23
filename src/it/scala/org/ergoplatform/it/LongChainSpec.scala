package org.ergoplatform.it

import org.ergoplatform.it.container.IntegrationSuite
import org.scalatest.FreeSpec

import scala.async.Async.{async, await}
import scala.concurrent.Await
import scala.concurrent.duration._

class LongChainSpec extends FreeSpec with IntegrationSuite {

  s"Synchronize long chain" ignore {
    val minerConfig = noDelayConfig.withFallback(nodeSeedConfigs.head)
    val followerConfig = nonGeneratingPeerConfig.withFallback(nodeSeedConfigs(1))
    val miner = docker.startNode(minerConfig).success.value
    val blocksCount = 500
    val check = async {
      await(miner.waitForHeight(blocksCount))
      val follower = docker.startNode(followerConfig).success.value
      await(follower.waitForHeight(blocksCount))
      succeed
    }
    Await.result(check, 15.minutes)
  }
}

