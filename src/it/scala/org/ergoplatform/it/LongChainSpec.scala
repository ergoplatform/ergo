package org.ergoplatform.it

import org.ergoplatform.it.container.IntegrationSuite
import org.scalatest.FreeSpec

import scala.async.Async.{async, await}
import scala.concurrent.Await
import scala.concurrent.duration._

class LongChainSpec extends FreeSpec with IntegrationSuite {

    s"Synchronize long blocks" ignore {
    val minerConfig = noDelayConfig.withFallback(nodeSeedConfigs.head)
    val followerConfig = nonGeneratingPeerConfig.withFallback(nodeSeedConfigs(1))
    val miner = docker.startNode(minerConfig).get
    val blocksCount = 300
      val check = async {
        await(miner.waitForHeight(blocksCount))
        val follower = docker.startNode(followerConfig).get
        await(follower.waitForHeight(blocksCount))
      succeed
    }
      Await.result(check, 15.minutes)
  }
}

