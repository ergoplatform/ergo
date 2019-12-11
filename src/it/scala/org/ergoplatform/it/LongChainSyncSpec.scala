package org.ergoplatform.it

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class LongChainSyncSpec extends FreeSpec with IntegrationSuite {

  val chainLength = 300

  val minerConfig: Config = shortMiningDelayConfig.withFallback(nodeSeedConfigs.head)
  val nonGeneratingConfig: Config = nonGeneratingPeerConfig.withFallback(nodeSeedConfigs(1))

  val miner: Node = docker.startDevNetNode(minerConfig).get

  s"Long chain ($chainLength blocks) synchronization" in {
    val miner: Node = docker.startDevNetNode(minerConfig).get

    val result: Future[Int] = miner.waitForFullHeight(chainLength)
      .flatMap { _ =>
        val follower = docker.startDevNetNode(nonGeneratingConfig).get
        follower.waitForHeight(chainLength)
      }

    Await.result(result, 10.minutes)
  }
}

