package org.ergoplatform.it

import akka.japi.Option.Some
import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class NodeRecoverySpec extends FreeSpec with IntegrationSuite {

  val shutdownAtHeight: Int = 5

  val localVolume = "/tmp/ergo/data"
  val remoteVolume = "/app"

  val offlineGeneratingPeer: Config = specialDataDirConfig(remoteVolume)
    .withFallback(offlineGeneratingPeerConfig.withFallback(nodeSeedConfigs.head))

  val nonGeneratingPeer: Config = specialDataDirConfig(remoteVolume)
    .withFallback(nonGeneratingPeerConfig.withFallback(nodeSeedConfigs.head))

  val node: Node = docker.startNode(offlineGeneratingPeer, specialVolumeOpt = Some((localVolume, remoteVolume))).get

  "Node recovery after unexpected shutdown" in {

    val result = node.waitForHeight(shutdownAtHeight)
      .flatMap { _ =>
        docker.forceStopNode(node.containerId)
        val restartedNode = docker
          .startNode(nonGeneratingPeer, specialVolumeOpt = Some((localVolume, remoteVolume))).get
        restartedNode.waitForHeight(shutdownAtHeight)
      }

    Await.result(result, 5.minutes)
  }
}
