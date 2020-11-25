package org.ergoplatform.it

import java.io.File

import akka.japi.Option.Some
import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.OptionValues
import org.scalatest.flatspec.AnyFlatSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class NodeRecoverySpec
  extends AnyFlatSpec
    with IntegrationSuite
    with OptionValues {

  val shutdownAtHeight: Int = 5

  val localVolume = s"$localDataDir/node-recovery-spec/data"
  val remoteVolume = "/app"

  val dir = new File(localVolume)
  dir.mkdirs()

  val offlineGeneratingPeer: Config = specialDataDirConfig(remoteVolume)
    .withFallback(offlineGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs.head)

  val node: Node = docker.startDevNetNode(offlineGeneratingPeer, specialVolumeOpt = Some((localVolume, remoteVolume))).get

  //  Testing scenario:
  // 1. Start up one node and let it mine {shutdownAtHeight} blocks;
  // 2. Shut it down unexpectedly and then restart;
  // 3. Check that node's state is consistent;
  it should "Node recovery after unexpected shutdown" in {

    val result = node.waitForHeight(shutdownAtHeight)
      .flatMap(_ => node.headerIdsByHeight(shutdownAtHeight))
      .flatMap { ids =>
        docker.forceStopNode(node.containerId)
        val restartedNode = docker
          .startDevNetNode(offlineGeneratingPeer, specialVolumeOpt = Some((localVolume, remoteVolume))).get
        restartedNode.waitForHeight(shutdownAtHeight)
          .flatMap(_ => restartedNode.headerIdsByHeight(shutdownAtHeight))
          .map(_.headOption.value shouldEqual ids.headOption.value)
      }

    Await.result(result, 4.minutes)
  }

}
