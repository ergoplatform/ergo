package org.ergoplatform.it

import java.io.File

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.{FreeSpec, OptionValues}

import scala.concurrent.Await
import scala.concurrent.duration._

class HistoryConsistencySpec
  extends FreeSpec
    with IntegrationSuite
    with OptionValues {

  val shutdownAtHeight: Int = 5

  val localVolume = s"$localDataDir/history-consistency-spec/data"
  val remoteVolume = "/app"

  val dir = new File(localVolume)
  dir.mkdirs()

  val nodeConfig: Config = specialDataDirConfig(remoteVolume)
    .withFallback(shortDelayConfig)
    .withFallback(offlineGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs.head)

  val node: Node = docker.startNode(nodeConfig, specialVolumeOpt = Some((localVolume, remoteVolume))).get

  "History consistency after unexpected shutdown" in {

    val result = node.waitForHeight(shutdownAtHeight, 100.millis)
      .flatMap(_ => node.waitForInconsistentHistory)
      .flatMap { _ =>
        docker.forceStopNode(node.containerId)
        val restartedNode = docker
          .startNode(nodeConfig, specialVolumeOpt = Some((localVolume, remoteVolume))).get
        restartedNode.historyInfo
          .flatMap(hi => restartedNode.waitForHeight(hi.bestBlockHeight + 1, 100.millis))
          .flatMap(_ => restartedNode.historyInfo)
          .flatMap { hi =>
            restartedNode.headerIdsByHeight(hi.bestBlockHeight)
              .map(_.headOption.value shouldEqual hi.bestBlockId)
          }
      }

    Await.result(result, 4.minutes)
  }
}
