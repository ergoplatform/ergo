package org.ergoplatform.it

import java.io.File

import com.typesafe.config.Config
import org.ergoplatform.it.api.NodeApi.HistoryInfo
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class HistoryInconsistencySpec extends FreeSpec with IntegrationSuite {

  val shutdownAtHeight: Int = 40

  val localVolume = "/tmp/ergo2/history-inconsistency-spec/data"
  val remoteVolume = "/app"

  val dir = new File(localVolume)

  dir.mkdirs()

  val offlineGeneratingPeer: Config = specialDataDirConfig(remoteVolume)
    .withFallback(shortDelayConfig)
    .withFallback(offlineGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs.head)

  val node: Node = docker.startNode(offlineGeneratingPeer, specialVolumeOpt = Some((localVolume, remoteVolume))).get

  "History inconsistency after unexpected shutdown" in {

    val result = node.waitForHeight(shutdownAtHeight, 100.millis)
      .flatMap { _ =>
        node.waitFor[HistoryInfo](_.historyInfo, hi => hi.bestHeaderHeight > hi.bestBlockHeight, 50.millis)
      }
      .flatMap { _ =>
        docker.forceStopNode(node.containerId)
        val restartedNode = docker
          .startNode(offlineGeneratingPeer, specialVolumeOpt = Some((localVolume, remoteVolume))).get
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
