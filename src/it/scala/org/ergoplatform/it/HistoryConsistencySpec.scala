package org.ergoplatform.it

import java.io.File

import akka.japi.Option.Some
import com.typesafe.config.Config
import org.ergoplatform.it.api.NodeApi.HistoryInfo
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.Await
import scala.concurrent.duration._

class HistoryConsistencySpec extends FreeSpec with IntegrationSuite {

  val shutdownAtHeight: Int = 5

  val localVolume = "/tmp/ergo/history-consistency-spec/data"
  val remoteVolume = "/app"

  val dir = new File(localVolume)

  dir.mkdirs()
  dir.deleteOnExit()

  val offlineGeneratingPeer: Config = specialDataDirConfig(remoteVolume)
    .withFallback(shortDelayConfig)
    .withFallback(offlineGeneratingPeerConfig)
    .withFallback(nodeSeedConfigs.head)

  val node: Node = docker.startNode(offlineGeneratingPeer, specialVolumeOpt = Some((localVolume, remoteVolume))).get

  "History consistency after unexpected shutdown" in {

    val result = node.waitForHeight(shutdownAtHeight, 100.millis)
      .flatMap { _ =>
        node.waitFor[HistoryInfo](_.historyInfo, hi => hi.bestHeaderHeight > hi.bestBlockHeight, 100.millis)
      }
      .flatMap { _ =>
        docker.forceStopNode(node.containerId)
        val restartedNode = docker
          .startNode(offlineGeneratingPeer, specialVolumeOpt = Some((localVolume, remoteVolume))).get
        restartedNode.waitForHeight(shutdownAtHeight, 100.millis)
          .flatMap(_ => restartedNode.historyInfo)
          .map(hi => hi.bestHeaderId shouldEqual hi.bestBlockId)
      }

    Await.result(result, 4.minutes)
  }
}
