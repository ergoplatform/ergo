package org.ergoplatform.it

import java.io.File

import com.typesafe.config.Config
import io.netty.util.{HashedWheelTimer, Timer}
import org.asynchttpclient.util.HttpConstants
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.ergoplatform.it.util._
import org.scalatest.{FreeSpec, OptionValues}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class HistoryConsistencySpec
  extends FreeSpec
    with IntegrationSuite
    with OptionValues {

  val timer: Timer = new HashedWheelTimer()

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

  /** Tries to catch the moment when full block is already persisted in history,
    * but indexes aren't updated yet. */
  def waitForInconsistentHistory: Future[Unit] = {
    timer.retryUntil[Boolean](historyIsInconsistent, bool => bool, 25.millis).map(_ => ())
  }

  /** To find out whether history is inconsistent at this moment we should:
    * 1. Fetch current `HistoryInfo`;
    * 2. Check whether `FullBlock` with `bestHeaderId` from `HistoryInfo` is already available;
    * 3. Check that `bestFullBlockId` isn't updated yet in `HistoryInfo`;
    * */
  def historyIsInconsistent: Future[Boolean] = {
    for {
      hiInit <- node.historyInfo
      fullBlockPersisted <- node.singleGet(s"/blocks/${hiInit.bestHeaderId}")
        .map(_.getStatusCode == HttpConstants.ResponseStatusCodes.OK_200)
      hi <- node.historyInfo
    } yield {
      fullBlockPersisted &&
        hi.bestHeaderHeight > hi.bestBlockHeight &&
        hi.bestBlockId != hi.bestHeaderId &&
        hiInit == hi
    }
  }

  "History consistency after unexpected shutdown" in {

    val result = node.waitForHeight(shutdownAtHeight, 100.millis)
      .flatMap(_ => waitForInconsistentHistory)
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
