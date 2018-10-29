package org.ergoplatform.it

import java.io.File

import com.typesafe.config.Config
import org.ergoplatform.it.container.{IntegrationSuite, Node}
import org.scalatest.FreeSpec

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class ForkResolutionSpec extends FreeSpec with IntegrationSuite {

  val initialCommonChainLength: Int = 4
  val forkLength: Int = 5
  val syncLength: Int = 5

  def localVolume(n: Int): String = s"$localDataDir/fork-resolution-spec/node$n/data"

  val localVolumes: Seq[String] = (0 to 3).map(localVolume)
  val remoteVolume = "/app"

  val volumesMapping: Seq[(String, String)] = localVolumes.map(_ -> remoteVolume)

  val dirs: Seq[File] = localVolumes.map(vol => new File(vol))
  dirs.foreach(_.mkdirs())

  val minerConfig: Config = nodeSeedConfigs.head
  val onlineMiningNodesConfig: List[Config] = nodeSeedConfigs.slice(1, 4).map(_.withFallback(onlineGeneratingPeerConfig))
  val offlineMiningNodesConfig: List[Config] = nodeSeedConfigs.slice(1, 4)

  "Fork resolution after isolated mining" in {

    val nodes: List[Node] = docker.startNodes(minerConfig +: onlineMiningNodesConfig, specialVolumes = volumesMapping).get

    val result = for {
      b <- Future.traverse(nodes)(_.height).map(_.max)
      _ <- Future.traverse(nodes)(_.waitForHeight(b + initialCommonChainLength))
      isolatedNodes <- {
        nodes.foreach(node => docker.stopNode(node.containerId))
        Future.successful(docker.startNodes(minerConfig +: offlineMiningNodesConfig, isolatedPeersConfig, volumesMapping).get)
      }
      _ <- Future.traverse(isolatedNodes)(_.waitForHeight(b + initialCommonChainLength + forkLength))
      regularNodes <- {
        isolatedNodes.foreach(node => docker.stopNode(node.containerId))
        Future.successful(docker.startNodes(minerConfig +: offlineMiningNodesConfig, specialVolumes = volumesMapping).get)
      }
      _ <- Future.traverse(regularNodes)(_.waitForHeight(b + initialCommonChainLength + forkLength + syncLength))
      headers <- Future.traverse(regularNodes)(_.headerIdsByHeight(b + initialCommonChainLength + forkLength))
    } yield {
      log.debug(s"Headers at height $b: ${headers.mkString(",")}")
      val headerIdsAtSameHeight = headers.flatten
      val sample = headerIdsAtSameHeight.head
      headerIdsAtSameHeight should contain only sample
    }
    Await.result(result, 4.minutes)
  }
}
