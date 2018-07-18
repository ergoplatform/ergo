package org.ergoplatform.it

import com.typesafe.config.Config
import org.scalatest.FreeSpec

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class FourNodeSyncSpec extends FreeSpec with IntegrationSuite {

  val blocksCount = 5

  val forkDepth = blocksCount
  val minerConfig = Docker.nodeConfigs.head

  val nonGeneratingConfig = nonGeneratingPeerConfig.withFallback(Docker.nodeConfigs(1))
  val onlineGeneratingConfigs = Docker.nodeConfigs.slice(2, 2).map(onlineGeneratingPeerConfig.withFallback)
  val nodeConfigs = minerConfig +: nonGeneratingConfig +: onlineGeneratingConfigs

  val nodes: List[Node] = docker.startNodes(nodeConfigs).success.value

  s"Generate $blocksCount blocks" in {
    val result = for {
      b <- Future.traverse(nodes)(_.height).map(_.max)
      _ <- Future.traverse(nodes)(_.waitForHeight(b + blocksCount))
      headers <- Future.traverse(nodes)(_.headerIdsByHeight(b + blocksCount - forkDepth))
    } yield {
      log.debug(s"Headers at height ${b + blocksCount - forkDepth}: ${headers.mkString(",")}")
      val headerIdsAtSameHeight = headers.flatten
      val sample = headerIdsAtSameHeight.head
      headerIdsAtSameHeight should contain only sample
    }
    Await.result(result, 10.minutes)
  }

  def knownPeers(nodes: Map[String, Node], nodeConfig: Config) = {
    val node01 = nodes.map(_._2).find(_.settings.scorexSettings.network.nodeName == "node01")
    node01 match {
      case Some(n) if n != nodeConfig.getString("scorex.network.nodeName")=>
        s" -Dscorex.network.knownPeers.0=${n.nodeInfo.networkIpAddress}:${n.nodeInfo.containerNetworkPort}"
      case None => ""
    }
  }
}

