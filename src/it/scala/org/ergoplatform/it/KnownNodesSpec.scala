package org.ergoplatform.it

import org.ergoplatform.it.util.IntegrationTest
import org.scalatest.FreeSpec

import scala.concurrent.Await
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class KnownNodesSpec(nodes: Seq[Node]) extends FreeSpec with IntegrationTest {

  val blocksCount = 5
  val forkDepth = blocksCount

  s"The third node knows first node" in {

    val node01 = nodes.find(_.settings.scorexSettings.network.nodeName == "node01")
    val node03 = nodes.find(_.settings.scorexSettings.network.nodeName == "node03")

    val result = node03.map(_.waitForPeers(2).map { peers =>
      println(peers)
      peers.map(_.name) should contain ("node01")
    }).get

    Await.result(result, 10.minutes)
  }

//  s"Generate $blocksCount blocks" in {
//    val result = for {
//      b <- traverse(nodes)(_.height).map(_.max)
//      _ <- traverse(nodes)(_.waitForHeight(b + blocksCount))
//      headers <- traverse(nodes)(_.headerIdsByHeight(b + blocksCount - forkDepth))
//    } yield {
//      log.debug(s"Headers at height ${b + blocksCount - forkDepth}: ${headers.mkString(",")}")
//      val headerIdsAtSameHeight = headers.flatten
//      val sample = headerIdsAtSameHeight.head
//      headerIdsAtSameHeight should contain only sample
//    }
//    Await.result(result, 10.minutes)
//  }
}

