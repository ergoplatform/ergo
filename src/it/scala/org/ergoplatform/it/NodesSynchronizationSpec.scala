package org.ergoplatform.it

import org.ergoplatform.it.util.IntegrationTest
import org.scalatest.FreeSpec

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future.traverse

class NodesSynchronizationSpec(nodes: Seq[Node]) extends FreeSpec with IntegrationTest {

  val blocksCount = 5
  val forkDepth = blocksCount

  s"Generate $blocksCount blocks" in {
    val result = for {
      b <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(b + blocksCount))
      headers <- traverse(nodes)(_.headerIdsByHeight(b + blocksCount - forkDepth))
    } yield {
      log.debug(s"Headers at height ${b + blocksCount - forkDepth}: ${headers.mkString(",")}")
      val headerIdsAtSameHeight = headers.flatten
      val sample = headerIdsAtSameHeight.head
      headerIdsAtSameHeight should contain only sample
    }
    Await.result(result, 10.minutes)
  }
}

