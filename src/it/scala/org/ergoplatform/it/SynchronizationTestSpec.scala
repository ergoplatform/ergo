package org.ergoplatform.it

import org.ergoplatform.it.util.IntegrationTest
import org.scalatest.FreeSpec
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.Await.result
import scala.concurrent.Future.traverse
import scala.concurrent.duration._

class SynchronizationTestSpec(nodes: Seq[Node]) extends FreeSpec with IntegrationTest {

  val blocksCount = 5
  val forkDepth = blocksCount

  s"Generate $blocksCount blocks" in {
    val headerIdsAtSameHeight = result(for {
      b <- traverse(nodes)(_.height).map(_.max)
      _ <- traverse(nodes)(_.waitForHeight(b + blocksCount))
      headers <- traverse(nodes)(_.headerIdsByHeight(b + blocksCount - forkDepth))
    } yield {
      log.debug(s"Headers at height ${b + blocksCount - forkDepth}: ${headers.mkString(",")}")
      headers.flatten
    }, 10.minutes)

    val sample = headerIdsAtSameHeight.head
    headerIdsAtSameHeight should contain only sample
  }
}

