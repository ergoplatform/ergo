package org.ergoplatform.it

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import scorex.core.utils.ScorexLogging

import scala.concurrent.Await.result
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import org.ergoplatform.utils.ErgoTestHelpers.defaultExecutionContext

class SynchronizationTestSpec(nodes: Seq[Node]) extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers with ScorexLogging {

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

