package org.ergoplatform.it

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import scorex.core.utils.ScorexLogging

import scala.concurrent.Await.result
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class SyncronizationTestSpec(nodes: Seq[Node]) extends FreeSpec with ScalaFutures with IntegrationPatience
  with Matchers with LazyLogging {

  "Generate 30 blocks" in {
    val headerIdsAtSameHeight = result(for {
      b <- traverse(nodes)(_.height).map(_.min)
      _ <- traverse(nodes)(_.waitForHeight(b + 30))
      headers <- traverse(nodes)(_.headerIdsByHeight(b + 29))
    } yield {
      logger.debug(s"Headers at height ${b + 29}: ${headers.mkString(",")}")
      headers
    }, 10.minutes)

    val someHeaderId = headerIdsAtSameHeight.head.head
    require(headerIdsAtSameHeight.forall(_.contains(someHeaderId)))
  }
}

