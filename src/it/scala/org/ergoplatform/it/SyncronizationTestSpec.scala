package org.ergoplatform.it

import org.scalatest.Matchers
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}

import scala.concurrent.Await.result
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class SyncronizationTestSpec(nodes: Seq[Node]) extends TestFourNodesSuite with ScalaFutures with IntegrationPatience
  with Matchers {

  "Generate 30 blocks" in {
    val headerIdsAtSameHeight = result(for {
      b <- traverse(nodes)(_.height).map(_.min)
      _ <- traverse(nodes)(_.waitForHeight(b + 30))
      headers <- traverse(nodes)(_.headerIdsByHeight(b + 29))
    } yield headers, 10.minutes)

    val someHeaderId = headerIdsAtSameHeight.head.head
    require(headerIdsAtSameHeight.forall(_.contains(someHeaderId)))
  }
}

