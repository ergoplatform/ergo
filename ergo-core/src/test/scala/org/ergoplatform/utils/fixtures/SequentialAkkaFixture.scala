package org.ergoplatform.utils.fixtures

import java.util.concurrent.atomic.AtomicInteger

import akka.actor.ActorSystem
import akka.testkit.{TestKit, ImplicitSender}
import org.scalatest.{Outcome, propspec}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object SequentialAkkaFixture {
  val sysId = new AtomicInteger()
}

trait SequentialAkkaFixture extends propspec.FixtureAnyPropSpec {
  import SequentialAkkaFixture._
  type Fixture <: TestKit
  type FixtureParam = Fixture

  class AkkaFixture extends TestKit(ActorSystem("WithIsoFix-%d".format(sysId.incrementAndGet()))) with ImplicitSender

  def createAkkaFixture(): Fixture

  override def withFixture(test: OneArgTest): Outcome = {
    val sys = createAkkaFixture()
    try {
      test(sys)
    } finally {
      Await.result(sys.system.terminate(), Duration.Inf)
    }
  }
}
