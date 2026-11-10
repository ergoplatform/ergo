package org.ergoplatform.utils

import java.util.concurrent.atomic.AtomicReference

import akka.actor.ActorSystem
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Args, Reporter}

class TestActorSystemsSpec extends AnyFlatSpec with Matchers {

  private val silent: Reporter = (_: org.scalatest.events.Event) => ()

  /** Runs a one-test suite whose test creates a system with `testSystem()`, and returns that system. */
  private def systemOfInnerTest(failing: Boolean): ActorSystem = {
    val created = new AtomicReference[ActorSystem]()
    val inner = new AnyFlatSpec with TestActorSystems {
      "an inner test" should "create a system" in {
        created.set(testSystem())
        if (failing) fail("failing on purpose")
      }
    }
    inner.run(None, Args(silent))
    created.get
  }

  "TestActorSystems" should "terminate a test's system when the test passes" in {
    systemOfInnerTest(failing = false).whenTerminated.isCompleted shouldBe true
  }

  it should "terminate a test's system when the test fails" in {
    systemOfInnerTest(failing = true).whenTerminated.isCompleted shouldBe true
  }
}
