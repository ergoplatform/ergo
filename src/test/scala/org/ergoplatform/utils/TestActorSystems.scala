package org.ergoplatform.utils

import akka.actor.ActorSystem
import akka.testkit.TestKit
import org.scalatest.{Outcome, TestSuite}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.util.Try

/**
  * Actor systems that end with their test. A test that creates its system with `testSystem()` gets it terminated once
  * the test ends, whether it passed or failed. A system that is not terminated keeps its actors (in the mining specs, a
  * candidate generator and a mining thread) running after its test ends.
  */
trait TestActorSystems extends TestSuite {

  private val testSystems = mutable.ArrayBuffer.empty[ActorSystem]

  protected def testSystem(): ActorSystem = {
    val system = ActorSystem()
    testSystems.synchronized(testSystems += system)
    system
  }

  abstract override protected def withFixture(test: NoArgTest): Outcome =
    try super.withFixture(test)
    finally {
      val systems = testSystems.synchronized {
        val all = testSystems.toList
        testSystems.clear()
        all
      }
      // every system is asked to stop before any is awaited; a stop over 30 s is reported (TestKit prints the system's
      // actor tree) and not thrown, so it neither replaces the test's own outcome nor keeps the other systems running
      systems.foreach(_.terminate())
      systems.foreach(system => Try(TestKit.shutdownActorSystem(system, 30.seconds)))
    }
}
