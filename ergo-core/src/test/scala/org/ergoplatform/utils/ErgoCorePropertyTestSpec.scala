package org.ergoplatform.utils

import org.scalacheck.Gen
import org.scalatest.events.{Event, TestFailed}
import org.scalatest.{Args, DoNotDiscover, Reporter}

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import scala.collection.mutable

class ErgoCorePropertyTestSpec extends ErgoCorePropertyTest {

  import ErgoCorePropertyTestSpec._

  // A forked test JVM sends this event to sbt, so it has to serialize.
  property("a failed property check over a value that cannot be serialized is reported in a serializable event") {
    val events = mutable.Buffer.empty[Event]
    new FailingCheck().run(None, Args(new Reporter {
      override def apply(event: Event): Unit = events += event
    }))
    val failed = events.collect { case e: TestFailed => e }
    failed should have size 1
    failed.head.message should include (Unsendable.Shown)
    failed.head.throwable shouldBe defined
    new ObjectOutputStream(new ByteArrayOutputStream).writeObject(failed.head)
  }

}

object ErgoCorePropertyTestSpec {

  final class Unsendable {
    override def toString: String = Unsendable.Shown
  }

  object Unsendable {
    val Shown = "unsendable value"
  }

  @DoNotDiscover
  class FailingCheck extends ErgoCorePropertyTest {
    property("fails") {
      forAll(Gen.const(new Unsendable)) { _ => false shouldBe true }
    }
  }

}
