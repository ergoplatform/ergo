package org.ergoplatform.utils

import org.scalatest.{Failed, Outcome}
import org.scalatest.exceptions.{PropertyCheckFailedException, StackDepthException, TestFailedException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import scala.util.Try

trait ErgoCorePropertyTest extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with NoShrink {

  // The forked test JVM cannot always send a property check failure to sbt (it may not serialize), and a few such
  // failures can end the run with no summary. Report it as a plain test failure with the same message, position and
  // stack trace, and the cause if it can be sent.
  override def withFixture(test: NoArgTest): Outcome = super.withFixture(test) match {
    case Failed(e: PropertyCheckFailedException) =>
      val message = e.message
      val cause = e.cause.filter(isSerializable)
      val failure = e.position match {
        case Some(pos) => new TestFailedException((_: StackDepthException) => message, cause, pos)
        case None => new TestFailedException(message, cause, e.failedCodeStackDepth)
      }
      failure.setStackTrace(e.getStackTrace)
      Failed(failure)
    case outcome => outcome
  }

  private def isSerializable(t: Throwable): Boolean =
    Try(new ObjectOutputStream(new ByteArrayOutputStream).writeObject(t)).isSuccess

}
