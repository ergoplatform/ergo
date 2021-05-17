package org.ergoplatform.wallet.utils

import org.ergoplatform.settings.ErgoAlgos
import org.scalatest.propspec.AnyPropSpec
import scorex.util.ModifierId

import scala.annotation.tailrec

/** Implements various helper methods to use in specifications of [[org.ergoplatform.wallet]] package. */
trait WalletTestHelpers extends AnyPropSpec {

  /** Runs the given `fun` block, catches a [[Throwable]] and checks the given assertion is true.
    *
    * @throws org.scalatest.exceptions.TestFailedException if either exception is not thrown or the assertion
    *                                                      is false.
    */
  def assertExceptionThrown(fun: => Any, assertion: Throwable => Boolean): Unit = {
    try {
      fun
      fail("exception is expected")
    }
    catch {
      case e: Throwable =>
        if (!assertion(e))
          fail(s"exception check failed on $e (root cause: ${rootCause(e)}) \n trace:\n${e.getStackTrace.mkString("\n")}}")
    }
  }

  /** Find the root cause of the given exception by following the exception chain. */
  @tailrec
  final def rootCause(t: Throwable): Throwable =
    if (t.getCause == null) t
    else rootCause(t.getCause)

  /** Hashes the string and returns the hash as [[ModifierId]]. */
  def stringToId(s: String): ModifierId = {
    scorex.util.bytesToId(ErgoAlgos.hash(s))
  }

}
