package org.ergoplatform.wallet.utils

import org.scalatest.PropSpec

import scala.annotation.tailrec

trait WalletTestHelpers extends PropSpec {

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

  @tailrec
  final def rootCause(t: Throwable): Throwable =
    if (t.getCause == null) t
    else rootCause(t.getCause)

}
