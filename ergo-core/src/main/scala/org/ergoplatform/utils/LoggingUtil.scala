package org.ergoplatform.utils

object LoggingUtil {

  def getReasonMsg(t: Throwable): String = Option(t.getMessage)
    .map(m => s"${t.getClass.getName}: $m")
    .getOrElse(t.getClass.getName)

}
