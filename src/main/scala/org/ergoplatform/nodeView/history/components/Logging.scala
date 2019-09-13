package org.ergoplatform.nodeView.history.components

import scorex.util.ScorexLogging

trait Logging {

  protected def logInfo(msg: String): Unit

  protected def logWarn(msg: String): Unit

  protected def logError(msg: String): Unit

  protected def logDebug(msg: String): Unit

  object log {

    def info(msg: String): Unit = logInfo(msg)

    def warn(msg: String): Unit = logWarn(msg)

    def error(msg: String): Unit = logError(msg)

    def debug(msg: String): Unit = logDebug(msg)
  }

}

object Logging {

  trait Live extends Logging {

    private object LogProvider extends ScorexLogging {
      def logInfo(msg: String): Unit = log.info(msg)
      def logWarn(msg: String): Unit = log.warn(msg)
      def logError(msg: String): Unit = log.error(msg)
      def logDebug(msg: String): Unit = log.debug(msg)
    }

    protected final def logInfo(msg: String): Unit = LogProvider.logInfo(msg)

    protected final def logWarn(msg: String): Unit = LogProvider.logWarn(msg)

    protected final def logError(msg: String): Unit = LogProvider.logError(msg)

    protected final def logDebug(msg: String): Unit = LogProvider.logDebug(msg)
  }

}
