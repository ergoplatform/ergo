package org.ergoplatform.nodeView.history.components

/**
  * Trait allowing to disable log messages in tests.
  */
trait VoidLogging extends Logging {

  def logInfo(msg: String): Unit = ()

  def logWarn(msg: String): Unit = ()

  def logError(msg: String): Unit = ()

  def logDebug(msg: String): Unit = ()

}
