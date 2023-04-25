package org.ergoplatform.utils

import scorex.util.ScorexLogging

object DefaultErgoLogger extends scorex.utils.Logger with ScorexLogging {
  override def error(message: String): Unit = log.error(message)
}
