package org.ergoplatform.settings

import scala.concurrent.duration.FiniteDuration

/**
  * Configuration file for Ergo chain
  * @see src/main/resources/application.conf for parameters description
  */
case class ChainSettings(blockInterval: FiniteDuration, epochLength: Int)
