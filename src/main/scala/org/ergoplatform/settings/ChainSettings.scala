package org.ergoplatform.settings

import org.ergoplatform.modifiers.history.PoWScheme

import scala.concurrent.duration.FiniteDuration

/**
  * Configuration file for Ergo chain
  * @see src/main/resources/application.conf for parameters description
  */
case class ChainSettings(blockInterval: FiniteDuration,
                         epochLength: Int,
                         useLastEpochs: Int,
                         poWScheme: PoWScheme)
