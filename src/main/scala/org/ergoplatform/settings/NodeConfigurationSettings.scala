package org.ergoplatform.settings

import scala.concurrent.duration.FiniteDuration

/**
  * Configuration file for Ergo node regime
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class NodeConfigurationSettings(ADState: Boolean,
                                     verifyTransactions: Boolean,
                                     blocksToKeep: Int,
                                     PoPoWBootstrap: Boolean,
                                     minimalSuffix: Int,
                                     mining: Boolean,
                                     miningDelay: FiniteDuration,
                                     offlineGeneration: Boolean)
