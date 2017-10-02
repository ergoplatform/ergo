package org.ergoplatform.settings

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
                                     offlineGeneration: Boolean = false)
