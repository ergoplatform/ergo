package org.ergoplatform.settings

/**
  * Configuration file for Ergo chain
  * @see src/main/resources/application.conf for parameters description
  */
case class TestingSettings(transactionGeneration: Boolean,
                           keepPoolSize: Int,
                           transactionsInBlock: Int)
