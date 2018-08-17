package org.ergoplatform.settings

/**
  * Configuration file for Ergo chain
  * @see src/main/resources/application.conf for parameters description
  */
case class TestingSettings(transactionGeneration: Boolean, maxTransactionsPerBlock: Int) {
  require(maxTransactionsPerBlock > 0, "Non-positive number of transactions to generate per block")
}
