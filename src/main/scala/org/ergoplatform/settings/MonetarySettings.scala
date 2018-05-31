package org.ergoplatform.settings

/**
  * Configuration file for monetary settings of Ergo chain
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class MonetarySettings(fixedRatePeriod: Long = 30 * 2 * 24 * 365,
                            epochLength: Int = 90 * 24 * 30,
                            fixedRate: Long = 7500000000L,
                            oneEpochReduction: Long = 300000000L)
