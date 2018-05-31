package org.ergoplatform.settings

import scorex.crypto.authds.ADDigest
import scorex.crypto.encode.Base16

import scala.util.Success

/**
  * Configuration file for monetary settings of Ergo chain
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class MonetarySettings(fixedRatePeriod: Long = 30 * 2 * 24 * 365,
                            epochLength: Int = 90 * 24 * 30,
                            fixedRate: Long = 7500000000L,
                            oneEpochReduction: Long = 300000000L,
                            afterGenesisStateDigestHex: String) {

  val afterGenesisStateDigest: ADDigest = Base16.decode(afterGenesisStateDigestHex) match {
    case Success(b) => ADDigest @@ b
    case _ => throw new Error(s"Failed to parse afterGenesisStateDigestHex = $afterGenesisStateDigestHex")
  }

}
