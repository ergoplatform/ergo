package org.ergoplatform.settings

import org.ergoplatform.nodeView.state.ErgoState
import scorex.crypto.authds.ADDigest
import scorex.util.encode.Base16
import sigmastate.{SBoolean, Values}

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
                            minerRewardDelay: Int = 720,
                            afterGenesisStateDigestHex: String) {

  val feeProposition: Values.Value[SBoolean.type] = ErgoState.feeProposition(minerRewardDelay)
  val feePropositionBytes: Array[Byte] = feeProposition.bytes

  val afterGenesisStateDigest: ADDigest = Base16.decode(afterGenesisStateDigestHex) match {
    case Success(b) => ADDigest @@ b
    case _ => throw new Error(s"Failed to parse afterGenesisStateDigestHex = $afterGenesisStateDigestHex")
  }

}
