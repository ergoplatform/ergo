package org.ergoplatform.settings

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.emission.EmissionRules
import scorex.crypto.authds.ADDigest
import scorex.util.ModifierId
import scorex.util.encode.Base16

import scala.concurrent.duration.FiniteDuration

/**
  * Configuration file for Ergo chain
  *
  * @see src/main/resources/application.conf for parameters description
  */
case class ChainSettings(protocolVersion: Byte,
                         addressPrefix: Byte,
                         blockInterval: FiniteDuration,
                         epochLength: Int,
                         useLastEpochs: Int,
                         voting: VotingSettings,
                         powScheme: AutolykosPowScheme,
                         monetary: MonetarySettings,
                         noPremineProof: Seq[String],
                         foundersPubkeys: Seq[String],
                         genesisStateDigestHex: String,
                         initialDifficultyHex: String,
                         genesisId: Option[ModifierId] = None) {

  val genesisStateDigest: ADDigest = Base16.decode(genesisStateDigestHex)
    .fold(_ => throw new Error(s"Failed to parse genesisStateDigestHex = $genesisStateDigestHex"), ADDigest @@ _)

  val emissionRules: EmissionRules = new EmissionRules(monetary)

  val initialDifficulty: BigInt = Base16.decode(initialDifficultyHex)
    .fold(_ => throw new Error(s"Failed to parse initialDifficultyHex = $initialDifficultyHex"), BigInt(_))

  val initialNBits: Long = RequiredDifficulty.encodeCompactBits(initialDifficulty)

  val initialDifficultyVersion2: BigInt = Base16.decode(voting.version2ActivationDifficultyHex)
    .fold(_ => throw new Error(s"Failed to parse initialDifficultyHex = $initialDifficultyHex"), BigInt(_))

}
