package org.ergoplatform.settings

import sigmastate.utils.Helpers._
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.DifficultySerializer
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
                         eip37EpochLength: Option[Int],
                         useLastEpochs: Int,
                         voting: VotingSettings,
                         powScheme: AutolykosPowScheme,
                         monetary: MonetarySettings,
                         reemission: ReemissionSettings,
                         noPremineProof: Seq[String],
                         foundersPubkeys: Seq[String],
                         genesisStateDigestHex: String,
                         initialDifficultyHex: String,
                         makeSnapshotEvery: Int,
                         genesisId: Option[ModifierId] = None) {

  val isMainnet: Boolean = addressPrefix == ErgoAddressEncoder.MainnetNetworkPrefix

  val genesisStateDigest: ADDigest = Base16.decode(genesisStateDigestHex)
    .fold(_ => throw new Error(s"Failed to parse genesisStateDigestHex = $genesisStateDigestHex"), ADDigest @@ _)

  // The configured genesis id is compared as a string with header ids, which are 64 lower-case hex characters,
  // so any other value would make every genesis header fail the check.
  genesisId.foreach { id =>
    if (!id.matches("[0-9a-f]{64}")) {
      throw new Error(s"Malformed genesisId = '$id': expected a block id, 64 lower-case hex characters")
    }
  }

  val emissionRules: EmissionRules = new EmissionRules(monetary)

  val addressEncoder = new ErgoAddressEncoder(addressPrefix)

  val initialDifficulty: BigInt = Base16.decode(initialDifficultyHex)
    .fold(_ => throw new Error(s"Failed to parse initialDifficultyHex = $initialDifficultyHex"), BigInt(_))

  /**
    * Initial (genesis block) difficulty encoded as nbits
    */
  val initialNBits: Long = DifficultySerializer.encodeCompactBits(initialDifficulty)

  val initialDifficultyVersion2: BigInt = Base16.decode(voting.version2ActivationDifficultyHex)
    .fold(_ => throw new Error(s"Failed to parse initialDifficultyVersion2 = $initialDifficultyHex"), BigInt(_))

}
