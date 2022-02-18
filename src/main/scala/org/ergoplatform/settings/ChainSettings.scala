package org.ergoplatform.settings

import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.difficulty.RequiredDifficulty
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.reemission.ReemissionRules
import org.ergoplatform.wallet.boxes.ErgoBoxSerializer
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
                         reemission: ReemissionSettings,
                         noPremineProof: Seq[String],
                         foundersPubkeys: Seq[String],
                         genesisStateDigestHex: String,
                         initialDifficultyHex: String,
                         genesisId: Option[ModifierId] = None) {

  val genesisStateDigest: ADDigest = Base16.decode(genesisStateDigestHex)
    .fold(_ => throw new Error(s"Failed to parse genesisStateDigestHex = $genesisStateDigestHex"), ADDigest @@ _)

  val emissionRules: EmissionRules = new EmissionRules(monetary)

  val addressEncoder = new ErgoAddressEncoder(addressPrefix)

  val initialDifficulty: BigInt = Base16.decode(initialDifficultyHex)
    .fold(_ => throw new Error(s"Failed to parse initialDifficultyHex = $initialDifficultyHex"), BigInt(_))

  val initialNBits: Long = RequiredDifficulty.encodeCompactBits(initialDifficulty)

  val initialDifficultyVersion2: BigInt = Base16.decode(voting.version2ActivationDifficultyHex)
    .fold(_ => throw new Error(s"Failed to parse initialDifficultyVersion2 = $initialDifficultyHex"), BigInt(_))

}


case class ReemissionSettings(checkReemissionRules: Boolean,
                              emissionNftId: ModifierId,
                              reemissionTokenId: ModifierId,
                              reemissionNftId: ModifierId,
                              activationHeight: Int,
                              reemissionStartHeight: Int,
                              injectionBoxBytesEncoded: ModifierId) {

  val reemissionRules = new ReemissionRules(this)

  val emissionNftIdBytes: Array[Byte] = Algos.decode(emissionNftId).get
  val reemissionNftIdBytes: Array[Byte] = Algos.decode(reemissionNftId).get
  val reemissionTokenIdBytes: Array[Byte] = Algos.decode(reemissionTokenId).get

  // todo: move box id to settings
  lazy val InjectionBoxBytes: Array[Byte] = Base16.decode(injectionBoxBytesEncoded).get
  // Base16.decode("8094ebdc03100204000580c0dfda8ee906d191c1b2a4730000730181030202aeaee7a0286770b5a5f4ee75e92ad412b6a58ce90ed3b7d45f6d6940520f340106ad86473b1d3ef06a4fc38e6b79b042c4930f017469e4f4957589a17cc60179808094f6c2d7e85800690e17713c44ade202860b7023f350739f943bf2cf77de15b7f359c1dcfc373f00").get

  lazy val injectionBox: ErgoBox = ErgoBoxSerializer.parseBytes(InjectionBoxBytes)
}
