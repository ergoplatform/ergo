package org.ergoplatform.settings

import org.ergoplatform.mining.AutolykosPowScheme
import org.ergoplatform.mining.emission.EmissionRules
import scorex.crypto.authds.ADDigest
import scorex.util.ModifierId
import scorex.util.encode.Base16

import scala.concurrent.duration.FiniteDuration
import scala.util.Success

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
                         genesisId: Option[ModifierId] = None) {

  val genesisStateDigest: ADDigest = Base16.decode(genesisStateDigestHex) match {
    case Success(b) => ADDigest @@ b
    case _ => throw new Error(s"Failed to parse genesisStateDigestHex = $genesisStateDigestHex")
  }

  val emissionRules: EmissionRules = new EmissionRules(monetary)

}
