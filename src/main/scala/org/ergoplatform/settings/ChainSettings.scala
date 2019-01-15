package org.ergoplatform.settings

import scorex.util.ModifierId

import scala.concurrent.duration.FiniteDuration
import org.ergoplatform.mining.AutolykosPowScheme

/**
  * Configuration file for Ergo chain
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
                         genesisId: Option[ModifierId] = None)
