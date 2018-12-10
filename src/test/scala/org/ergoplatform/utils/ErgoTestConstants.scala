package org.ergoplatform.utils

import akka.util.Timeout
import org.ergoplatform.ErgoBox
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.mining.{DefaultFakePowScheme, PowScheme}
import org.ergoplatform.modifiers.history.ExtensionCandidate
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, StateConstants}
import org.ergoplatform.nodeView.wallet.ErgoProvingInterpreter
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.settings.{ErgoSettings, LaunchParameters, Parameters, VotingSettings}
import scapi.sigma.DLogProtocol.{DLogProverInput, ProveDlog}
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging

import scala.concurrent.duration._

trait ErgoTestConstants extends ScorexLogging {

  val parameters: Parameters = LaunchParameters

  val timeProvider: NetworkTimeProvider = ErgoTestHelpers.defaultTimeProvider
  val initSettings: ErgoSettings = ErgoSettings.read(None)
  val settings: ErgoSettings = initSettings
  val coinsTotal: Long = settings.emission.coinsTotal
  val stateConstants: StateConstants = StateConstants(None, settings)
  val afterGenesisDigest: ADDigest = settings.chainSettings.monetary.afterGenesisStateDigest


  implicit val votingSettings = VotingSettings(1024, 32, 128)
  
  val emptyStateContext: ErgoStateContext = ErgoStateContext.empty(afterGenesisDigest, votingSettings)
  val startHeight: Int = emptyStateContext.currentHeight
  val startDigest: ADDigest = emptyStateContext.genesisStateDigest
  val genesisEmissionBox: ErgoBox = ErgoState.genesisEmissionBox(settings.emission)
  val defaultSeed: String = ErgoSettings.read(None).walletSettings.seed
  val defaultProver: ErgoProvingInterpreter = new ErgoProvingInterpreter(defaultSeed, 1, parameters)
  val defaultMinerSecret: DLogProverInput = defaultProver.secrets.head
  val defaultMinerSecretNumber: BigInt = defaultProver.secrets.head.w
  val defaultMinerPk: ProveDlog = defaultMinerSecret.publicImage

  val powScheme: PowScheme = DefaultFakePowScheme
  val EmptyStateRoot: ADDigest = ADDigest @@ Array.fill(HashLength + 1)(0.toByte)
  val EmptyDigest32: Digest32 = Digest32 @@ Array.fill(HashLength)(0.toByte)
  val defaultDifficultyControl = new LinearDifficultyControl(1.minute, 8, 256)
  val defaultExtension: ExtensionCandidate = ExtensionCandidate(Seq(), Seq((EmptyDigest32, EmptyDigest32)))
  val emptyExtension: ExtensionCandidate = ExtensionCandidate(Seq(), Seq())

  val defaultTimeout: Timeout = Timeout(14.seconds)
  val defaultAwaitDuration: FiniteDuration = defaultTimeout.duration + 1.second

}
