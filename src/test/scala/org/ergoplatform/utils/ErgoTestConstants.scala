package org.ergoplatform.utils

import akka.util.Timeout
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.mining.{AutolykosPowScheme, DefaultFakePowScheme}
import org.ergoplatform.modifiers.history.ExtensionCandidate
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, StateConstants}
import org.ergoplatform.nodeView.wallet.ErgoProvingInterpreter
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.settings.{ErgoSettings, LaunchParameters, Parameters, VotingSettings}
import org.ergoplatform.{ErgoBox, ErgoScriptPredef}
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import sigmastate.SBoolean
import sigmastate.Values.Value
import sigmastate.basics.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.interpreter.CryptoConstants.EcPointType
import sigmastate.interpreter.{ContextExtension, ProverResult}

import scala.concurrent.duration._

trait ErgoTestConstants extends ScorexLogging {

  implicit val votingSettings: VotingSettings = VotingSettings(1024, 32, 128)

  val parameters: Parameters = LaunchParameters
  val timeProvider: NetworkTimeProvider = ErgoTestHelpers.defaultTimeProvider
  val initSettings: ErgoSettings = ErgoSettings.read(None)
  val settings: ErgoSettings = initSettings
  val emission: EmissionRules = settings.chainSettings.emissionRules
  val coinsTotal: Long = emission.coinsTotal
  val stateConstants: StateConstants = StateConstants(None, settings)
  val genesisStateDigest: ADDigest = settings.chainSettings.genesisStateDigest
  val feeProp: Value[SBoolean.type] = ErgoScriptPredef.feeProposition(emission.settings.minerRewardDelay)

  val emptyStateContext: ErgoStateContext = ErgoStateContext.empty(genesisStateDigest, votingSettings)
  val emptyProverResult: ProverResult = ProverResult(Array.emptyByteArray, ContextExtension.empty)
  val startHeight: Int = emptyStateContext.currentHeight
  val startDigest: ADDigest = emptyStateContext.genesisStateDigest
  val genesisBoxes: Seq[ErgoBox] = ErgoState.genesisBoxes(settings.chainSettings)
  val genesisEmissionBox: ErgoBox = ErgoState.genesisBoxes(settings.chainSettings).head
  val defaultSeed: String = ErgoSettings.read(None).walletSettings.seed
  val defaultProver: ErgoProvingInterpreter = ErgoProvingInterpreter(defaultSeed, 2, parameters)
  val defaultMinerSecret: DLogProverInput = defaultProver.secrets.head
  val defaultMinerSecretNumber: BigInt = defaultProver.secrets.head.w
  val defaultMinerPk: ProveDlog = defaultMinerSecret.publicImage
  val defaultMinerPkPoint: EcPointType = defaultMinerPk.h

  lazy val powScheme: AutolykosPowScheme = settings.chainSettings.powScheme.ensuring(_.isInstanceOf[DefaultFakePowScheme])
  val EmptyStateRoot: ADDigest = ADDigest @@ Array.fill(HashLength + 1)(0.toByte)
  val EmptyDigest32: Digest32 = Digest32 @@ Array.fill(HashLength)(0.toByte)
  val defaultDifficultyControl = new LinearDifficultyControl(1.minute, 8, 256)
  val defaultExtension: ExtensionCandidate = ExtensionCandidate(Seq(), Seq(Array(0: Byte, 8: Byte) -> EmptyDigest32))
  val emptyExtension: ExtensionCandidate = ExtensionCandidate(Seq(), Seq())

  val defaultTimeout: Timeout = Timeout(14.seconds)
  val defaultAwaitDuration: FiniteDuration = defaultTimeout.duration + 1.second

}
