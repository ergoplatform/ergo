package org.ergoplatform.utils

import akka.util.Timeout
import org.ergoplatform.mining.difficulty.LinearDifficultyControl
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.mining.{AutolykosPowScheme, DefaultFakePowScheme}
import org.ergoplatform.modifiers.history.ExtensionCandidate
import org.ergoplatform.nodeView.state.{ErgoState, ErgoStateContext, StateConstants}
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.settings._
import org.ergoplatform.wallet.interpreter.{ErgoInterpreter, ErgoProvingInterpreter}
import org.ergoplatform.{DataInput, ErgoBox, ErgoScriptPredef}
import scorex.core.app.Version
import scorex.core.network.PeerSpec
import scorex.core.utils.NetworkTimeProvider
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import sigmastate.Values.ErgoTree
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
  val feeProp: ErgoTree = ErgoScriptPredef.feeProposition(emission.settings.minerRewardDelay)

  val emptyProverResult: ProverResult = ProverResult(Array.emptyByteArray, ContextExtension.empty)
  val genesisBoxes: Seq[ErgoBox] = ErgoState.genesisBoxes(settings.chainSettings)
  val genesisEmissionBox: ErgoBox = ErgoState.genesisBoxes(settings.chainSettings).head
  val defaultProver: ErgoProvingInterpreter = ???
  val defaultMinerSecret: DLogProverInput = defaultProver.secrets.head
  val defaultMinerSecretNumber: BigInt = defaultProver.secrets.head.w
  val defaultMinerPk: ProveDlog = defaultMinerSecret.publicImage
  val defaultMinerPkPoint: EcPointType = defaultMinerPk.h

  val defaultTimestamp: Long = 1552217190000L
  val defaultnBits: Long = settings.chainSettings.initialNBits
  val defaultVotes: Array[Byte] = Array.fill(3)(0.toByte)
  val defaultVersion: Byte = 0
  lazy val powScheme: AutolykosPowScheme = settings.chainSettings.powScheme.ensuring(_.isInstanceOf[DefaultFakePowScheme])
  val emptyStateContext: ErgoStateContext = ErgoStateContext.empty(genesisStateDigest, settings)
    .upcoming(defaultMinerPkPoint, defaultTimestamp, defaultnBits, defaultVotes, defaultVersion)

  val startHeight: Int = emptyStateContext.currentHeight
  val startDigest: ADDigest = emptyStateContext.genesisStateDigest

  val EmptyStateRoot: ADDigest = ADDigest @@ Array.fill(HashLength + 1)(0.toByte)
  val EmptyDigest32: Digest32 = Digest32 @@ Array.fill(HashLength)(0.toByte)
  val defaultDifficultyControl = new LinearDifficultyControl(settings.chainSettings)
  val defaultExtension: ExtensionCandidate = ExtensionCandidate(Seq(Array(0: Byte, 8: Byte) -> EmptyDigest32))
  val emptyExtension: ExtensionCandidate = ExtensionCandidate(Seq())
  val emptyDataInputs: IndexedSeq[DataInput] = IndexedSeq()
  val emptyDataBoxes: IndexedSeq[ErgoBox] = IndexedSeq()
  lazy val emptyVerifier: ErgoInterpreter = ErgoInterpreter(emptyStateContext.currentParameters)

  val defaultTimeout: Timeout = Timeout(14.seconds)
  val defaultAwaitDuration: FiniteDuration = defaultTimeout.duration + 1.second

  val defaultPeerSpec = PeerSpec(
    settings.scorexSettings.network.agentName,
    Version(settings.scorexSettings.network.appVersion),
    settings.scorexSettings.network.nodeName,
    None,
    Seq.empty
  )

}
