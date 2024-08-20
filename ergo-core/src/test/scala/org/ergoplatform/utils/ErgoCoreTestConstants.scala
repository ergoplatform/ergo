package org.ergoplatform.utils

import com.typesafe.config.ConfigFactory
import org.ergoplatform.mining.difficulty.DifficultyAdjustment
import org.ergoplatform.mining.emission.EmissionRules
import org.ergoplatform.mining.{AutolykosPowScheme, DefaultFakePowScheme}
import org.ergoplatform.modifiers.history.extension.ExtensionCandidate
import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.sdk.wallet.secrets.ExtendedSecretKey
import org.ergoplatform.settings.Constants.HashLength
import org.ergoplatform.settings.ValidationRules._
import org.ergoplatform.settings._
import org.ergoplatform.wallet.interface4j.SecretString
import org.ergoplatform.wallet.interpreter.ErgoProvingInterpreter
import org.ergoplatform.wallet.mnemonic.Mnemonic
import org.ergoplatform.{DataInput, ErgoBox, ErgoTreePredef}
import scorex.crypto.authds.ADDigest
import scorex.crypto.hash.Digest32
import scorex.util.ScorexLogging
import sigmastate.Values.ErgoTree
import sigmastate.crypto.CryptoConstants.EcPointType
import sigmastate.crypto.DLogProtocol.{DLogProverInput, ProveDlog}
import sigmastate.interpreter.{ContextExtension, ProverResult}
import net.ceedubs.ficus.Ficus._
import org.ergoplatform.nodeView.state.{ErgoStateContext, UpcomingStateContext}

import java.io.File

object ErgoCoreTestConstants extends ScorexLogging {
  val configPath = "src/test/resources/application.conf"
  val walletTestMnemonic: Option[String] =
    Some(ConfigFactory.parseFile(new File(configPath)).as[String]("ergo.wallet.testMnemonic"))
  val walletTestKeysQty: Option[Int] =
    Some(ConfigFactory.parseFile(new File(configPath)).as[Int]("ergo.wallet.testKeysQty"))

  val chainSettings: ChainSettings = ChainSettingsReader.read(configPath).get

  implicit val votingSettings: VotingSettings = VotingSettings(1024, 32, 128, 32 * 1024, "01")
  val validationSettings: ErgoValidationSettings = ErgoValidationSettings.initial
  implicit val validationSettingsNoIl: ErgoValidationSettings = validationSettings
    .updated(ErgoValidationSettingsUpdate(Seq(exIlUnableToValidate, exIlEncoding, exIlStructure, exEmpty), Seq()))

  val parameters: Parameters = MainnetLaunchParameters
  val nipopowAlgos = new NipopowAlgos(chainSettings)

  val emission: EmissionRules = chainSettings.emissionRules
  val coinsTotal: Long = emission.coinsTotal
  val genesisStateDigest: ADDigest = chainSettings.genesisStateDigest
  val feeProp: ErgoTree = ErgoTreePredef.feeProposition(emission.settings.minerRewardDelay)

  val emptyProverResult: ProverResult = ProverResult(Array.emptyByteArray, ContextExtension.empty)
  val defaultTimestamp: Long = 1552217190000L
  val defaultNBits: Long = chainSettings.initialNBits
  val defaultVotes: Array[Byte] = Array.fill(3)(0.toByte)
  val defaultVersion: Byte = 0
  lazy val powScheme: AutolykosPowScheme = chainSettings.powScheme.ensuring(_.isInstanceOf[DefaultFakePowScheme])
  val emptyVSUpdate = ErgoValidationSettingsUpdate.empty

  val EmptyStateRoot: ADDigest = ADDigest @@ Array.fill(HashLength + 1)(0.toByte)
  val EmptyDigest32: Digest32 = Digest32 @@ Array.fill(HashLength)(0.toByte)
  val defaultDifficultyControl = new DifficultyAdjustment(chainSettings)
  val defaultExtension: ExtensionCandidate = ExtensionCandidate(Seq(Array(0: Byte, 8: Byte) -> EmptyDigest32))
  val emptyExtension: ExtensionCandidate = ExtensionCandidate(Seq())
  val emptyDataInputs: IndexedSeq[DataInput] = IndexedSeq()
  val emptyDataBoxes: IndexedSeq[ErgoBox] = IndexedSeq()

  lazy val defaultSeed: Array[Byte] = Mnemonic.toSeed(walletTestMnemonic.fold[SecretString](SecretString.empty())(SecretString.create(_)))
  val defaultRootSecret: ExtendedSecretKey = ExtendedSecretKey.deriveMasterKey(defaultSeed, usePre1627KeyDerivation = false)
  val defaultChildSecrets: IndexedSeq[ExtendedSecretKey] = walletTestKeysQty
    .toIndexedSeq
    .flatMap(x => (0 until x).map(defaultRootSecret.child))
  val defaultProver: ErgoProvingInterpreter = ErgoProvingInterpreter(
    defaultRootSecret +: defaultChildSecrets, parameters)
  val defaultMinerSecret: DLogProverInput = defaultProver.hdKeys.head.privateInput
  val defaultMinerSecretNumber: BigInt = defaultProver.hdKeys.head.privateInput.w
  val defaultMinerPk: ProveDlog = defaultMinerSecret.publicImage
  val defaultMinerPkPoint: EcPointType = defaultMinerPk.value

  val emptyStateContext: UpcomingStateContext = ErgoStateContext.empty(genesisStateDigest, chainSettings, parameters)
    .upcoming(defaultMinerPkPoint, defaultTimestamp, defaultNBits, defaultVotes, emptyVSUpdate, defaultVersion)
  def stateContextWith(parameters: Parameters): UpcomingStateContext = ErgoStateContext.empty(genesisStateDigest, chainSettings, parameters)
    .upcoming(defaultMinerPkPoint, defaultTimestamp, defaultNBits, defaultVotes, emptyVSUpdate, defaultVersion)
  val startHeight: Int = emptyStateContext.currentHeight
  val startDigest: ADDigest = emptyStateContext.genesisStateDigest

}
