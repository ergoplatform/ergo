package org.ergoplatform.utils

import akka.util.Timeout
import org.ergoplatform.network.{PeerSpec, Version}
import org.ergoplatform.nodeView.state._
import org.ergoplatform.settings.Parameters.{MaxBlockCostIncrease, MinValuePerByteIncrease}
import org.ergoplatform.settings._
import org.ergoplatform.wallet.interpreter.ErgoInterpreter
import org.ergoplatform.ErgoBox
import scorex.util.ScorexLogging

import scala.concurrent.duration._

object ErgoNodeTestConstants extends ScorexLogging {
  import org.ergoplatform.utils.ErgoCoreTestConstants._

  val extendedParameters: Parameters = {
    // Randomness in tests is causing occasional cost overflow in the state context and insufficient box value
    val extension = Map(
      MaxBlockCostIncrease -> Math.ceil(parameters.parametersTable(MaxBlockCostIncrease) * 1.3).toInt,
      MinValuePerByteIncrease -> (parameters.parametersTable(MinValuePerByteIncrease) - 30)
    )
    Parameters(0, Parameters.DefaultParameters ++ extension, ErgoValidationSettingsUpdate.empty)
  }

  val initSettings: ErgoSettings = ErgoSettingsReader.read(Args(Some("src/test/resources/application.conf"), None))

  implicit val settings: ErgoSettings = initSettings

  val lightModeSettings: ErgoSettings = initSettings.copy(
    nodeSettings = initSettings.nodeSettings.copy(stateType = StateType.Digest)
  )

  val genesisBoxes: Seq[ErgoBox] = ErgoState.genesisBoxes(settings.chainSettings)
  val genesisEmissionBox: ErgoBox = ErgoState.genesisBoxes(settings.chainSettings).head

  def emptyVerifier: ErgoInterpreter = ErgoInterpreter(emptyStateContext.currentParameters)

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
