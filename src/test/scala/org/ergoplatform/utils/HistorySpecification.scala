package org.ergoplatform.utils

import org.ergoplatform.mining.DefaultFakePowScheme
import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.modifierprocessors.EmptyBlockSectionProcessor
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings._
import org.scalacheck.Gen
import scorex.core.settings.ScorexSettings

import scala.concurrent.duration._

trait HistorySpecification extends ErgoPropertyTest {

  override lazy val smallInt: Gen[Int] = Gen.choose(0, BlocksInChain)

  val BlocksInChain = 10
  val BlocksToKeep = BlocksInChain + 1

  def ensureMinimalHeight(history: ErgoHistory, height: Int = BlocksInChain): ErgoHistory = {
    val historyHeight = history.headersHeight
    if (historyHeight < height) {
      history match {
        case _: EmptyBlockSectionProcessor =>
          val chain = genHeaderChain(height - historyHeight, history)
          if (history.isEmpty) applyHeaderChain(history, chain) else applyHeaderChain(history, chain.tail)
        case _ =>
          ???
      }
    } else {
      history
    }
  }

  def generateHistory(verifyTransactions: Boolean,
                      stateType: StateType,
                      PoPoWBootstrap: Boolean,
                      blocksToKeep: Int,
                      epochLength: Int = 100000000,
                      useLastEpochs: Int = 10): ErgoHistory = {

    val networkPrefix = 0: Byte
    val blockInterval = 1.minute
    val miningDelay = 1.second
    val minimalSuffix = 2
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(stateType, verifyTransactions, blocksToKeep,
      PoPoWBootstrap, minimalSuffix, mining = false, miningDelay, offlineGeneration = false, 200)
    val scorexSettings: ScorexSettings = null
    val testingSettings: TestingSettings = null
    val walletSettings: WalletSettings = null
    val chainSettings = ChainSettings(networkPrefix, blockInterval, epochLength, useLastEpochs, DefaultFakePowScheme,
      settings.chainSettings.monetary)

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, chainSettings, testingSettings,
      nodeSettings, scorexSettings, walletSettings, CacheSettings.default)

    ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  }
}
