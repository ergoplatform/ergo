package org.ergoplatform.utils

import org.ergoplatform.nodeView.history.ErgoHistory
import org.ergoplatform.nodeView.history.storage.modifierprocessors.{EmptyBlockSectionProcessor, FullBlockPruningProcessor, ToDownloadProcessor}
import org.ergoplatform.nodeView.state.StateType
import org.ergoplatform.settings._
import org.scalacheck.Gen
import scorex.core.settings.ScorexSettings

import scala.concurrent.duration._

trait HistoryTestHelpers extends ErgoPropertyTest {

  override lazy val smallInt: Gen[Int] = Gen.choose(0, BlocksInChain)

  val BlocksInChain = 10
  val BlocksToKeep: Int = BlocksInChain + 1

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

  // todo looks like copy-paste from Stubs.generateHistory
  def generateHistory(verifyTransactions: Boolean,
                      stateType: StateType,
                      PoPoWBootstrap: Boolean,
                      blocksToKeep: Int,
                      epochLength: Int = 100000000,
                      useLastEpochs: Int = 10): ErgoHistory = {

    val miningDelay = 1.second
    val minimalSuffix = 2
    val nodeSettings: NodeConfigurationSettings = NodeConfigurationSettings(stateType, verifyTransactions, blocksToKeep,
      PoPoWBootstrap, minimalSuffix, mining = false, 1323, miningDelay, useExternalMiner = false, miningPubKeyHex = None,
      offlineGeneration = false, 200, 100000, 100000, 1.minute, 1000000)
    val scorexSettings: ScorexSettings = null
    val testingSettings: TestingSettings = null
    val walletSettings: WalletSettings = null
    val chainSettings = settings.chainSettings.copy(epochLength = epochLength, useLastEpochs = useLastEpochs)

    val dir = createTempDir
    val fullHistorySettings: ErgoSettings = ErgoSettings(dir.getAbsolutePath, chainSettings, testingSettings,
      nodeSettings, scorexSettings, walletSettings, CacheSettings.default)

    ErgoHistory.readOrGenerate(fullHistorySettings, timeProvider)
  }

}

object HistoryTestHelpers {

  /**
    * Use reflection to set `minimalFullBlockHeightVar` to 0 to change regular synchronization rule, that we
    * first apply headers chain, and apply full blocks only after that
    */
  def allowToApplyOldBlocks(history: ErgoHistory): Unit = {
    import scala.reflect.runtime.{universe => ru}
    val runtimeMirror = ru.runtimeMirror(getClass.getClassLoader)
    val procInstance = runtimeMirror.reflect(history.asInstanceOf[ToDownloadProcessor])
    val ppM = ru.typeOf[ToDownloadProcessor].member(ru.TermName("pruningProcessor")).asMethod
    val pp = procInstance.reflectMethod(ppM).apply().asInstanceOf[FullBlockPruningProcessor]
    val f = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("minimalFullBlockHeightVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f).set(ErgoHistory.GenesisHeight)
    val f2 = ru.typeOf[FullBlockPruningProcessor].member(ru.TermName("isHeadersChainSyncedVar")).asTerm.accessed.asTerm
    runtimeMirror.reflect(pp).reflectField(f2).set(true)
  }

}
